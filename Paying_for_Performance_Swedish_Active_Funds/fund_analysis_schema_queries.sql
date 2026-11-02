-- ==================================================================
-- "Paying for Performance: Are Swedish Active Funds Worth the Extra
--  Cost?" -- Database schema and extraction queries
--  Fabian Lindh & William Jarkander, Uppsala University, 2025
--
--  Purpose: organize raw data pulled from Morningstar (fund
--  selection), Refinitiv Eikon (monthly NAV / returns) and the
--  Swedish House of Finance (SMB/HML factors) into a small
--  relational schema, and expose the monthly excess-return panel
--  that the R and Python scripts consume.
-- ==================================================================

-- ------------------------------------------------------------------
-- 1. SCHEMA
-- ------------------------------------------------------------------

DROP TABLE IF EXISTS funds CASCADE;
CREATE TABLE funds (
    fund_id         SERIAL PRIMARY KEY,
    fund_name       VARCHAR(120) NOT NULL,
    fund_type       VARCHAR(10)  NOT NULL CHECK (fund_type IN ('active', 'passive')),
    morningstar_cat VARCHAR(50),      -- e.g. 'Sweden'
    country         VARCHAR(50),      -- registered country, e.g. 'Sweden'
    annual_fee_pct  NUMERIC(5,3) NOT NULL
);

DROP TABLE IF EXISTS fund_nav CASCADE;
CREATE TABLE fund_nav (
    fund_id     INT REFERENCES funds(fund_id),
    obs_date    DATE NOT NULL,
    nav_gross   NUMERIC(14,4),   -- NAV index, before fees
    nav_net     NUMERIC(14,4),   -- NAV index, after fees
    PRIMARY KEY (fund_id, obs_date)
);

DROP TABLE IF EXISTS market_index CASCADE;
CREATE TABLE market_index (
    obs_date    DATE PRIMARY KEY,
    omxs_level  NUMERIC(14,4)     -- OMXSPI level
);

DROP TABLE IF EXISTS risk_free CASCADE;
CREATE TABLE risk_free (
    obs_date    DATE PRIMARY KEY,
    rf_rate_m   NUMERIC(10,8)     -- monthly risk-free rate (Swedish 3M T-bill)
);

DROP TABLE IF EXISTS ff_factors CASCADE;
CREATE TABLE ff_factors (
    obs_date    DATE PRIMARY KEY,
    smb         NUMERIC(10,8),    -- Small Minus Big
    hml         NUMERIC(10,8)     -- High Minus Low
    -- NB: only populated through 2019-12 per Swedish House of Finance data
);

-- ------------------------------------------------------------------
-- 2. FUND UNIVERSE SELECTION
--    Morningstar category = Sweden, registered country = Sweden,
--    full 10-year history available (2015-04 to 2025-03)
-- ------------------------------------------------------------------

CREATE OR REPLACE VIEW v_fund_universe AS
SELECT f.fund_id, f.fund_name, f.fund_type, f.annual_fee_pct
FROM funds f
WHERE f.morningstar_cat = 'Sweden'
  AND f.country = 'Sweden'
  AND f.fund_id IN (
        SELECT fund_id
        FROM fund_nav
        GROUP BY fund_id
        HAVING MIN(obs_date) <= DATE '2015-04-30'
           AND MAX(obs_date) >= DATE '2025-03-31'
  );

-- ------------------------------------------------------------------
-- 3. MONTHLY SIMPLE RETURNS (gross and net)
-- ------------------------------------------------------------------

CREATE OR REPLACE VIEW v_fund_monthly_returns AS
SELECT
    n.fund_id,
    f.fund_name,
    f.fund_type,
    f.annual_fee_pct,
    n.obs_date,
    n.nav_gross,
    n.nav_net,
    n.nav_gross / LAG(n.nav_gross) OVER (PARTITION BY n.fund_id ORDER BY n.obs_date) - 1 AS r_gross,
    n.nav_net   / LAG(n.nav_net)   OVER (PARTITION BY n.fund_id ORDER BY n.obs_date) - 1 AS r_net
FROM fund_nav n
JOIN funds f ON f.fund_id = n.fund_id
WHERE n.obs_date BETWEEN DATE '2015-04-01' AND DATE '2025-03-31'
  AND n.fund_id IN (SELECT fund_id FROM v_fund_universe);

-- ------------------------------------------------------------------
-- 4. MARKET EXCESS RETURN
-- ------------------------------------------------------------------

CREATE OR REPLACE VIEW v_market_excess_return AS
SELECT
    m.obs_date,
    (m.omxs_level / LAG(m.omxs_level) OVER (ORDER BY m.obs_date) - 1) - rf.rf_rate_m AS ex_r_market
FROM market_index m
JOIN risk_free rf ON rf.obs_date = m.obs_date;

-- ------------------------------------------------------------------
-- 5. FULL EXCESS-RETURN PANEL
--    (fund excess return, market excess return, SMB, HML)
--    -> this is the table consumed by the R / Python scripts
-- ------------------------------------------------------------------

CREATE OR REPLACE VIEW v_fund_excess_returns AS
SELECT
    v.fund_id,
    v.fund_name,
    v.fund_type,
    v.annual_fee_pct,
    v.obs_date,
    v.nav_gross,
    v.nav_net,
    v.r_gross,
    v.r_net,
    rf.rf_rate_m,
    v.r_gross - rf.rf_rate_m AS ex_r_gross,
    v.r_net   - rf.rf_rate_m AS ex_r_net,
    mkt.ex_r_market,
    ff.smb,
    ff.hml
FROM v_fund_monthly_returns v
JOIN risk_free rf              ON rf.obs_date  = v.obs_date
JOIN v_market_excess_return mkt ON mkt.obs_date = v.obs_date
LEFT JOIN ff_factors ff        ON ff.obs_date  = v.obs_date
WHERE v.r_gross IS NOT NULL;   -- drop the first month of each fund (no lag available)

-- ------------------------------------------------------------------
-- 6. TOTAL RETURN OVER THE FULL 10-YEAR WINDOW (per fund)
-- ------------------------------------------------------------------

CREATE OR REPLACE VIEW v_fund_total_return AS
WITH endpoints AS (
    SELECT
        fund_id,
        fund_name,
        fund_type,
        annual_fee_pct,
        MIN(obs_date) FILTER (WHERE obs_date <= DATE '2015-04-30') AS start_date,
        MAX(obs_date) FILTER (WHERE obs_date <= DATE '2025-03-31') AS end_date
    FROM v_fund_monthly_returns
    GROUP BY fund_id, fund_name, fund_type, annual_fee_pct
)
SELECT
    e.fund_id, e.fund_name, e.fund_type, e.annual_fee_pct,
    (n_end.nav_gross / n_start.nav_gross - 1) * 100 AS total_return_gross_pct,
    (n_end.nav_net   / n_start.nav_net   - 1) * 100 AS total_return_net_pct
FROM endpoints e
JOIN fund_nav n_start ON n_start.fund_id = e.fund_id AND n_start.obs_date = e.start_date
JOIN fund_nav n_end   ON n_end.fund_id   = e.fund_id AND n_end.obs_date   = e.end_date;

-- ------------------------------------------------------------------
-- 7. GROUP-LEVEL SUMMARY (ACTIVE VS PASSIVE) -- sanity check numbers
--    quoted in the thesis (Section 4-5)
-- ------------------------------------------------------------------

SELECT
    fund_type,
    COUNT(*)                                   AS n_funds,
    ROUND(AVG(total_return_gross_pct), 2)       AS mean_gross_return,
    ROUND(AVG(total_return_net_pct), 2)         AS mean_net_return,
    ROUND(STDDEV(total_return_gross_pct), 2)    AS sd_gross_return,
    ROUND(STDDEV(total_return_net_pct), 2)      AS sd_net_return
FROM v_fund_total_return
GROUP BY fund_type;

-- ------------------------------------------------------------------
-- 8. FEE vs RETURN CORRELATION INPUT (Figures 5-6, Table 17)
-- ------------------------------------------------------------------

SELECT fund_id, fund_name, fund_type, annual_fee_pct, total_return_gross_pct
FROM v_fund_total_return
ORDER BY fund_type, annual_fee_pct;

-- ------------------------------------------------------------------
-- 9. EXPORT FOR DOWNSTREAM ANALYSIS (R / Python)
--    Run these from psql, or via your BI/ETL tool of choice.
-- ------------------------------------------------------------------

-- \copy (SELECT * FROM v_fund_excess_returns ORDER BY fund_id, obs_date)
--       TO 'fund_excess_returns.csv' CSV HEADER;

-- \copy (SELECT * FROM v_fund_universe)
--       TO 'fund_info.csv' CSV HEADER;

-- \copy (SELECT * FROM v_fund_total_return)
--       TO 'fund_total_return.csv' CSV HEADER;
