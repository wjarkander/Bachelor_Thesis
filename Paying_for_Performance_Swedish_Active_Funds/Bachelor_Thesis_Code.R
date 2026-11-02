# ==================================================================
# "Paying for Performance: Are Swedish Active Funds Worth the Extra
#  Cost?" -- full replication script
#  Fabian Lindh & William Jarkander, Uppsala University, 2025
#
#  Reads the excess-return panel produced by
#  fund_analysis_schema_queries.sql (fund_excess_returns.csv) and
#  reproduces every figure/table described in the thesis: Fig 1-10,
#  Tables 1-6 and 11-17.
# ==================================================================

set.seed(123)

# ---- 0. Packages --------------------------------------------------
library(tidyverse)   # dplyr, ggplot2, purrr, tidyr
library(lubridate)
library(broom)

# ---- 1. Import data -------------------------------------------------
# excess_returns columns:
#   fund_id, fund_name, fund_type, annual_fee_pct, obs_date,
#   nav_gross, nav_net, r_gross, r_net, rf_rate_m,
#   ex_r_gross, ex_r_net, ex_r_market, smb, hml
excess_returns <- read_csv("fund_excess_returns.csv", show_col_types = FALSE) %>%
  mutate(obs_date = as.Date(obs_date))

fund_info <- read_csv("fund_info.csv", show_col_types = FALSE)          # fund_id, fund_name, fund_type, annual_fee_pct
market_index <- read_csv("market_index.csv", show_col_types = FALSE) %>% # obs_date, omxs_level
  mutate(obs_date = as.Date(obs_date))

# ---- 2. Total return & Sharpe ratio ---------------------------------

total_return_pct <- function(nav_start, nav_end) (nav_end / nav_start - 1) * 100

sharpe_ratio <- function(monthly_returns, monthly_rf) {
  excess_m <- monthly_returns - monthly_rf
  mean(excess_m) / sd(monthly_returns) * sqrt(12)
}

fund_summary <- excess_returns %>%
  arrange(obs_date) %>%
  group_by(fund_id, fund_name, fund_type) %>%
  summarise(
    total_return_gross = total_return_pct(first(nav_gross), last(nav_gross)),
    total_return_net   = total_return_pct(first(nav_net),   last(nav_net)),
    sharpe_net          = sharpe_ratio(r_net, rf_rate_m),
    .groups = "drop"
  ) %>%
  left_join(fund_info %>% select(fund_id, annual_fee_pct), by = "fund_id")

group_means <- fund_summary %>%
  group_by(fund_type) %>%
  summarise(
    mean_gross = mean(total_return_gross), sd_gross = sd(total_return_gross),
    mean_net   = mean(total_return_net),   sd_net   = sd(total_return_net),
    mean_sharpe = mean(sharpe_net),        sd_sharpe = sd(sharpe_net)
  )
print(group_means)

# ---- 3. Descriptive plots -------------------------------------------

## Figure 1: cumulative return, active vs passive, gross & net, + OMXS
cum_returns <- excess_returns %>%
  group_by(fund_type, obs_date) %>%
  summarise(r_gross = mean(r_gross), r_net = mean(r_net), .groups = "drop") %>%
  arrange(fund_type, obs_date) %>%
  group_by(fund_type) %>%
  mutate(cum_gross = (cumprod(1 + r_gross) - 1) * 100,
         cum_net   = (cumprod(1 + r_net) - 1) * 100) %>%
  ungroup()

omxs_cum <- market_index %>%
  arrange(obs_date) %>%
  mutate(r = omxs_level / lag(omxs_level) - 1,
         cum = (cumprod(1 + replace_na(r, 0)) - 1) * 100)

fig1 <- ggplot() +
  geom_line(data = cum_returns, aes(obs_date, cum_gross,
                                     color = paste(fund_type, "before fees"))) +
  geom_line(data = cum_returns, aes(obs_date, cum_net,
                                     color = paste(fund_type, "after fees"))) +
  geom_line(data = omxs_cum, aes(obs_date, cum, color = "OMXS Index")) +
  labs(title = "Return before and after fees",
       x = "Year", y = "Cumulative return (%)", color = "Series") +
  theme_minimal()
print(fig1)

## Figure 2: active - passive difference, before/after fees
diff_df <- cum_returns %>%
  select(fund_type, obs_date, cum_gross, cum_net) %>%
  pivot_wider(names_from = fund_type, values_from = c(cum_gross, cum_net)) %>%
  mutate(diff_before = cum_gross_active - cum_gross_passive,
         diff_after  = cum_net_active   - cum_net_passive)

fig2 <- ggplot(diff_df, aes(obs_date)) +
  geom_line(aes(y = diff_before, color = "Before fees")) +
  geom_line(aes(y = diff_after, color = "After fees")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Return Difference Before and After Fees (Active - Passive)",
       x = "Year", y = "Return Difference in Percentage Points", color = NULL) +
  theme_minimal()
print(fig2)

## Figure 3 & 4: cumulative return per individual fund
fund_cum <- excess_returns %>%
  arrange(fund_id, obs_date) %>%
  group_by(fund_id) %>%
  mutate(cum_gross = (cumprod(1 + r_gross) - 1) * 100) %>%
  ungroup() %>%
  left_join(fund_info %>% select(fund_id, fund_type), by = "fund_id")

fig3 <- ggplot(filter(fund_cum, fund_type == "active"),
                aes(obs_date, cum_gross, color = fund_name)) +
  geom_line(show.legend = FALSE) +
  labs(title = "Cumulative returns across active funds (before fees)",
       x = "Year", y = "Cumulative return (%)") +
  theme_minimal()
print(fig3)

fig4 <- ggplot(filter(fund_cum, fund_type == "passive"),
                aes(obs_date, cum_gross, color = fund_name)) +
  geom_line(show.legend = FALSE) +
  labs(title = "Cumulative returns across passive funds (before fees)",
       x = "Year", y = "Cumulative return (%)") +
  theme_minimal()
print(fig4)

## Figure 5 & 6: fee vs total return scatterplots (Table 17 regression)
fig5 <- ggplot(filter(fund_summary, fund_type == "active"),
                aes(annual_fee_pct, total_return_gross)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Fee & Total Return Scatterplot: Active funds",
       x = "Annual Fee (%)", y = "Total Return (%)") +
  theme_minimal()
print(fig5)

fig6 <- ggplot(filter(fund_summary, fund_type == "passive"),
                aes(annual_fee_pct, total_return_gross)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Fee & Total Return Scatterplot: Passive funds",
       x = "Annual Fee (%)", y = "Total Return (%)") +
  theme_minimal()
print(fig6)

cor_active  <- cor(fund_summary$annual_fee_pct[fund_summary$fund_type == "active"],
                    fund_summary$total_return_gross[fund_summary$fund_type == "active"])
cor_passive <- cor(fund_summary$annual_fee_pct[fund_summary$fund_type == "passive"],
                    fund_summary$total_return_gross[fund_summary$fund_type == "passive"])
cat("Correlation fee vs gross return - active:", round(cor_active, 3),
    " passive:", round(cor_passive, 3), "\n")

# Table 17: regression of gross return on annual fee (all funds pooled)
fee_reg <- lm(total_return_gross ~ annual_fee_pct, data = fund_summary)
summary(fee_reg)

## Figure 7: Sharpe ratio boxplot
fig7 <- ggplot(fund_summary, aes(fund_type, sharpe_net, fill = fund_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.7) +
  labs(title = "Sharpe ratio boxplot", x = NULL, y = "Sharpe ratio") +
  theme_minimal()
print(fig7)

# ---- 4. Normality checks: QQ-plots (Fig 8) & Shapiro-Wilk (Table 1) --

vars_to_test <- list(
  "Active gross"    = fund_summary$total_return_gross[fund_summary$fund_type == "active"],
  "Passive gross"   = fund_summary$total_return_gross[fund_summary$fund_type == "passive"],
  "Active net"      = fund_summary$total_return_net[fund_summary$fund_type == "active"],
  "Passive net"     = fund_summary$total_return_net[fund_summary$fund_type == "passive"],
  "Sharpe: Active"  = fund_summary$sharpe_net[fund_summary$fund_type == "active"],
  "Sharpe: Passive" = fund_summary$sharpe_net[fund_summary$fund_type == "passive"]
)

qq_data <- imap_dfr(vars_to_test, ~ tibble(group = .y, value = .x))

fig8 <- ggplot(qq_data, aes(sample = value)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~ group, scales = "free") +
  labs(x = "Theoretical", y = "Observed") +
  theme_minimal()
print(fig8)

shapiro_results <- imap_dfr(vars_to_test, ~ {
  test <- shapiro.test(.x)
  tibble(variable = .y, W = unname(test$statistic), p_value = test$p.value)
})
print(shapiro_results)   # -> Table 1

# ---- 5. Welch two-sample t-tests (Table 2) --------------------------
# H0: mu_active <= mu_passive   H1: mu_active > mu_passive

t_gross  <- t.test(total_return_gross ~ fund_type, data = fund_summary,
                    alternative = "greater", var.equal = FALSE)
t_net    <- t.test(total_return_net ~ fund_type, data = fund_summary,
                    alternative = "greater", var.equal = FALSE)
t_sharpe <- t.test(sharpe_net ~ fund_type, data = fund_summary,
                    alternative = "greater", var.equal = FALSE)

t_test_table <- tibble(
  comparison = c("Gross return", "Net return", "Sharpe ratio"),
  t_value    = c(t_gross$statistic, t_net$statistic, t_sharpe$statistic),
  df         = c(t_gross$parameter, t_net$parameter, t_sharpe$parameter),
  p_value    = c(t_gross$p.value, t_net$p.value, t_sharpe$p.value)
)
print(t_test_table)   # -> Table 2

# ---- 6. Single-factor (Jensen) model, full 2015-2025 window --------

fit_single_factor <- function(df) {
  fit_gross <- lm(ex_r_gross ~ ex_r_market, data = df)
  fit_net   <- lm(ex_r_net   ~ ex_r_market, data = df)
  tibble(
    alpha_gross = unname(coef(fit_gross)[1]), beta_gross = unname(coef(fit_gross)[2]),
    alpha_net   = unname(coef(fit_net)[1]),   beta_net   = unname(coef(fit_net)[2])
  )
}

single_factor_results <- excess_returns %>%
  group_by(fund_id, fund_name, fund_type) %>%
  group_modify(~ fit_single_factor(.x)) %>%
  ungroup()

single_factor_summary <- single_factor_results %>%
  group_by(fund_type) %>%
  summarise(
    mean_alpha_gross = mean(alpha_gross), sd_alpha_gross = sd(alpha_gross),
    mean_alpha_net   = mean(alpha_net),   sd_alpha_net   = sd(alpha_net)
  )
print(single_factor_summary)   # -> Table 3

# ---- 7. Three-factor (Fama-French) model, 2015-2019 subset ----------
# SMB/HML for the Swedish market only available through Dec 2019

ff_window <- excess_returns %>% filter(obs_date <= as.Date("2019-12-31"))

fit_three_factor <- function(df) {
  fit_gross <- lm(ex_r_gross ~ ex_r_market + smb + hml, data = df)
  fit_net   <- lm(ex_r_net   ~ ex_r_market + smb + hml, data = df)
  tibble(
    alpha_gross    = unname(coef(fit_gross)[1]),
    beta_m_gross   = unname(coef(fit_gross)[2]),
    beta_smb_gross = unname(coef(fit_gross)[3]),
    beta_hml_gross = unname(coef(fit_gross)[4]),
    alpha_net      = unname(coef(fit_net)[1]),
    beta_m_net     = unname(coef(fit_net)[2]),
    beta_smb_net   = unname(coef(fit_net)[3]),
    beta_hml_net   = unname(coef(fit_net)[4])
  )
}

three_factor_results <- ff_window %>%
  group_by(fund_id, fund_name, fund_type) %>%
  group_modify(~ fit_three_factor(.x)) %>%
  ungroup()

three_factor_summary <- three_factor_results %>%
  group_by(fund_type) %>%
  summarise(
    mean_alpha_gross = mean(alpha_gross), sd_alpha_gross = sd(alpha_gross),
    mean_alpha_net   = mean(alpha_net),   sd_alpha_net   = sd(alpha_net)
  )
print(three_factor_summary)   # -> Table 4

# ---- 8. Bootstrap analysis: skill vs luck (Fig 9-10, Table 5-6) -----
#
# Under H0: alpha_i = 0 for all funds. Residuals from the original
# fit are resampled with replacement and combined with the fund's
# estimated betas to build a synthetic return series with alpha
# forced to zero (eq. 6-7 in the thesis). Re-estimating alpha on
# this synthetic series B times gives the empirical null
# distribution (eq. 8-9); the bootstrap p-value (eq. 10) is the
# share of bootstrap alphas at or above the fund's actual alpha.

bootstrap_alpha <- function(r, market, smb = NULL, hml = NULL,
                             B = 10000, model = c("single", "three")) {
  model <- match.arg(model)

  if (model == "single") {
    fit <- lm(r ~ market)
  } else {
    fit <- lm(r ~ market + smb + hml)
  }

  alpha_hat <- unname(coef(fit)[1])
  betas     <- unname(coef(fit)[-1])
  resid_hat <- unname(residuals(fit))
  n <- length(r)

  boot_alpha <- numeric(B)

  for (b in seq_len(B)) {
    eps_b <- sample(resid_hat, n, replace = TRUE)

    if (model == "single") {
      r_b   <- 0 + betas[1] * market + eps_b
      fit_b <- lm(r_b ~ market)
    } else {
      r_b   <- 0 + betas[1] * market + betas[2] * smb + betas[3] * hml + eps_b
      fit_b <- lm(r_b ~ market + smb + hml)
    }
    boot_alpha[b] <- unname(coef(fit_b)[1])
  }

  p_value <- mean(boot_alpha >= alpha_hat)
  list(alpha_hat = alpha_hat, boot_alpha = boot_alpha, p_value = p_value)
}

# 8a. Single-factor bootstrap for every fund (net returns), B = 10,000
boot_single <- excess_returns %>%
  group_by(fund_id, fund_name, fund_type) %>%
  group_map(~ {
    res <- bootstrap_alpha(.x$ex_r_net, .x$ex_r_market, model = "single", B = 10000)
    tibble(fund_id = .y$fund_id, fund_name = .y$fund_name, fund_type = .y$fund_type,
           alpha_net_single = res$alpha_hat, p_value_single = res$p_value)
  }) %>%
  bind_rows()

# 8b. Three-factor bootstrap for every fund (net returns, 2015-2019), B = 10,000
boot_three <- ff_window %>%
  group_by(fund_id, fund_name, fund_type) %>%
  group_map(~ {
    res <- bootstrap_alpha(.x$ex_r_net, .x$ex_r_market, .x$smb, .x$hml,
                            model = "three", B = 10000)
    tibble(fund_id = .y$fund_id, fund_name = .y$fund_name, fund_type = .y$fund_type,
           alpha_net_three = res$alpha_hat, p_value_three = res$p_value)
  }) %>%
  bind_rows()

boot_results <- full_join(boot_single, boot_three,
                           by = c("fund_id", "fund_name", "fund_type"))
print(boot_results)   # -> Tables 5, 6, 15, 16

# 8c. Bootstrap histograms for the top-alpha funds (Fig 9 & 10)
plot_bootstrap_hist <- function(fund_name_target, data, model = "single") {
  fund_data <- excess_returns %>% filter(fund_name == fund_name_target)
  if (model == "three") fund_data <- fund_data %>% filter(obs_date <= as.Date("2019-12-31"))

  res <- if (model == "single") {
    bootstrap_alpha(fund_data$ex_r_net, fund_data$ex_r_market, model = "single", B = 10000)
  } else {
    bootstrap_alpha(fund_data$ex_r_net, fund_data$ex_r_market,
                     fund_data$smb, fund_data$hml, model = "three", B = 10000)
  }

  ggplot(tibble(alpha = res$boot_alpha), aes(alpha)) +
    geom_histogram(bins = 40, fill = "navy") +
    geom_vline(xintercept = res$alpha_hat, color = "red", linetype = "dashed") +
    labs(title = fund_name_target, x = expression(alpha), y = "Frequency") +
    theme_minimal()
}

# Example: top two single-factor funds (Fig 9) and top two three-factor funds (Fig 10)
top_single <- single_factor_results %>% filter(fund_type == "active") %>%
  slice_max(alpha_net, n = 2) %>% pull(fund_name)
top_three  <- three_factor_results %>% filter(fund_type == "active") %>%
  slice_max(alpha_net, n = 2) %>% pull(fund_name)

fig9  <- map(top_single, plot_bootstrap_hist, model = "single")
fig10 <- map(top_three,  plot_bootstrap_hist, model = "three")
walk(fig9, print)
walk(fig10, print)

# ---- 9. Export result tables ----------------------------------------
write_csv(fund_summary,           "fund_summary.csv")
write_csv(single_factor_results,  "single_factor_results.csv")
write_csv(three_factor_results,   "three_factor_results.csv")
write_csv(boot_results,           "bootstrap_pvalues.csv")
write_csv(shapiro_results,        "shapiro_wilk_results.csv")
write_csv(t_test_table,           "welch_t_test_results.csv")
