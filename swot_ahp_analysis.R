# #########################################################
# Significance: * p<0.05, ** p<0.01, *** p<0.001
# #########################################################


# =========================================================
# Step 0. Environment & packages
# =========================================================
rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(MASS)
library(modelsummary)
library(broom)
library(openxlsx)
library(lavaan)

# =========================================================
# Step 1. Paths and import
# =========================================================
base_dir <- "~/Desktop/勉強/研究用/博士/swotに関する研究"

file_weight  <- file.path(base_dir, "AHP_SWOT_weights_table_clean.xlsx")
file_data    <- file.path(base_dir, "strategy_data.xlsx")
file_scored  <- file.path(base_dir, "strategy_data_with_SWOT_scores.xlsx")
file_results <- file.path(base_dir, "SWOT_AHP_final_all_results.xlsx")

if (!file.exists(file_weight)) stop(paste0("Weight file does not exist: ", file_weight))
if (!file.exists(file_data))   stop(paste0("Raw data file does not exist: ", file_data))

w_raw  <- readxl::read_excel(file_weight, sheet = "SWOT_weights")
df_raw <- readxl::read_excel(file_data, sheet = 1)
names(w_raw)  <- trimws(names(w_raw))
names(df_raw) <- trimws(names(df_raw))

cat("N weight rows =", nrow(w_raw), " | N raw rows =", nrow(df_raw), "\n")

# =========================================================
# Step 2. Helper functions
# =========================================================
add_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE ~ ""
  )
}

star_rule <- c("*" = 0.05, "**" = 0.01, "***" = 0.001)

calc_index <- function(data, vars, weights) {
  as.numeric(as.matrix(data[, vars]) %*% weights)
}

calc_mcfadden <- function(model) {
  ll_full <- as.numeric(logLik(model))
  ll_null <- as.numeric(logLik(update(model, . ~ 1, data = model$model)))
  1 - (ll_full / ll_null)
}

calc_cramers_v <- function(tab) {
  chi2 <- suppressWarnings(chisq.test(tab)$statistic)
  n <- sum(tab); r <- nrow(tab); c <- ncol(tab)
  as.numeric(sqrt(chi2 / (n * min(r - 1, c - 1))))
}

get_polr_p <- function(model) {
  ct <- coef(summary(model))
  p <- 2 * pnorm(abs(ct[, "t value"]), lower.tail = FALSE)
  as.data.frame(cbind(ct, "p_value" = p))
}

fmt_polr_table <- function(model, model_name = NA_character_, outcome_name = NA_character_) {
  res_df <- get_polr_p(model)
  res_df %>%
    dplyr::mutate(
      outcome = outcome_name, model = model_name, term = rownames(res_df),
      stars = add_stars(p_value),
      estimate = Value, std.error = `Std. Error`, t.value = `t value`, p.value = p_value,
      estimate_fmt = paste0(sprintf("%.4f", Value), stars),
      se_fmt = paste0("(", sprintf("%.4f", `Std. Error`), ")"),
      t_fmt = sprintf("%.3f", `t value`),
      p_fmt = sprintf("%.4f", p_value)
    ) %>%
    dplyr::select(outcome, model, term, estimate, std.error, t.value, p.value,
                  stars, estimate_fmt, se_fmt, t_fmt, p_fmt)
}

polr_or_ci_table <- function(model, model_name = NA_character_, outcome_name = NA_character_) {
  ct <- coef(summary(model))
  p <- 2 * pnorm(abs(ct[, "t value"]), lower.tail = FALSE)
  out <- data.frame(outcome = outcome_name, model = model_name, term = rownames(ct),
                    estimate = ct[, "Value"], std.error = ct[, "Std. Error"],
                    t.value = ct[, "t value"], p.value = p, row.names = NULL)
  out %>%
    dplyr::filter(!grepl("\\|", term)) %>%
    dplyr::mutate(
      stars = add_stars(p.value),
      OR = exp(estimate), OR_low = exp(estimate - 1.96 * std.error), OR_high = exp(estimate + 1.96 * std.error),
      OR_fmt = paste0(sprintf("%.3f", OR), stars),
      CI_fmt = paste0("[", sprintf("%.3f", OR_low), ", ", sprintf("%.3f", OR_high), "]"),
      p_fmt = sprintf("%.4f", p.value)
    )
}

tidy_lm_result <- function(model, model_name, outcome_name) {
  broom::tidy(model) %>%
    dplyr::mutate(
      outcome = outcome_name, model = model_name, stars = add_stars(p.value),
      estimate_fmt = paste0(sprintf("%.4f", estimate), stars),
      se_fmt = paste0("(", sprintf("%.4f", std.error), ")"),
      t_fmt = sprintf("%.3f", statistic), p_fmt = sprintf("%.4f", p.value)
    ) %>%
    dplyr::select(outcome, model, term, estimate, std.error, statistic, p.value,
                  stars, estimate_fmt, se_fmt, t_fmt, p_fmt)
}

make_cat_desc <- function(data, var_name, label_name) {
  data %>%
    dplyr::count(.data[[var_name]]) %>%
    dplyr::mutate(Variable = label_name, Category = as.character(.data[[var_name]]),
                  Percent = round(n / sum(n) * 100, 2)) %>%
    dplyr::select(Variable, Category, N = n, Percent)
}

clean_sheet_name <- function(x) substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", x), 1, 31)

write_three_line_sheet <- function(wb, sheet_name, data, note = NULL) {
  sheet_name <- clean_sheet_name(sheet_name)
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  if (!is.null(note)) {
    openxlsx::writeData(wb, sheet = sheet_name, x = note, startRow = 1, startCol = 1, colNames = FALSE)
    start_row <- 3
  } else start_row <- 1
  data <- as.data.frame(data)
  openxlsx::writeData(wb, sheet = sheet_name, x = data, startRow = start_row, startCol = 1, colNames = TRUE)
  n_rows <- nrow(data); n_cols <- ncol(data)
  if (n_cols == 0) return(invisible(NULL))
  header_row <- start_row; last_row <- start_row + n_rows
  header_style <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center",
                                        border = c("top", "bottom"), borderStyle = "thin")
  body_style <- openxlsx::createStyle(valign = "center")
  bottom_style <- openxlsx::createStyle(border = "bottom", borderStyle = "thin")
  openxlsx::addStyle(wb, sheet_name, header_style, rows = header_row, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  if (n_rows > 0) {
    openxlsx::addStyle(wb, sheet_name, body_style, rows = (header_row + 1):last_row, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, sheet_name, bottom_style, rows = last_row, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::freezePane(wb, sheet = sheet_name, firstActiveRow = header_row + 1)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 1:n_cols, widths = "auto")
  invisible(NULL)
}

# =========================================================
# Step 3. SWOT mapping & AHP weights
# =========================================================
swot_map <- dplyr::tibble(
  Indicators = c("S1","S2","S3","S4","W1","W2","W3","W4","O1","O2","O3","O4","T1","T2","T3","T4"),
  variable = c("Prod_Quality","Tech_Level","Comm_Skill","Price_Comp",
               "Biz_Scale","Financial","Live_Exp","Device_Diff",
               "Mkt_Demand","Gov_Support","Platform_Sup","Net_Infra",
               "Cons_Aware","Comp_Rivalry","Cons_Trust","Logistics"),
  SWOT_group = c(rep("S",4), rep("W",4), rep("O",4), rep("T",4)),
  order_id = 1:16
)

w  <- w_raw %>% dplyr::filter(!is.na(Indicators))
w2 <- w %>%
  dplyr::left_join(swot_map, by = "Indicators") %>%
  dplyr::arrange(order_id) %>%
  tidyr::fill(Guidelines, Guideline_Weights, .direction = "down")
if (any(is.na(w2$variable))) stop("Some AHP indicators were not matched. Check Indicators names.")

S_vars <- w2 %>% dplyr::filter(SWOT_group == "S") %>% dplyr::arrange(order_id) %>% dplyr::pull(variable)
W_vars <- w2 %>% dplyr::filter(SWOT_group == "W") %>% dplyr::arrange(order_id) %>% dplyr::pull(variable)
O_vars <- w2 %>% dplyr::filter(SWOT_group == "O") %>% dplyr::arrange(order_id) %>% dplyr::pull(variable)
T_vars <- w2 %>% dplyr::filter(SWOT_group == "T") %>% dplyr::arrange(order_id) %>% dplyr::pull(variable)
all_swot_vars <- w2 %>% dplyr::arrange(order_id) %>% dplyr::pull(variable)

missing_swot_vars <- setdiff(all_swot_vars, names(df_raw))
if (length(missing_swot_vars) > 0) stop(paste0("Missing SWOT vars in raw data: ", paste(missing_swot_vars, collapse = ", ")))

pull_w <- function(g) { x <- w2 %>% dplyr::filter(SWOT_group == g) %>% dplyr::arrange(order_id); v <- x$Indicator_Weights; names(v) <- x$variable; v / sum(v, na.rm = TRUE) }
vec_S <- pull_w("S"); vec_W <- pull_w("W"); vec_O <- pull_w("O"); vec_T <- pull_w("T")
if (any(is.na(c(vec_S, vec_W, vec_O, vec_T)))) stop("Some AHP Indicator_Weights are missing.")

# =========================================================
# Step 4. Construct SWOT indices, strategy type, and CLEAN internal index
# =========================================================
df <- df_raw %>% dplyr::mutate(dplyr::across(dplyr::all_of(all_swot_vars), ~ as.numeric(.)))

# range check 1-5
range_bad <- df %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(all_swot_vars),
                                 list(Min = ~min(., na.rm = TRUE), Max = ~max(., na.rm = TRUE)))) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "v", values_to = "val") %>%
  tidyr::separate(v, into = c("Variable", "Stat"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Stat, values_from = val) %>%
  dplyr::filter(Min < 1 | Max > 5)
if (nrow(range_bad) > 0) { print(range_bad); stop("Some SWOT items outside 1-5.") }

# favorable-direction indices (kept for checking)
df <- df %>%
  dplyr::mutate(
    S_index      = calc_index(., names(vec_S), vec_S),
    W_good_index = calc_index(., names(vec_W), vec_W),
    O_index      = calc_index(., names(vec_O), vec_O),
    T_good_index = calc_index(., names(vec_T), vec_T)
  )

# reverse W/T for traditional SWOT direction (higher = stronger weakness/threat)
df <- df %>%
  dplyr::mutate(
    dplyr::across(dplyr::all_of(W_vars), ~ 6 - ., .names = "{.col}_weakness"),
    dplyr::across(dplyr::all_of(T_vars), ~ 6 - ., .names = "{.col}_threat")
  )
W_weakness_vars <- paste0(W_vars, "_weakness")
T_threat_vars   <- paste0(T_vars, "_threat")
vec_W_weakness <- vec_W; names(vec_W_weakness) <- W_weakness_vars
vec_T_threat   <- vec_T; names(vec_T_threat)   <- T_threat_vars

df <- df %>%
  dplyr::mutate(
    W_weakness_index = calc_index(., names(vec_W_weakness), vec_W_weakness),
    T_threat_index   = calc_index(., names(vec_T_threat), vec_T_threat),
    Internal_diff = S_index - W_weakness_index,
    External_diff = O_index - T_threat_index,
    Strategy_Factor = dplyr::case_when(
      Internal_diff >= 0 & External_diff >= 0 ~ "SO",
      Internal_diff >= 0 & External_diff <  0 ~ "ST",
      Internal_diff <  0 & External_diff >= 0 ~ "WO",
      Internal_diff <  0 & External_diff <  0 ~ "WT",
      TRUE ~ NA_character_),
    Strategy_Factor = factor(Strategy_Factor, levels = c("WT","WO","ST","SO"))
  )

# [MODIFIED] CLEAN internal index: drop Financial only (income proxy).
# Biz_Scale stays as a W item, reverse-coded and renormalized.
# W still has 4 items in the main model; clean index uses 3 W items for robustness only.
W_drop          <- c("Financial")                                        # ← MODIFIED: 原为 c("Biz_Scale", "Financial")
W_keep          <- setdiff(W_vars, W_drop)                               # Biz_Scale, Live_Exp, Device_Diff
W_keep_weakness <- paste0(W_keep, "_weakness")
vec_W_keep <- vec_W[W_keep]; vec_W_keep <- vec_W_keep / sum(vec_W_keep); names(vec_W_keep) <- W_keep_weakness
df <- df %>%
  dplyr::mutate(
    W_weakness_index_clean = calc_index(., names(vec_W_keep), vec_W_keep),
    Internal_diff_clean    = S_index - W_weakness_index_clean
  )
# equal-weight variants (reverse-coded; for overlap robustness)
df$S_eq              <- rowMeans(df[, S_vars], na.rm = TRUE)
df$Wweak_eq_full     <- rowMeans(df[, W_weakness_vars], na.rm = TRUE)
df$Wweak_eq_clean    <- rowMeans(df[, W_keep_weakness], na.rm = TRUE)
df$Internal_eq_full  <- df$S_eq - df$Wweak_eq_full
df$Internal_eq_clean <- df$S_eq - df$Wweak_eq_clean

openxlsx::write.xlsx(df, file = file_scored, overwrite = TRUE)

# =========================================================
# Step 5. Descriptives
# =========================================================
index_stats <- df %>%
  dplyr::summarise(dplyr::across(
    c(S_index, W_weakness_index, O_index, T_threat_index, Internal_diff, External_diff, W_good_index, T_good_index),
    list(N = ~sum(!is.na(.)), Mean = ~mean(., na.rm = TRUE), SD = ~sd(., na.rm = TRUE),
         Min = ~min(., na.rm = TRUE), Median = ~median(., na.rm = TRUE), Max = ~max(., na.rm = TRUE)))) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "v", values_to = "val") %>%
  tidyr::separate(v, into = c("Variable", "Stat"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Stat, values_from = val) %>%
  dplyr::mutate(dplyr::across(c(Mean, SD, Min, Median, Max), ~round(., 3)))

strategy_dist <- df %>% dplyr::count(Strategy_Factor) %>%
  dplyr::mutate(Percent = round(n / sum(n) * 100, 2)) %>%
  dplyr::rename(Strategy_type = Strategy_Factor, N = n)

desc_livestream <- df %>% dplyr::count(livestream_flag) %>%
  dplyr::mutate(Group = dplyr::case_when(livestream_flag == 0 ~ "Non-participants",
                                         livestream_flag == 1 ~ "Participants", TRUE ~ "Missing"),
                Percent = round(n / sum(n) * 100, 2)) %>%
  dplyr::select(Group, livestream_flag, N = n, Percent)

desc_farmer_numeric <- df %>%
  dplyr::summarise(dplyr::across(c(age, education, farm_size, Labor),
                                 list(N = ~sum(!is.na(.)), Mean = ~mean(., na.rm = TRUE), SD = ~sd(., na.rm = TRUE),
                                      Min = ~min(., na.rm = TRUE), Median = ~median(., na.rm = TRUE), Max = ~max(., na.rm = TRUE)))) %>%
  tidyr::pivot_longer(dplyr::everything(), names_to = "v", values_to = "val") %>%
  tidyr::separate(v, into = c("Variable", "Stat"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Stat, values_from = val) %>%
  dplyr::mutate(dplyr::across(c(Mean, SD, Min, Median, Max), ~round(., 3)))

desc_categorical_all <- dplyr::bind_rows(
  make_cat_desc(df, "gender", "Gender"),
  make_cat_desc(df, "main_sales_route", "Main sales route"),
  make_cat_desc(df, "Funding_Sources", "Funding sources"),
  make_cat_desc(df, "livestream_flag", "Livestream participation"),
  make_cat_desc(df, "Strategy_Factor", "SWOT strategic type"),
  make_cat_desc(df, "income_total", "Total income"),
  make_cat_desc(df, "Live_share", "Livestream sales share")
)

# =========================================================
# Step 6. Participation/non-participation differences
# =========================================================
swot_compare_vars <- c("S_index","W_weakness_index","O_index","T_threat_index","Internal_diff","External_diff")

desc_swot_by_live <- df %>%
  dplyr::mutate(Group = dplyr::case_when(livestream_flag == 0 ~ "Non-participants",
                                         livestream_flag == 1 ~ "Participants", TRUE ~ NA_character_)) %>%
  dplyr::filter(!is.na(Group)) %>% dplyr::group_by(Group) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(swot_compare_vars),
                                 list(N = ~sum(!is.na(.)), Mean = ~mean(., na.rm = TRUE), SD = ~sd(., na.rm = TRUE), Median = ~median(., na.rm = TRUE))), .groups = "drop") %>%
  tidyr::pivot_longer(-Group, names_to = "v", values_to = "val") %>%
  tidyr::separate(v, into = c("Variable", "Stat"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = Stat, values_from = val) %>%
  dplyr::mutate(dplyr::across(c(Mean, SD, Median), ~round(., 3)))

ttest_one_var <- function(var_name) {
  tr <- t.test(as.formula(paste(var_name, "~ livestream_flag")), data = df)
  gs <- df %>% dplyr::filter(livestream_flag %in% c(0, 1)) %>% dplyr::group_by(livestream_flag) %>%
    dplyr::summarise(N = sum(!is.na(.data[[var_name]])), Mean = mean(.data[[var_name]], na.rm = TRUE),
                     SD = sd(.data[[var_name]], na.rm = TRUE), .groups = "drop")
  tibble::tibble(
    Variable = var_name,
    `Non N` = gs$N[gs$livestream_flag == 0], `Non Mean` = gs$Mean[gs$livestream_flag == 0], `Non SD` = gs$SD[gs$livestream_flag == 0],
    `Part N` = gs$N[gs$livestream_flag == 1], `Part Mean` = gs$Mean[gs$livestream_flag == 1], `Part SD` = gs$SD[gs$livestream_flag == 1],
    `Mean diff` = `Part Mean` - `Non Mean`, `t` = as.numeric(tr$statistic), `p value` = tr$p.value)
}
ttest_swot_live <- purrr::map_dfr(swot_compare_vars, ttest_one_var) %>%
  dplyr::mutate(Stars = add_stars(`p value`),
                dplyr::across(c(`Non Mean`,`Non SD`,`Part Mean`,`Part SD`,`Mean diff`,`t`), ~round(., 3)),
                `p value` = round(`p value`, 4))

tab_live_strategy <- table(df$livestream_flag, df$Strategy_Factor)
live_strategy_count <- as.data.frame.matrix(tab_live_strategy) %>% tibble::rownames_to_column("livestream_flag")
live_strategy_rowpct <- round(prop.table(tab_live_strategy, 1) * 100, 2) %>% as.data.frame.matrix() %>% tibble::rownames_to_column("livestream_flag")
chisq_ls <- chisq.test(tab_live_strategy)
chi_ls_summary <- tibble::tibble(Test = "Chi-square", Variable = "Strategy x participation",
                                 Statistic = round(as.numeric(chisq_ls$statistic), 3), df = as.numeric(chisq_ls$parameter),
                                 `p value` = round(chisq_ls$p.value, 4), Stars = add_stars(chisq_ls$p.value))
std_resid_ls <- as.data.frame(as.table(chisq_ls$stdres))
names(std_resid_ls) <- c("Livestream_flag", "Strategy_type", "Std_residual")
std_resid_ls <- std_resid_ls %>% dplyr::mutate(Std_residual = round(Std_residual, 3)) %>% dplyr::arrange(dplyr::desc(abs(Std_residual)))
cramers_ls <- tibble::tibble(Test = "Cramer's V", Variable = "Strategy x participation", Value = round(calc_cramers_v(tab_live_strategy), 4))

# =========================================================
# Step 7. Adoption logit models (full sample)
# =========================================================
df_model <- df %>%
  dplyr::mutate(gender = factor(gender), main_sales_route = factor(main_sales_route),
                Funding_Sources = factor(Funding_Sources), livestream_flag = as.integer(livestream_flag),
                Strategy_Factor = factor(Strategy_Factor, levels = c("WT","WO","ST","SO")),
                age = as.numeric(age), education = as.numeric(education), farm_size = as.numeric(farm_size), Labor = as.numeric(Labor)) %>%
  dplyr::select(livestream_flag, S_index, W_weakness_index, O_index, T_threat_index,
                Internal_diff, External_diff, Strategy_Factor, age, gender, education, farm_size,
                main_sales_route, Funding_Sources, Labor)

m_logit_A_base <- glm(livestream_flag ~ S_index + W_weakness_index + O_index + T_threat_index, df_model, family = binomial("logit"))
m_logit_A_ctrl <- glm(livestream_flag ~ S_index + W_weakness_index + O_index + T_threat_index + age + gender + education + Labor, df_model, family = binomial("logit"))
m_logit_B_base <- glm(livestream_flag ~ Internal_diff + External_diff, df_model, family = binomial("logit"))
m_logit_B_ctrl <- glm(livestream_flag ~ Internal_diff + External_diff + age + gender + education + Labor, df_model, family = binomial("logit"))
m_logit_C_base <- glm(livestream_flag ~ Strategy_Factor, df_model, family = binomial("logit"))
m_logit_C_ctrl <- glm(livestream_flag ~ Strategy_Factor + age + gender + education + Labor, df_model, family = binomial("logit"))

model_list_logit <- list(A_baseline = m_logit_A_base, A_controlled = m_logit_A_ctrl,
                         B_baseline = m_logit_B_base, B_controlled = m_logit_B_ctrl,
                         C_baseline = m_logit_C_base, C_controlled = m_logit_C_ctrl)

coef_map_logit <- c("(Intercept)" = "Constant", "S_index" = "Strength index", "W_weakness_index" = "Weakness index",
                    "O_index" = "Opportunity index", "T_threat_index" = "Threat index",
                    "Internal_diff" = "Internal balance (S - W)", "External_diff" = "External balance (O - T)",
                    "Strategy_FactorWO" = "WO type (ref. WT)", "Strategy_FactorST" = "ST type (ref. WT)", "Strategy_FactorSO" = "SO type (ref. WT)",
                    "age" = "Age", "gender1" = "Gender", "education" = "Education", "Labor" = "Labor")

gof_map_glm <- tibble::tribble(~raw, ~clean, ~fmt, "nobs", "N", 0, "AIC", "AIC", 2, "BIC", "BIC", 2)
mcfadden_values <- sapply(model_list_logit, calc_mcfadden)
add_rows_logit <- tibble::tribble(
  ~term, ~A_baseline, ~A_controlled, ~B_baseline, ~B_controlled, ~C_baseline, ~C_controlled,
  "Controls","No","Yes","No","Yes","No","Yes",
  "Reference group","","","","","WT","WT",
  "McFadden pseudo R2", sprintf("%.3f", mcfadden_values["A_baseline"]), sprintf("%.3f", mcfadden_values["A_controlled"]),
  sprintf("%.3f", mcfadden_values["B_baseline"]), sprintf("%.3f", mcfadden_values["B_controlled"]),
  sprintf("%.3f", mcfadden_values["C_baseline"]), sprintf("%.3f", mcfadden_values["C_controlled"]))

table_logit_df <- modelsummary::modelsummary(model_list_logit, coef_map = coef_map_logit, gof_map = gof_map_glm,
                                             add_rows = add_rows_logit, stars = star_rule, statistic = "({std.error})", estimate = "{estimate}{stars}", output = "data.frame")
table_logit_or <- modelsummary::modelsummary(model_list_logit, coef_map = coef_map_logit, gof_map = gof_map_glm,
                                             add_rows = add_rows_logit, stars = star_rule, exponentiate = TRUE, statistic = "[{conf.low}, {conf.high}]", estimate = "{estimate}{stars}", output = "data.frame")
mcfadden_logit <- tibble::tibble(Model = names(model_list_logit), McFadden_pseudo_R2 = round(as.numeric(mcfadden_values), 3))

rep_grid <- data.frame(
  Strategy_Factor = factor(c("WT","WO","ST","SO"), levels = c("WT","WO","ST","SO")),
  age = mean(df_model$age, na.rm = TRUE),
  gender = factor(levels(df_model$gender)[1], levels = levels(df_model$gender)),
  education = mean(df_model$education, na.rm = TRUE),
  Labor = mean(df_model$Labor, na.rm = TRUE))
pp_link <- predict(m_logit_C_ctrl, newdata = rep_grid, type = "link", se.fit = TRUE)
pred_prob_strategy <- tibble::tibble(
  Strategy_type = as.character(rep_grid$Strategy_Factor),
  Pred_prob = round(plogis(pp_link$fit), 4),
  CI_low = round(plogis(pp_link$fit - 1.96 * pp_link$se.fit), 4),
  CI_high = round(plogis(pp_link$fit + 1.96 * pp_link$se.fit), 4),
  Note = "Adjusted: age/education/Labor at mean, gender at reference level")

# =========================================================
# Step 8. External perception models (village FE)
# =========================================================
df_ext <- df %>%
  dplyr::mutate(village_id = factor(village_id), gender = factor(gender),
                age = as.numeric(age), education = as.numeric(education), farm_size = as.numeric(farm_size), Labor = as.numeric(Labor)) %>%
  dplyr::select(village_id, O_index, T_threat_index, External_diff, S_index, W_weakness_index,
                age, gender, education, farm_size, Labor)

m_O_A <- lm(O_index ~ age + gender + education + Labor + village_id, df_ext)
m_O_B <- lm(O_index ~ age + gender + education + Labor + S_index + W_weakness_index + village_id, df_ext)
m_T_A <- lm(T_threat_index ~ age + gender + education + Labor + village_id, df_ext)
m_T_B <- lm(T_threat_index ~ age + gender + education + Labor + S_index + W_weakness_index + village_id, df_ext)
m_E_A <- lm(External_diff ~ age + gender + education + Labor + village_id, df_ext)
m_E_B <- lm(External_diff ~ age + gender + education + Labor + S_index + W_weakness_index + village_id, df_ext)

external_full_results <- dplyr::bind_rows(
  tidy_lm_result(m_O_A, "Model A", "O_index"), tidy_lm_result(m_O_B, "Model B", "O_index"),
  tidy_lm_result(m_T_A, "Model A", "T_threat_index"), tidy_lm_result(m_T_B, "Model B", "T_threat_index"),
  tidy_lm_result(m_E_A, "Model A", "External_diff"), tidy_lm_result(m_E_B, "Model B", "External_diff"))

external_model_summary <- dplyr::bind_rows(
  broom::glance(m_O_A) %>% dplyr::mutate(outcome = "O_index", model = "A"),
  broom::glance(m_O_B) %>% dplyr::mutate(outcome = "O_index", model = "B"),
  broom::glance(m_T_A) %>% dplyr::mutate(outcome = "T_threat_index", model = "A"),
  broom::glance(m_T_B) %>% dplyr::mutate(outcome = "T_threat_index", model = "B"),
  broom::glance(m_E_A) %>% dplyr::mutate(outcome = "External_diff", model = "A"),
  broom::glance(m_E_B) %>% dplyr::mutate(outcome = "External_diff", model = "B")) %>%
  dplyr::select(outcome, model, nobs, r.squared, adj.r.squared, statistic, p.value, AIC, BIC)

# =========================================================
# Step 9. Strategy x performance cross-tabs (participants)
# =========================================================
df_live_tab <- df %>% dplyr::filter(livestream_flag == 1) %>%
  dplyr::mutate(Strategy_Factor = droplevels(factor(Strategy_Factor)),
                income_total = droplevels(factor(income_total)), Live_share = droplevels(factor(Live_share)))
tab_strategy_income <- table(df_live_tab$Strategy_Factor, df_live_tab$income_total)
tab_strategy_share  <- table(df_live_tab$Strategy_Factor, df_live_tab$Live_share)
strategy_income_count  <- as.data.frame.matrix(tab_strategy_income) %>% tibble::rownames_to_column("Strategy_type")
strategy_income_rowpct <- round(prop.table(tab_strategy_income, 1) * 100, 2) %>% as.data.frame.matrix() %>% tibble::rownames_to_column("Strategy_type")
strategy_share_count   <- as.data.frame.matrix(tab_strategy_share) %>% tibble::rownames_to_column("Strategy_type")
strategy_share_rowpct  <- round(prop.table(tab_strategy_share, 1) * 100, 2) %>% as.data.frame.matrix() %>% tibble::rownames_to_column("Strategy_type")
ci <- chisq.test(tab_strategy_income); cs <- chisq.test(tab_strategy_share)
ci_sim <- chisq.test(tab_strategy_income, simulate.p.value = TRUE, B = 10000); cs_sim <- chisq.test(tab_strategy_share, simulate.p.value = TRUE, B = 10000)
strategy_chi_summary <- tibble::tibble(
  Test = c("Chi-square","Chi-square simulated","Chi-square","Chi-square simulated"),
  Outcome = c("Total income","Total income","Livestream sales share","Livestream sales share"),
  Statistic = c(round(as.numeric(ci$statistic),3), NA, round(as.numeric(cs$statistic),3), NA),
  df = c(as.numeric(ci$parameter), NA, as.numeric(cs$parameter), NA),
  `p value` = round(c(ci$p.value, ci_sim$p.value, cs$p.value, cs_sim$p.value), 4),
  Cramers_V = round(c(calc_cramers_v(tab_strategy_income), NA, calc_cramers_v(tab_strategy_share), NA), 4)) %>%
  dplyr::mutate(Stars = add_stars(`p value`))

# =========================================================
# Step 10. Ordered logit performance models (participants)
# =========================================================
df_live_model <- df %>% dplyr::filter(livestream_flag == 1) %>%
  dplyr::mutate(gender = factor(gender), Strategy_Factor = factor(Strategy_Factor, levels = c("WT","WO","ST","SO")),
                income_total = ordered(income_total), Live_share = ordered(Live_share),
                age = as.numeric(age), education = as.numeric(education), farm_size = as.numeric(farm_size), Labor = as.numeric(Labor))

pol <- function(f, d = df_live_model, m = "logistic") MASS::polr(as.formula(f), data = d, method = m, Hess = TRUE)
m_inc_C_base <- pol("income_total ~ Strategy_Factor"); m_inc_C_ctrl <- pol("income_total ~ Strategy_Factor + age + gender + education + Labor")
m_share_C_base <- pol("Live_share ~ Strategy_Factor"); m_share_C_ctrl <- pol("Live_share ~ Strategy_Factor + age + gender + education + Labor")
m_inc_A_base <- pol("income_total ~ S_index + W_weakness_index + O_index + T_threat_index"); m_inc_A_ctrl <- pol("income_total ~ S_index + W_weakness_index + O_index + T_threat_index + age + gender + education + Labor")
m_share_A_base <- pol("Live_share ~ S_index + W_weakness_index + O_index + T_threat_index"); m_share_A_ctrl <- pol("Live_share ~ S_index + W_weakness_index + O_index + T_threat_index + age + gender + education + Labor")
m_inc_B_base <- pol("income_total ~ Internal_diff + External_diff"); m_inc_B_ctrl <- pol("income_total ~ Internal_diff + External_diff + age + gender + education + Labor")
m_share_B_base <- pol("Live_share ~ Internal_diff + External_diff"); m_share_B_ctrl <- pol("Live_share ~ Internal_diff + External_diff + age + gender + education + Labor")

ordered_logit_coef <- dplyr::bind_rows(
  fmt_polr_table(m_inc_A_base,"A_baseline","income_total"), fmt_polr_table(m_inc_A_ctrl,"A_controlled","income_total"),
  fmt_polr_table(m_inc_B_base,"B_baseline","income_total"), fmt_polr_table(m_inc_B_ctrl,"B_controlled","income_total"),
  fmt_polr_table(m_inc_C_base,"C_baseline","income_total"), fmt_polr_table(m_inc_C_ctrl,"C_controlled","income_total"),
  fmt_polr_table(m_share_A_base,"A_baseline","Live_share"), fmt_polr_table(m_share_A_ctrl,"A_controlled","Live_share"),
  fmt_polr_table(m_share_B_base,"B_baseline","Live_share"), fmt_polr_table(m_share_B_ctrl,"B_controlled","Live_share"),
  fmt_polr_table(m_share_C_base,"C_baseline","Live_share"), fmt_polr_table(m_share_C_ctrl,"C_controlled","Live_share"))
ordered_logit_or <- dplyr::bind_rows(
  polr_or_ci_table(m_inc_A_ctrl,"A_controlled","income_total"), polr_or_ci_table(m_inc_B_ctrl,"B_controlled","income_total"), polr_or_ci_table(m_inc_C_ctrl,"C_controlled","income_total"),
  polr_or_ci_table(m_share_A_ctrl,"A_controlled","Live_share"), polr_or_ci_table(m_share_B_ctrl,"B_controlled","Live_share"), polr_or_ci_table(m_share_C_ctrl,"C_controlled","Live_share"))

# =========================================================
# Step 11. Internal S/W item-level decomposition (participants)
# =========================================================
df_live_items <- df %>% dplyr::filter(livestream_flag == 1) %>%
  dplyr::mutate(gender = factor(gender), income_total_ordered = ordered(income_total), Live_share_ordered = ordered(Live_share),
                age = as.numeric(age), education = as.numeric(education), Labor = as.numeric(Labor))
fI <- "+ age + gender + education + Labor"
m_S_income_polr  <- pol(paste("income_total_ordered ~ Prod_Quality + Tech_Level + Comm_Skill + Price_Comp", fI), df_live_items)
m_W_income_polr  <- pol(paste("income_total_ordered ~ Biz_Scale + Financial + Live_Exp + Device_Diff", fI), df_live_items)
m_SW_income_polr <- pol(paste("income_total_ordered ~ Prod_Quality + Tech_Level + Comm_Skill + Price_Comp + Biz_Scale + Financial + Live_Exp + Device_Diff", fI), df_live_items)
m_S_share_polr   <- pol(paste("Live_share_ordered ~ Prod_Quality + Tech_Level + Comm_Skill + Price_Comp", fI), df_live_items)
m_W_share_polr   <- pol(paste("Live_share_ordered ~ Biz_Scale + Financial + Live_Exp + Device_Diff", fI), df_live_items)
m_SW_share_polr  <- pol(paste("Live_share_ordered ~ Prod_Quality + Tech_Level + Comm_Skill + Price_Comp + Biz_Scale + Financial + Live_Exp + Device_Diff", fI), df_live_items)
SW_items_ordered_coef <- dplyr::bind_rows(
  fmt_polr_table(m_S_income_polr,"S_items","income_total"), fmt_polr_table(m_W_income_polr,"W_items","income_total"), fmt_polr_table(m_SW_income_polr,"S_W_items","income_total"),
  fmt_polr_table(m_S_share_polr,"S_items","Live_share"), fmt_polr_table(m_W_share_polr,"W_items","Live_share"), fmt_polr_table(m_SW_share_polr,"S_W_items","Live_share")) %>%
  dplyr::filter(!grepl("\\|", term))
SW_item_correlations <- df_live_items %>%
  dplyr::mutate(income_total_num = as.numeric(income_total), Live_share_num = as.numeric(Live_share)) %>%
  dplyr::select(income_total_num, Live_share_num, Prod_Quality, Tech_Level, Comm_Skill, Price_Comp, Biz_Scale, Financial, Live_Exp, Device_Diff) %>%
  stats::cor(use = "pairwise.complete.obs") %>% as.data.frame() %>% tibble::rownames_to_column("Variable")

# =========================================================
# Step 12. Construct-overlap robustness: Internal full vs clean
# =========================================================
# [MODIFIED] clean = Financial removed only; Biz_Scale remains as W item.
ctrlD <- "+ External_diff + age + gender + education + Labor"
m_inc_full_ahp  <- pol(paste("income_total ~ Internal_diff", ctrlD), df_live_model)
m_inc_clean_ahp <- pol(paste("income_total ~ Internal_diff_clean", ctrlD), df_live_model)
m_inc_full_eq   <- pol(paste("income_total ~ Internal_eq_full", ctrlD), df_live_model)
m_inc_clean_eq  <- pol(paste("income_total ~ Internal_eq_clean", ctrlD), df_live_model)
m_shr_full_ahp  <- pol(paste("Live_share ~ Internal_diff", ctrlD), df_live_model)
m_shr_clean_ahp <- pol(paste("Live_share ~ Internal_diff_clean", ctrlD), df_live_model)
m_shr_full_eq   <- pol(paste("Live_share ~ Internal_eq_full", ctrlD), df_live_model)
m_shr_clean_eq  <- pol(paste("Live_share ~ Internal_eq_clean", ctrlD), df_live_model)
overlap_diag <- dplyr::bind_rows(
  fmt_polr_table(m_inc_full_ahp,"AHP_full","income_total"), fmt_polr_table(m_inc_clean_ahp,"AHP_clean","income_total"),
  fmt_polr_table(m_inc_full_eq,"Equal_full","income_total"), fmt_polr_table(m_inc_clean_eq,"Equal_clean","income_total"),
  fmt_polr_table(m_shr_full_ahp,"AHP_full","Live_share"), fmt_polr_table(m_shr_clean_ahp,"AHP_clean","Live_share"),
  fmt_polr_table(m_shr_full_eq,"Equal_full","Live_share"), fmt_polr_table(m_shr_clean_eq,"Equal_clean","Live_share")) %>%
  dplyr::filter(!grepl("\\|", term))

# =========================================================
# Step 13. Robustness: probit, ordered probit, exclude ST
# =========================================================
m_probit_A_ctrl <- glm(livestream_flag ~ S_index + W_weakness_index + O_index + T_threat_index + age + gender + education + Labor, df_model, family = binomial("probit"))
m_probit_B_ctrl <- glm(livestream_flag ~ Internal_diff + External_diff + age + gender + education + Labor, df_model, family = binomial("probit"))
m_probit_C_ctrl <- glm(livestream_flag ~ Strategy_Factor + age + gender + education + Labor, df_model, family = binomial("probit"))
table_probit_df <- modelsummary::modelsummary(
  list(A_controlled = m_probit_A_ctrl, B_controlled = m_probit_B_ctrl, C_controlled = m_probit_C_ctrl),
  coef_map = coef_map_logit, gof_map = gof_map_glm, stars = star_rule,
  statistic = "({std.error})", estimate = "{estimate}{stars}", output = "data.frame")

m_inc_C_oprobit  <- pol("income_total ~ Strategy_Factor + age + gender + education + Labor", df_live_model, "probit")
m_share_C_oprobit<- pol("Live_share ~ Strategy_Factor + age + gender + education + Labor", df_live_model, "probit")
m_inc_B_oprobit  <- pol("income_total ~ Internal_diff + External_diff + age + gender + education + Labor", df_live_model, "probit")
m_share_B_oprobit<- pol("Live_share ~ Internal_diff + External_diff + age + gender + education + Labor", df_live_model, "probit")
oprobit_coef <- dplyr::bind_rows(
  fmt_polr_table(m_inc_C_oprobit,"C_controlled","income_total"), fmt_polr_table(m_share_C_oprobit,"C_controlled","Live_share"),
  fmt_polr_table(m_inc_B_oprobit,"B_controlled","income_total"), fmt_polr_table(m_share_B_oprobit,"B_controlled","Live_share")) %>%
  dplyr::filter(!grepl("\\|", term))

df_live_noST <- df %>% dplyr::filter(livestream_flag == 1, Strategy_Factor != "ST") %>%
  dplyr::mutate(Strategy_Factor = droplevels(factor(Strategy_Factor, levels = c("WT","WO","SO"))), gender = factor(gender),
                income_total = ordered(income_total), Live_share = ordered(Live_share),
                age = as.numeric(age), education = as.numeric(education), Labor = as.numeric(Labor))
m_inc_noST   <- pol("income_total ~ Strategy_Factor + age + gender + education + Labor", df_live_noST)
m_share_noST <- pol("Live_share ~ Strategy_Factor + age + gender + education + Labor", df_live_noST)
noST_coef <- dplyr::bind_rows(
  fmt_polr_table(m_inc_noST,"noST_logit","income_total"), fmt_polr_table(m_share_noST,"noST_logit","Live_share")) %>%
  dplyr::filter(!grepl("\\|", term))

# =========================================================
# Step 14. Common method bias — Harman single-factor (PCA on 16 items)
# =========================================================
harman_mat <- as.matrix(df[, all_swot_vars])
harman_mat <- harman_mat[stats::complete.cases(harman_mat), , drop = FALSE]
harman_pca <- prcomp(harman_mat, scale. = TRUE)
harman_var <- (harman_pca$sdev^2) / sum(harman_pca$sdev^2)
harman_tbl <- tibble::tibble(
  Item = c("Method", "First component variance %", "Threshold", "Conclusion"),
  Content = c("Harman single-factor test (PCA on 16 SWOT self-report items)",
              sprintf("%.2f%%", harman_var[1] * 100),
              "First factor < 50% suggests common method bias is not severe",
              ifelse(harman_var[1] < 0.5, "First component < 50%: CMB not a major concern",
                     "First component >= 50%: potential CMB; interpret with caution")))

# =========================================================
# Step 15. SEM / path analysis
# =========================================================
df_semF <- df %>%
  dplyr::transmute(age = as.numeric(age), gender_num = as.numeric(as.character(gender)),
                   education = as.numeric(education), Labor = as.numeric(Labor),
                   INS = as.numeric(scale(Internal_diff)), ENO = as.numeric(scale(External_diff)),
                   livestream_flag_num = as.numeric(livestream_flag),
                   income_total_num = as.numeric(income_total), Live_share_num = as.numeric(Live_share)) %>%
  tidyr::drop_na()
sem_F <- '
  INS ~ age + gender_num + education + Labor
  ENO ~ age + gender_num + education + Labor
  livestream_flag_num ~ INS + ENO
  income_total_num    ~ INS + ENO
  Live_share_num      ~ INS + ENO
  INS ~~ ENO
  income_total_num ~~ Live_share_num
'
fit_F <- lavaan::sem(sem_F, data = df_semF, estimator = "MLR", meanstructure = TRUE, fixed.x = FALSE, auto.cov.y = FALSE)
sem_F_fit   <- tibble::enframe(lavaan::fitMeasures(fit_F, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), name = "Fit_index", value = "Value") %>% dplyr::mutate(Value = round(Value, 4))
sem_F_paths <- lavaan::standardizedSolution(fit_F) %>% dplyr::filter(op == "~") %>%
  dplyr::transmute(lhs, rhs, est.std = round(est.std,3), se = round(se,3), z = round(z,3), pvalue = round(pvalue,4), stars = add_stars(pvalue))

df_semB <- df %>% dplyr::filter(livestream_flag == 1) %>%
  dplyr::transmute(age = as.numeric(age), gender_num = as.numeric(as.character(gender)),
                   education = as.numeric(education), Labor = as.numeric(Labor),
                   INS = as.numeric(scale(Internal_diff)), ENO = as.numeric(scale(External_diff)),
                   income_total_ord = ordered(income_total), Live_share_ord = ordered(Live_share)) %>%
  tidyr::drop_na()
sem_B <- '
  INS ~ age + gender_num + education + Labor
  ENO ~ age + gender_num + education + Labor
  income_total_ord ~ INS + ENO
  Live_share_ord   ~ INS + ENO
  INS ~~ ENO
  income_total_ord ~~ Live_share_ord
'
fit_B <- lavaan::sem(sem_B, data = df_semB, ordered = c("income_total_ord","Live_share_ord"), estimator = "WLSMV")
sem_B_fit   <- tibble::enframe(lavaan::fitMeasures(fit_B, c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled","srmr")), name = "Fit_index", value = "Value") %>% dplyr::mutate(Value = round(Value, 4))
sem_B_paths <- lavaan::standardizedSolution(fit_B) %>% dplyr::filter(op == "~") %>%
  dplyr::transmute(lhs, rhs, est.std = round(est.std,3), se = round(se,3), z = round(z,3), pvalue = round(pvalue,4), stars = add_stars(pvalue))

# =========================================================
# Step 16. Export everything to ONE Excel (Chinese sheet names)
# =========================================================
wb <- openxlsx::createWorkbook()

write_three_line_sheet(wb, "00_说明", tibble::tibble(
  Item = c("评分数据文件","最终结果文件","显著性标准","W/T原始题项","主回归W/T定义","内部清洁指数","SEM说明"),
  Content = c("strategy_data_with_SWOT_scores.xlsx","SWOT_AHP_final_all_results.xlsx",
              "* p<0.05, ** p<0.01, *** p<0.001",
              "原始W/T题项:数值越高=条件越好",
              "主回归用 W_weakness_index / T_threat_index(越高=弱势/威胁越强)",
              "Internal_diff_clean = S_index - (剔除Financial后反向并重新归一的weakness；Biz_Scale仍保留为W变量)",  # ← MODIFIED
              "15A为全样本路径(拟合差、Live_share受参与门槛污染);15B为参与者条件版(推荐),std.all为潜变量尺度")))
write_three_line_sheet(wb, "01_AHP权重表", w2 %>% dplyr::select(Indicators, variable, SWOT_group, order_id, dplyr::any_of(c("Guidelines","Guideline_Weights","Indicator_Weights","Combined_Weights"))))
write_three_line_sheet(wb, "02_SWOT指数描述", index_stats)
write_three_line_sheet(wb, "03_农户数值变量", desc_farmer_numeric)
write_three_line_sheet(wb, "04_全部分类变量", desc_categorical_all)
write_three_line_sheet(wb, "05_直播参与分布", desc_livestream)
write_three_line_sheet(wb, "06_SWOT类型分布", strategy_dist)
write_three_line_sheet(wb, "07_参与差异描述", desc_swot_by_live)
write_three_line_sheet(wb, "08_参与差异t检验", ttest_swot_live)
write_three_line_sheet(wb, "09_参与类型数量", live_strategy_count)
write_three_line_sheet(wb, "10_参与类型比例", live_strategy_rowpct)
write_three_line_sheet(wb, "11_参与类型卡方", chi_ls_summary)
write_three_line_sheet(wb, "12_卡方标准残差", std_resid_ls)
write_three_line_sheet(wb, "13_CramersV", cramers_ls)
write_three_line_sheet(wb, "14_参与Logit系数", table_logit_df)
write_three_line_sheet(wb, "15_参与LogitOR", table_logit_or)
write_three_line_sheet(wb, "16_Logit拟合度", mcfadden_logit)
write_three_line_sheet(wb, "17_类型预测参与概率", pred_prob_strategy, note = "Predicted participation probability by SWOT strategy type (from controlled logit C).")
write_three_line_sheet(wb, "18_外部评价回归", external_full_results)
write_three_line_sheet(wb, "19_外部模型汇总", external_model_summary)
write_three_line_sheet(wb, "20_类型收入数量", strategy_income_count)
write_three_line_sheet(wb, "21_类型收入比例", strategy_income_rowpct)
write_three_line_sheet(wb, "22_类型占比数量", strategy_share_count)
write_three_line_sheet(wb, "23_类型占比比例", strategy_share_rowpct)
write_three_line_sheet(wb, "24_类型绩效卡方", strategy_chi_summary)
write_three_line_sheet(wb, "25_绩效有序Logit系数", ordered_logit_coef)
write_three_line_sheet(wb, "26_绩效有序LogitOR", ordered_logit_or)
write_three_line_sheet(wb, "27_SW项目分解", SW_items_ordered_coef, note = "Item-level decomposition (participants). Raw W items, not reversed.")
write_three_line_sheet(wb, "28_SW项目相关矩阵", SW_item_correlations)
write_three_line_sheet(wb, "29_内部指数重叠诊断", overlap_diag,
  note = "Internal_diff full vs clean (Financial removed only; Biz_Scale remains as W item, reverse-coded). Controls: External_diff, age, gender, education, Labor. Participants only.")  # ← MODIFIED
write_three_line_sheet(wb, "30_稳健Probit参与", table_probit_df)
write_three_line_sheet(wb, "31_稳健OProbit绩效", oprobit_coef)
write_three_line_sheet(wb, "32_排除ST绩效Logit", noST_coef)
write_three_line_sheet(wb, "33_共同方法偏误Harman", harman_tbl)
write_three_line_sheet(wb, "34_SEM全样本拟合", sem_F_fit, note = "Full-sample path model (MLR, numeric outcomes). Fit is poor; Live_share path contaminated by participation hurdle. Superseded by 36/37.")
write_three_line_sheet(wb, "35_SEM全样本路径", sem_F_paths)
write_three_line_sheet(wb, "36_SEM参与者拟合", sem_B_fit, note = "Participant-subsample conditional path model (WLSMV, ordered outcomes). std.all is latent-scale. Report fit honestly (SRMR vs CFI/TLI/RMSEA).")
write_three_line_sheet(wb, "37_SEM参与者路径", sem_B_paths, note = "income path may be inflated by scale/finance overlap; see sheet 29 for the cleaned-index check.")

openxlsx::saveWorkbook(wb, file = file_results, overwrite = TRUE)
cat("\nAll results saved to:\n", file_results, "\n")
