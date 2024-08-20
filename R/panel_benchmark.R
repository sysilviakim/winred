source(here::here("R", "utilities.R"))

# Import data ==================================================================
load(here("data", "raw", "panel_filtered.Rda"))
df_ls_orig <- df_ls[c("full", "senate", "house", "inc")]
df <- df_ls$full
fm_bench <- as.formula(paste0(yvar, " ~ ", tvar))
yvar_logged <- "ttl_log"

# Basic model: 2-way FE ========================================================
df %<>% # we actually need lagged treatment to be consistent with PanelMatch
  arrange(cand_id, rpt_int) %>%
  group_by(cand_id) %>%
  mutate(
    treated_lagged = lag(treated),
    treated_lead1 = lead(treated, n = 1),
    treated_lead2 = lead(treated, n = 2),
    treated_lead3 = lead(treated, n = 3)
  ) %>%
  ungroup()
class(df) <- "data.frame"
m_2wfe <- fixest_form(y = yvar, x = tvar, fe = c("cand_id", "rpt"))
est_2wfe <- feols(m_2wfe, data = df, cluster = ~cand_id)
print(est_2wfe)

tvar_granger <- c(
  # can't include more leads due to collinearity
  "treated_lead2",
  "treated_lead1",
  "treated"
)
m_2wfe_granger <-
  fixest_form(y = yvar, x = tvar_granger, fe = c("cand_id", "rpt"))
est_2wfe_granger <- feols(m_2wfe_granger, data = df, cluster = ~cand_id)
print(est_2wfe_granger)

setFixest_dict(
  c(
    "treated_lead2" = "t-2",
    "treated_lead1" = "t-1",
    "treated" = "t",
    "ttl" = "Quarterly Fundraising",
    "ttl_log" = "Quarterly Fundraising (Logged)"
  )
)

## Figure I.1(a), formerly 2wfe_granger.pdf
pdf(here("fig", "FigI1a.pdf"), width = 7, height = 4)
coefplot(
  est_2wfe_granger,
  main = "Granger Test for Quarter Fundraising",
  ylim.add = c(-0.2, 0.2)
)
dev.off()

yvar_logged <- "ttl_log"
m_2wfe_logged <-
  fixest_form(y = yvar_logged, x = tvar, fe = c("cand_id", "rpt"))
est_2wfe_logged <- feols(m_2wfe_logged, data = df, cluster = ~cand_id)
print(est_2wfe_logged)

m_2wfe_logged_granger <-
  fixest_form(y = yvar_logged, x = tvar_granger, fe = c("cand_id", "rpt"))
est_2wfe_logged_granger <-
  feols(m_2wfe_logged_granger, data = df, cluster = ~cand_id)
print(est_2wfe_logged_granger)

## Figure I.1(2), formerly 2wfe_logged_granger.pdf
pdf(here("fig", "FigI1b.pdf"), width = 7, height = 4)
coefplot(
  est_2wfe_logged_granger,
  main = "Granger Test for Quarter Fundraising (Logged)",
  ylim.add = c(-0.2, 0.2)
)
dev.off()

setFixest_dict(
  c(
    treated = "WinRed Adoption",
    ttl = "Total Fundraising ($)",
    ttl_log = "Total Fundraising (Logged)",
    cand_id = "Candidate",
    rpt = "Year-Quarter"
  )
)

setFixest_etable(fitstat = ~ n + r2)

tablestyle <- style.tex(
  main = "aer",
  fixef.suffix = " FEs",
  fixef.where = "var",
  fixef.title = "",
  stats.title = "\\midrule",
  tablefoot = FALSE,
  yesNo = "\\checkmark"
)

## Table I.1, formerly treatment_effect_2wfe_baseline.tex
etable(
  est_2wfe, est_2wfe_logged,
  file = here("tab", "TableI1.tex"),
  title = paste0(
    "Two-Way Fixed-Effect Estimates of Quarterly Fundraising Effects",
    " From WinRed Adoption"
  ),
  label = "tab:treatment-effect-2wfe-baseline",
  cluster = ~cand_id,
  drop = "(Intercept)",
  digits = 3,
  digits.stats = 2,
  style.tex = tablestyle,
  replace = TRUE
)
