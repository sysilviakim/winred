source(here::here("R", "utilities.R"))

# Load datasets ================================================================

load(here("data", "raw", "panel_filtered.Rda"))
load(here("data", "raw", "corp_itemized.Rda"))
df <- corp_itemized

## Delete zero variance
df <- df %>%
  select(
    -{
      df %>%
        map_dbl(~ length(table(.x, useNA = "ifany"))) %>%
        {which(. == 1)} %>%
        names()
    }
  )

df <- df %>%
  mutate(
    transaction_dt = mdy(transaction_dt),
    transaction_amt = as.numeric(transaction_amt),
    rpt = paste0("Q", quarter(transaction_dt), "_", year(transaction_dt))
  ) %>%
  select(rpt, transaction_dt, everything())

intersect(names(df), names(df_ls$full))

df_summ <- df %>%
  # select(-transaction_dt, -state, -active) %>%
  mutate(
    org_tp = case_when(
      org_tp == "CRP" | org_tp == "W" | org_tp == "T" ~ "C",
      TRUE ~ org_tp
    )
  ) %>%
  group_by(cand_id, rpt, org_tp) %>%
  summarise(amt = sum(transaction_amt, na.rm = TRUE)) %>%
  pivot_wider(
    id_cols = c("cand_id", "rpt"),
    names_from = "org_tp", values_from = "amt"
  )

# Match to PanelMatch prepped data =============================================
df_ls <- df_ls %>%
  map(
    ~ left_join(.x, df_summ) %>%
      select(-state, everything()) %>%
      mutate(across(c("C", "L"), ~ case_when(is.na(.x) ~ 0, TRUE ~ .x))) %>%
      rename(corp = C, labor = L) %>%
      mutate(corp_all = corp + labor) %>%
      group_by(cand_id) %>%
      arrange(cand_id, rpt_int) %>%
      rowwise() %>%
      mutate(
        corp_log = log(max(corp, 0) + 1),
        labor_log = log(max(labor, 0) + 1),
        corp_all_log = log(max(corp_all, 0) + 1)
      ) %>%
      ungroup() %>%
      group_by(cand_id) %>%
      arrange(cand_id, rpt_int) %>%
      mutate(
        diff_corp = corp - lag(corp),
        diff_corp_log = corp_log - lag(corp_log),
        diff_labor = labor - lag(labor),
        diff_labor_log = labor_log - lag(labor_log),
        diff_corp_all = corp_all - lag(corp_all),
        diff_corp_all_log = corp_all_log - lag(corp_all_log)
      ) %>%
      select(
        office, state_cd, cand_id, treated, treated_sum, rpt,
        last_name, first_name, corp, labor, corp_all, everything()
      ) %>%
      as.data.frame()
  )

# The usual PanelMatch =========================================================
df_ls_orig <- df_ls[c("full", "senate", "house", "inc")]
pm_list <- list()
for (min_rpt in c(0, 16, 17, 18, 19)) {
  df_ls <- minrpt_filter(df_ls_orig, min_rpt = min_rpt)
  
  ## Plot a sample of random 150 candidates ====================================
  df <- df_ls$full %>% mutate(cand_id = as.integer(factor(cand_id)) + 1)
  for (dv in "corp") {
    obj_name <- paste0("pm_", dv, "_log_minrpt_", min_rpt)
    fname <- here("output", paste0(obj_name, ".Rda"))
    pm <- df_ls %>%
      imap(
        ~ PanelMatch_short(
          .x,
          paste0(dv, "_log"),
          cov_bench(
            dv = paste0("diff_", dv, "_log"),
            xvar = xvar
          )
        )
      )
    save(pm, file = fname)
    
    ## Covariate balance =======================================================
    pm_list[[obj_name]]$covbal <- pm %>%
      imap(
        ~ covbal_short(
          .x, df_ls[[.y]],
          plot = FALSE,
          diffdv = paste0("diff_", dv, "_log")
        )
      )
    
    ## ATT export and visualization ============================================
    pm_list[[obj_name]]$att_summ <- pm %>% imap(~ summary(.x$est)$summary)
  }
}
save(pm_list, file = here("output", "pm_corp_list.Rda"))

# Covariate balance evaluation =================================================
load(here("output", "pm_corp_list.Rda"))

## Figure I.4, formerly pm_corp_full_log_varying.pdf
pdf(here("fig", "FigI4.pdf"), width = 4.8, height = 3.5)
print(ggPM_var("pm_corp_log_minrpt_", ylim = c(-2, 2), breaks = 1))
dev.off()

file.remove(list.files(here("fig"), pattern = "corp_", full.names = TRUE))
file.remove(list.files(here("tab"), pattern = "corp_", full.names = TRUE))
