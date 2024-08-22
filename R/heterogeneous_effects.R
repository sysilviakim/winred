source(here::here("R", "utilities.R"))

# Import data ==================================================================
load(here("data", "raw", "panel_filtered.Rda"))
df_ls_orig <- df_ls[c("full", "senate", "house", "inc")]

# Estimation ===================================================================
pm_list <- list()
for (min_rpt in c(0, 16, 17, 18, 19)) {
  print(paste0("----- min_rpt == ", min_rpt, " -----"))

  first_treated_rpt_int <- df_ls_orig$full %>%
    filter(treated == 1) %$%
    min(rpt_int)

  df_ls_orig <- df_ls_orig %>%
    map(
      ~ .x %>%
        mutate(cand_id = as.integer(factor(cand_id)) + 1) %>%
        group_by(cand_id_int) %>%
        mutate(
          mean_pre_pct_unitem = mean(
            pct_unitem[rpt_int < first_treated_rpt_int],
            na.rm = TRUE
          ),
          mean_pre_pct_indv = mean(
            pct_indv[rpt_int < first_treated_rpt_int],
            na.rm = TRUE
          )
        ) %>%
        ungroup()
    )

  df_ls <- minrpt_filter(df_ls_orig, min_rpt = min_rpt)
  df <- df_ls$full %>%
    select(cand_id, where(is.numeric)) %>%
    select(rpt_int, everything())

  ## % unitemized ==============================================================
  unitem_cutoff <- df %>%
    group_by(cand_id_int) %>%
    slice(1) %$%
    median(mean_pre_pct_unitem, na.rm = TRUE)

  df <- df %>%
    mutate(pre_pct_unitem_high = mean_pre_pct_unitem > unitem_cutoff) %>%
    as.data.frame()

  obj_name <- paste0("pm_hetero_unitemized_log_minrpt_", min_rpt)
  fname <- here("output", paste0(obj_name, ".Rda"))

  pm <- PanelMatch_short(
    df,
    "ttl_log",
    # For some reason this is really necessary
    # for getting rid of an obscure R error msg
    cov_bench(dv = "diff_ttl_log", xvar = xvar),
    moderator = "pre_pct_unitem_high"
  )
  save(pm, file = fname)
  pm_list[[obj_name]] <- list(pm = pm)
  pm_list[[obj_name]]$covbal <-
    covbal_short(pm, df, plot = FALSE, diffdv = "diff_ttl_log")
  pm_list[[obj_name]]$att_summ <- summary(pm$est$`TRUE`)$summary

  ## % indiv ===================================================================
  indv_cutoff <- df %>%
    group_by(cand_id_int) %>%
    slice(1) %$%
    median(mean_pre_pct_indv, na.rm = TRUE)

  df <- df %>%
    mutate(pre_pct_indv_high = mean_pre_pct_indv > indv_cutoff) %>%
    as.data.frame()

  obj_name <- paste0("pm_hetero_indiv_log_minrpt_", min_rpt)
  fname <- here("output", paste0(obj_name, ".Rda"))

  pm <- PanelMatch_short(
    df,
    "ttl_log",
    cov_bench(
      dv = "diff_ttl_log",
      xvar = xvar
      # For some reason this is really necessary
      # for getting rid of an obscure R error msg
    ),
    moderator = "pre_pct_indv_high"
  )
  save(pm, file = fname)
  pm_list[[obj_name]] <- list(pm = pm)
  pm_list[[obj_name]]$covbal <-
    covbal_short(pm, df, plot = FALSE, diffdv = "diff_ttl_log")
  pm_list[[obj_name]]$att_summ <- summary(pm$est$`TRUE`)$summary

  ## % gender ==================================================================
  obj_name <- paste0("pm_hetero_gender_log_minrpt_", min_rpt)
  fname <- here("output", paste0(obj_name, ".Rda"))

  pm <- PanelMatch_short(
    df,
    "ttl_log",
    cov_bench(
      dv = "diff_ttl_log",
      xvar = xvar
      # For some reason this is really necessary
      # for getting rid of an obscure R error msg
    ),
    moderator = "gender"
  )
  pm_list[[obj_name]] <- list(pm = pm)
  pm_list[[obj_name]]$covbal <-
    covbal_short(pm, df, plot = FALSE, diffdv = "diff_ttl_log")
  pm_list[[obj_name]]$att_summ <- summary(pm$est$`1`)$summary
}

# Visualization ================================================================
## Figure 7, formerly heterogeneity_unitemized_wrap.pdf
pdf(
  here("fig", paste0("Fig7.pdf")),
  width = 6, height = 3
)
print(
  grid_arrange_shared_legend(
    ggPM_var("pm_hetero_unitemized_log_minrpt_", target = "FALSE") +
      ggtitle("Low Reliance on Unitemized $") +
      scale_y_continuous(limits = c(-4.5, 2), breaks = seq(-4, 2, by = 1)),
    ggPM_var("pm_hetero_unitemized_log_minrpt_", target = "TRUE") +
      ggtitle("High Reliance on Unitemized $") + ylab("") +
      scale_y_continuous(limits = c(-4.5, 2), breaks = seq(-4, 2, by = 1))
  )
)
dev.off()

## Figure 8, formerly heterogeneity_gender_wrap.pdf
pdf(
  here("fig", paste0("Fig8.pdf")),
  width = 6, height = 3
)
print(
  grid_arrange_shared_legend(
    ggPM_var("pm_hetero_gender_log_minrpt_", target = "1") +
      ggtitle("Male Candidates") +
      scale_y_continuous(limits = c(-3.5, 2.5), breaks = seq(-3, 2.5, by = 1)),
    ggPM_var("pm_hetero_gender_log_minrpt_", target = "0") +
      ggtitle("Female Candidates") + ylab("") +
      scale_y_continuous(limits = c(-3.5, 2.5), breaks = seq(-3, 2.5, by = 1))
  )
)
dev.off()
