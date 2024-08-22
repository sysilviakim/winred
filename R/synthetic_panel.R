source(here::here("R", "utilities.R"))

# Import data ==================================================================
load(here("data", "raw", "panel_filtered.Rda"))
df_ls_orig <- df_ls[c("full", "senate", "house", "inc")]
df <- df_ls$full
fm_bench <- as.formula(paste0(yvar, " ~ ", tvar))
yvar_logged <- "ttl_log"

# Loop over min_rpt to generate models under different samples =================
## 19 = Q3 2019
pm_list <- list()
for (min_rpt in c(0, 16, 17, 18, 19)) {
  print(paste0("----- min_rpt == ", min_rpt, " -----"))
  df_ls <- minrpt_filter(df_ls_orig, min_rpt = min_rpt)

  ## Plot a sample of random 150 candidates ====================================
  df <- df_ls$full %>% mutate(cand_id = as.integer(factor(cand_id)) + 1)
  set.seed(123)
  p <- DisplayTreatment(
    unit.id = "cand_id", time.id = "rpt_int", legend.position = "bottom",
    xlab = "Quarters", ylab = "Candidates",
    ## Too many data points; not plotting properly.
    treatment = "treated", data = df %>% filter(cand_id %in% sample(993, 150)),
    ## 5-class RdBu
    color.of.treated = "#ca0020",
    color.of.untreated = "#f4a582"
  )

  pdf(
    here("fig", paste0("display_treatment_150sample_minrpt_", min_rpt, ".pdf")),
    width = 5, height = 4.5
  )
  print(
    plot_notitle(pdf_default(p)) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(family = "CM Roman"),
        legend.position = "bottom"
      )
  )
  dev.off()

  ## For varying y-variables, execute PanelMatch ===============================
  for (dv in c("ttl", "indv", "oth", "item", "unitem")) {
    print(paste0("--- '", dv, "' ---"))
    dv2 <- ifelse(dv == "indv", "indv_ttl", dv)
    obj_name <- paste0("pm_", dv, "_log_minrpt_", min_rpt)
    fname <- here("output", paste0(obj_name, ".Rda"))

    pm <- df_ls %>%
      imap(
        ~ PanelMatch_short(
          .x,
          paste0(dv2, "_log"),
          cov_bench(
            dv = paste0("diff_", dv2, "_log"),
            xvar = xvar
          )
        )
      )
    save(pm, file = fname)
    pm_list[[obj_name]] <- list(pm = pm)

    ## Matched set size ========================================================
    ## Sample size for "senate" is much larger under min_rpt <= 16
    ## Sample size for "inc" is much larger under min_rpt <= 18
    pm_list[[obj_name]]$matching.size <- list()
    for (x in names(df_ls)) {
      matching.size <- summary(pm_list[[x]]$out$att)
      pm_list[[obj_name]]$matching.size[[x]] <- matching.size
    }

    ## Covariate balance =======================================================
    pm_list[[obj_name]]$covbal <- pm %>%
      imap(
        ~ covbal_short(
          .x, df_ls[[.y]],
          plot = FALSE,
          diffdv = paste0("diff_", dv2, "_log")
        )
      )

    p_list <- pm_list[[obj_name]]$covbal %>%
      map(~ ggCB(.x, nrow = 2))

    for (x in names(df_ls)) {
      pdf(
        here("fig", paste0(dv, "_log_", x, "_covbal_minrpt_", min_rpt, ".pdf")),
        width = 4, height = 3.5
      )
      print(
        p_list$full +
          guides(
            colour = guide_legend(nrow = 6),
            linetype = guide_legend(nrow = 6)
          )
      )
      dev.off()
    }

    pdf(
      here("fig", paste0(dv, "_log_wrap_covbal_minrpt_", min_rpt, ".pdf")),
      width = 5, height = 3.5
    )
    print(grid_arrange_shared_legend(
      p_list$full + ggtitle("All Candidates"),
      p_list$inc + ggtitle("Incumbents") + ylab(""),
      p_list$senate + ggtitle("Senate") + ylab(""),
      p_list$house + ggtitle("House") + ylab(""),
      nrow = 2, ncol = 2
    ))
    dev.off()

    ## ATT export and visualization ============================================
    pm_list[[obj_name]]$att_summ <- pm %>% imap(~ summary(.x$est)$summary)

    ## xtable export
    pm_list[[obj_name]]$att_summ %>%
      imap(
        ~ xtable_summary(
          .x, paste0(dv, "_att_", .y, "_minrpt_", min_rpt, ".tex")
        )
      )

    ## visualization
    p_list <- pm %>%
      map(~ ggPM(.x$est, ylim = c(-1, 2), breaks = 1, main = NULL))
    p_list$senate <- ggPM(pm$senate$est, ylim = c(-5, 9), main = NULL)

    pdf(
      here("fig", paste0(dv, "_log_main_minrpt_", min_rpt, ".pdf")),
      width = 3, height = 2.5
    )
    print(pdf_default(p_list$full) + scale_y_continuous(limits = c(-1, 1.5)))
    dev.off()

    pdf(
      here("fig", paste0(dv, "_log_wrap_minrpt_", min_rpt, ".pdf")),
      width = 5, height = 3.5
    )
    print(grid_arrange_shared_axes(
      list = p_list[c("full", "inc", "senate", "house")],
      title = c("All Candidates", "Incumbents", "Senate", "House"),
      nrow = 2, ncol = 2
    ))
    dev.off()
  }

  msets <- pm$full$out$att
  p <- DisplayTreatment(
    data = df_ls$full,
    treatment = "treated", time.id = "rpt_int", unit.id = "cand_id_int",
    xlab = "Quarters", ylab = "Candidates", show.set.only = TRUE,
    matched.set = msets[1]
  )

  pdf(
    here("fig", paste0("matchset_only_1stcand_minrpt_", min_rpt, ".pdf")),
    width = 5, height = 4
  )
  print(
    plot_nolegend(pdf_default(p)) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        text = element_text(family = "CM Roman")
      )
  )
  dev.off()
}
save(pm_list, file = here("output", "pm_list.Rda"))

## Table I.3 -------------------------------------------------------------------
## Rename files externally; otherwise, hard to hardcode
## No truncation
file.rename(
  here("tab", "ttl_att_full_minrpt_0.tex"),
  here("tab", "TableI3a.tex")
)
file.rename(
  here("tab", "indv_att_full_minrpt_0.tex"),
  here("tab", "TableI3b.tex")
)
file.rename(
  here("tab", "oth_att_full_minrpt_0.tex"),
  here("tab", "TableI3c.tex")
)

## Truncated from 2018 Q4
file.rename(
  here("tab", "ttl_att_full_minrpt_16.tex"),
  here("tab", "TableI3d.tex")
)
file.rename(
  here("tab", "indv_att_full_minrpt_16.tex"),
  here("tab", "TableI3e.tex")
)
file.rename(
  here("tab", "oth_att_full_minrpt_16.tex"),
  here("tab", "TableI3f.tex")
)

## Truncated from 2019 Q1
file.rename(
  here("tab", "ttl_att_full_minrpt_17.tex"),
  here("tab", "TableI3g.tex")
)
file.rename(
  here("tab", "indv_att_full_minrpt_17.tex"),
  here("tab", "TableI3h.tex")
)
file.rename(
  here("tab", "oth_att_full_minrpt_17.tex"),
  here("tab", "TableI3i.tex")
)

## Truncated from 2019 Q2
file.rename(
  here("tab", "ttl_att_full_minrpt_18.tex"),
  here("tab", "TableI3j.tex")
)
file.rename(
  here("tab", "indv_att_full_minrpt_18.tex"),
  here("tab", "TableI3k.tex")
)
file.rename(
  here("tab", "oth_att_full_minrpt_18.tex"),
  here("tab", "TableI3l.tex")
)

## Truncated from 2019 Q3
file.rename(
  here("tab", "ttl_att_full_minrpt_19.tex"),
  here("tab", "TableI3m.tex")
)
file.rename(
  here("tab", "indv_att_full_minrpt_19.tex"),
  here("tab", "TableI3n.tex")
)
file.rename(
  here("tab", "oth_att_full_minrpt_19.tex"),
  here("tab", "TableI3o.tex")
)

## Table I.4 -------------------------------------------------------------------
## Rename files externally; otherwise, hard to hardcode
## No truncation
file.rename(
  here("tab", "ttl_att_full_minrpt_0.tex"),
  here("tab", "TableI4a.tex")
)
file.rename(
  here("tab", "ttl_att_inc_minrpt_0.tex"),
  here("tab", "TableI4b.tex")
)
file.rename(
  here("tab", "ttl_att_senate_minrpt_0.tex"),
  here("tab", "TableI4c.tex")
)
file.rename(
  here("tab", "ttl_att_house_minrpt_0.tex"),
  here("tab", "TableI4d.tex")
)

## Truncated from 2018 Q4
file.rename(
  here("tab", "ttl_att_full_minrpt_16.tex"),
  here("tab", "TableI4e.tex")
)
file.rename(
  here("tab", "ttl_att_inc_minrpt_16.tex"),
  here("tab", "TableI4f.tex")
)
file.rename(
  here("tab", "ttl_att_senate_minrpt_16.tex"),
  here("tab", "TableI4g.tex")
)
file.rename(
  here("tab", "ttl_att_house_minrpt_16.tex"),
  here("tab", "TableI4h.tex")
)

## Truncated from 2019 Q1
file.rename(
  here("tab", "ttl_att_full_minrpt_17.tex"),
  here("tab", "TableI4i.tex")
)
file.rename(
  here("tab", "ttl_att_inc_minrpt_17.tex"),
  here("tab", "TableI4j.tex")
)
file.rename(
  here("tab", "ttl_att_senate_minrpt_17.tex"),
  here("tab", "TableI4k.tex")
)
file.rename(
  here("tab", "ttl_att_house_minrpt_17.tex"),
  here("tab", "TableI4l.tex")
)

## Truncated from 2019 Q2
file.rename(
  here("tab", "ttl_att_full_minrpt_18.tex"),
  here("tab", "TableI4m.tex")
)
file.rename(
  here("tab", "ttl_att_inc_minrpt_18.tex"),
  here("tab", "TableI4n.tex")
)
file.rename(
  here("tab", "ttl_att_senate_minrpt_18.tex"),
  here("tab", "TableI4o.tex")
)
file.rename(
  here("tab", "ttl_att_house_minrpt_18.tex"),
  here("tab", "TableI4p.tex")
)

## Truncated from 2019 Q3
file.rename(
  here("tab", "ttl_att_full_minrpt_19.tex"),
  here("tab", "TableI4q.tex")
)
file.rename(
  here("tab", "ttl_att_inc_minrpt_19.tex"),
  here("tab", "TableI4r.tex")
)
file.rename(
  here("tab", "ttl_att_senate_minrpt_19.tex"),
  here("tab", "TableI4s.tex")
)
file.rename(
  here("tab", "ttl_att_house_minrpt_19.tex"),
  here("tab", "TableI4t.tex")
)

## Figure I.2(a)
file.rename(
  here("fig", "display_treatment_150sample_minrpt_17.pdf"),
  here("fig", "FigI2a.pdf")
)

## Figure I.2(b)
file.rename(
  here("fig", "matchset_only_1stcand_minrpt_17.pdf"),
  here("fig", "FigI2b.pdf")
)

## Remove the rest that ends with _0, _16, _17, _18, _19 (both .tex and .pdf)
file.remove(
  list.files(
    here("tab"),
    pattern = "_0|_16|_17|_18|_19", full.names = TRUE
  )
)
file.remove(
  list.files(
    here("fig"),
    pattern = "_0|_16|_17|_18|_19", full.names = TRUE
  )
)

# Covariate balance evaluation =================================================
## compared to benchmark without refinement ------------------------------------
df_ls <- minrpt_filter(df_ls_orig, min_rpt = 17)

df_ls$full %<>% as.data.frame()
no_matching_benchmark <- PanelMatch_short(
  df_ls$full, "ttl_log",
  matching = FALSE,
  covs.formula = NULL,
  refinement.method = "none"
)

no_refinement_benchmark <- PanelMatch_short(
  df_ls$full, "ttl_log",
  covs.formula = NULL, refinement.method = "none"
)

temp <- list(
  no_matching_benchmark =
    get_covariate_balance(
      no_matching_benchmark$out$att[
        names(no_matching_benchmark$out$att) %in%
          names(no_refinement_benchmark$out$att)
      ],
      data = df_ls$full,
      covariates = c(
        "diff_ttl_log", "senate", "incumbent", "open",
        "no_election", "PVI", "pct_indv"
      ), plot = FALSE
    ),
  no_refinement_benchmark = covbal_short(
    no_refinement_benchmark, df_ls$full,
    plot = FALSE
  ),
  refined = pm_list[["pm_ttl_log_minrpt_17"]]$covbal$full
)

p1 <- ggCB(temp$no_matching_benchmark, ylim = c(-1.5, 1.5))
p2 <- ggCB(temp$no_refinement_benchmark, ylim = c(-1.5, 1.5))
p3 <- ggCB(temp$refined, ylim = c(-1.5, 1.5))

## Figure 5, formerly covbal_improve.pdf
pdf(here("fig", "Fig5.pdf"), width = 7.5, height = 3)
print(
  grid_arrange_shared_legend(
    p1 + ggtitle("Pre-Matching"),
    p2 + ggtitle("Pre-Refinement"),
    p3 + ggtitle("Refinement")
  )
)
dev.off()

# Summary figures ==============================================================
## Three dependent variables ---------------------------------------------------
## Figure 6(a), formerly pm_3ys_full_log_varying.pdf
pdf(here("fig", "Fig6a.pdf"), width = 6, height = 3.5)
ggPM_3ys(ymin = -3, ymax = 2.5) # previously set to -2.5 and 2.5
dev.off()

## Figure I.5(a), formerly pm_3ys_full_log_varying_axis2.pdf
pdf(here("fig", "FigI5a.pdf"), width = 6, height = 3.5)
ggPM_3ys(ymin = -4, ymax = 4)
dev.off()

## Figure I.5(c), formerly pm_3ys_inc_log_varying.pdf
pdf(here("fig", "FigI5c.pdf"), width = 6, height = 3.5)
ggPM_3ys(choice = "inc", ymin = -4, ymax = 4)
dev.off()

## Figure I.5(e), formerly pm_3ys_senate_log_varying.pdf
pdf(here("fig", "FigI5e.pdf"), width = 6, height = 3.5)
ggPM_3ys(choice = "senate", breaks = 1, ymin = -5.1, ymax = 9)
dev.off()

## Figure I.5(g), formerly pm_3ys_house_log_varying.pdf
pdf(here("fig", "FigI5g.pdf"), width = 6, height = 3.5)
ggPM_3ys(choice = "house", ymin = -4, ymax = 4)
dev.off()

## Itemized vs. unitemized -----------------------------------------------------
## Figure 6(b), formerly pm_item_full_log_varying.pdf
pdf(here("fig", "Fig6b.pdf"), width = 6, height = 3.5)
ggPM_3ys(item = TRUE, ymin = -3, ymax = 2.5)
dev.off()

## Figure I.5(b), formerly pm_item_full_log_varying_axis2.pdf
pdf(here("fig", "FigI5b.pdf"), width = 6, height = 3.5)
ggPM_3ys(item = TRUE, ymin = -4, ymax = 4)
dev.off()

## Figure I.5(d), formerly pm_item_inc_log_varying.pdf
pdf(here("fig", "FigI5d.pdf"), width = 6, height = 3.5)
ggPM_3ys(item = TRUE, choice = "inc", ymin = -4, ymax = 4)
dev.off()

## Figure I.5(f), formerly pm_item_senate_log_varying.pdf
pdf(here("fig", "FigI5f.pdf"), width = 6, height = 3.5)
ggPM_3ys(item = TRUE, choice = "senate", breaks = 1, ymin = -5.1, ymax = 9)
dev.off()

## Figure I.5(h), formerly pm_item_house_log_varying.pdf
pdf(here("fig", "FigI5h.pdf"), width = 6, height = 3.5)
ggPM_3ys(item = TRUE, choice = "house", ymin = -4, ymax = 4)
dev.off()

# Matched set size comparison ==================================================
temp <- pm_list[paste0("pm_ttl_log_minrpt_", c(0, 16, 17, 18, 19))] %>%
  map("pm") %>%
  map_dfr(
    ~ data.frame(
      number_treated = summary(.x$full$out$att)$number.of.treated.units,
      matchset = summary(.x$full$out$att)$overview$matched.set.size
    ),
    .id = "min_rpt"
  ) %>%
  mutate(min_rpt = readr::parse_number(min_rpt)) %>%
  mutate(
    Truncation = factor(
      min_rpt,
      levels = c(0, 16, 17, 18, 19),
      labels = c(
        "Not\nTruncated", "From\n2018 Q4",
        "From\n2019 Q1", "From\n2019 Q2", "From\n2019 Q3"
      )
    )
  ) %>%
  select(Truncation, everything()) %>%
  select(-min_rpt) %>%
  mutate(
    percent_treated =
      number_treated / length(unique((df %>% filter(treated == 1))$cand_id)),
    percent_treated = percent(percent_treated)
  )

## Number of treated units -----------------------------------------------------
p <- ggplot(
  temp %>% group_by(Truncation) %>% slice(1),
  aes(x = Truncation, y = number_treated, fill = Truncation)
) +
  geom_col() +
  theme(axis.title.x = element_blank()) +
  ylab("Number of Treated Units") +
  geom_text(aes(label = percent_treated), vjust = -.75, family = "CM Roman") +
  scale_fill_viridis_d(end = 0.8, direction = -1) +
  scale_y_continuous(limits = c(0, 400))

## Figure I.3(a), formerly number_treated_full.pdf
pdf(here("fig", "FigI3a.pdf"), width = 4.8, height = 3.5)
print(plot_nolegend(pdf_default(p)))
dev.off()

## Matched set size mean and sd ------------------------------------------------
p <- ggplot(temp, aes(x = Truncation, y = matchset, colour = Truncation)) +
  geom_boxplot(outlier.alpha = 0.1) +
  theme(axis.title.x = element_blank()) +
  ylab("Matched Set Size") +
  scale_colour_viridis_d(end = 0.8, direction = -1)

## Figure I.3(b), formerly matched_set_size_full.pdf
pdf(here("fig", "FigI3b.pdf"), width = 4.8, height = 3.5)
print(plot_nolegend(pdf_default(p)))
dev.off()
