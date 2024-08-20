source(here::here("R", "utilities.R"))

# Data import + creation =======================================================
df <- loadRData(here("data", "raw", "panel_filtered.Rda"))$full
treated_df <- desc_df_create_fxn(df)
timing_df <- desc_df_create_fxn(df, timing = TRUE)
assert_that(length(unique(treated_df$cand_id)) == nrow(treated_df))

# Treated status by cand. traits ===============================================
## By incumbency ===============================================================
## Figure H.1(a), formerly treatment_status_inc.pdf
pdf(here("fig", "FigH1a.pdf"), width = 3.8, height = 2.8)
print(barplot_quick(treated_df, "inc_lab", ""))
dev.off()

## By chamber ==================================================================
## Figure H.1(b), formerly treatment_status_chamber_election.pdf
pdf(
  here("fig", "FigH1b.pdf"),
  width = 3.8, height = 2.8
)
print(barplot_quick(treated_df, "office_election_lab", ""))
dev.off()

## By Cook PVI =================================================================
## Figure H.1(c), formerly treatment_status_pvi.pdf
pdf(here("fig", "FigH1c.pdf"), width = 3.8, height = 2.8)
print(barplot_quick(treated_df, "PVI_bin", ""))
dev.off()

## By average past fundraising (over active quarters start in 2015) ============
## Figure H.1(d), formerly treatment_status_ttl.pdf
pdf(here("fig", "FigH1d.pdf"), width = 3.8, height = 2.8)
print(boxplot_quick(treated_df, "avg_past_ttl_log", ""))
dev.off()

# Treated timing by cand. traits================================================
## By incumbency ===============================================================
## Figure H.2(a), formerly treatment_timing_inc.pdf
pdf(here("fig", "FigH2a.pdf"), width = 4.2, height = 3)
print(
  stackedplot_quick(timing_df, "inc_lab") +
    theme(legend.text = element_text(size = 7.5))
)
dev.off()

## By chamber ==================================================================
## Figure H.2(b), formerly treatment_timing_chamber_election.pdf
pdf(
  here("fig", "FigH2b.pdf"),
  width = 4.2, height = 3
)
print(
  stackedplot_quick(timing_df, "office_election_lab") +
    theme(legend.text = element_text(size = 7.5))
)
dev.off()

## By Cook PVI =================================================================
## Figure H.2(c), formerly treatment_timing_pvi.pdf
pdf(here("fig", "FigH2c.pdf"), width = 4.8, height = 3)
print(
  stackedplot_quick(timing_df, "PVI_bin") +
    theme(legend.text = element_text(size = 7.5))
)
dev.off()

## By average past fundraising (over active quarters start in 2015) ============
## Figure H.2(d), formerly treatment_timing_ttl.pdf
pdf(here("fig", "FigH2d.pdf"), width = 4, height = 3)
p <- boxplot_quick(
  timing_df, "avg_past_ttl_log",
  lp = c(.1, .5), label = "", timing = TRUE
) +
  scale_y_continuous(limits = c(-3, max(timing_df$avg_past_ttl_log)))
print(p + theme(legend.text = element_text(size = 7.5)))
dev.off()
