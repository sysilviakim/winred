source(here::here("R", "utilities.R"))

# Import data ==================================================================
load(here("data", "raw", "panel_filtered.Rda"))
df_ls_orig <- df_ls[c("full", "senate", "house", "inc")]

# ZL: explore PNAS data for referee request on unitem ==========================
refunds_by_day <- read.csv(here("data", "raw", "pnas_refunds_by_day.csv"))

pnas_campaigns <- unique(refunds_by_day$recipient)
pnas_campaigns %<>% as.data.frame()
names(pnas_campaigns) <- "recipient"
pnas_campaigns %<>% mutate( # Based on their SI (p. 54-55)
  name = case_when(
    recipient == "cg" ~ "cory gardner",
    recipient == "djt" ~ "donald j trump",
    recipient == "dp" ~ "david purdue",
    recipient == "je" ~ "joni ernst",
    recipient == "jj" ~ "john james",
    recipient == "kk" ~ "kim klacik",
    recipient == "km" ~ "kevin mccarthy",
    recipient == "lg" ~ "lindsey graham",
    recipient == "lj" ~ "lacy johnson",
    recipient == "maga" ~ "maga",
    recipient == "mmc" ~ "mitch mcconnell",
    recipient == "mms" ~ "martha mcsally",
    recipient == "rnc" ~ "rnc",
    recipient == "nrcc" ~ "nrcc",
    recipient == "nrsc" ~ "nrsc",
    recipient == "sc" ~ "susan collins",
    recipient == "sd" ~ "steve daines",
    recipient == "ss" ~ "steve scalise",
    recipient == "ts" ~ "tim scott",
    recipient == "tt" ~ "thom tillis",
    TRUE ~ NA_character_
  )
)
pnas_campaigns %<>% mutate(
  cand_id_int = case_when(
    str_detect(name, "gardner") ~ 1208,
    str_detect(name, "ernst") ~ 1211,
    str_detect(name, "james") ~ 1270,
    str_detect(name, "klacik") ~ 305,
    str_detect(name, "mccarthy") ~ 822,
    str_detect(name, "graham") ~ 1162,
    str_detect(name, "johnson") ~ 345,
    str_detect(name, "mcconnell") ~ 1194,
    str_detect(name, "mcsally") ~ 1257,
    str_detect(name, "collins") ~ 1146,
    str_detect(name, "daines") ~ 1195,
    str_detect(name, "scalise") ~ 284,
    str_detect(name, "scott") ~ 1231,
    str_detect(name, "tillis") ~ 1221,
    TRUE ~ NA_integer_
  )
)

df_ls_orig <- df_ls_orig %>%
  map(
    ~ .x %>%
      # mutate(cand_id = as.integer(factor(cand_id)) + 1) %>%
      group_by(cand_id_int) %>%
      mutate(
        mean_pre_pct_unitem = mean(
          pct_unitem[rpt_int < 17],
          na.rm = TRUE
        ),
        mean_pre_pct_indv = mean(
          pct_indv[rpt_int < 17],
          na.rm = TRUE
        )
      ) %>%
      ungroup()
  )
tempdat <- df_ls_orig$full %>%
  group_by(cand_id_int) %>%
  arrange(desc(rpt_int)) %>%
  slice(1) %>%
  select(
    cand_id_int, mean_pre_pct_unitem,
    senate, incumbent, open, no_election,
    PVI, pct_indv
  ) %>%
  ungroup() %>%
  mutate(pre_pct_unitem_high = mean_pre_pct_unitem > 0.0514)
# median for min_rpt==17
pnas_campaigns %<>% left_join(tempdat, by = "cand_id_int")
# $ kim klacik and lacy johnson had no pre-2019 election experience
# View(pnas_campaigns)
pnas_campaigns %$% table(pre_pct_unitem_high)

sample_table <- pnas_campaigns %>% filter(!is.na(mean_pre_pct_unitem))
sample_table %<>% select(name, mean_pre_pct_unitem, pre_pct_unitem_high)
sample_table %<>% arrange(mean_pre_pct_unitem)
sample_table$mean_pre_pct_unitem %<>% round(3) %>% scales::percent()
sample_table$name %<>% str_to_title()
sample_table$name %<>% str_replace_all(., pattern = "Mcc", replacement = "McC")
sample_table$name %<>% str_replace_all(., pattern = "Mcs", replacement = "McS")
names(sample_table) <- c(
  "Candidate Name",
  "Ave. Pre-2019 % Unitemized",
  "Past Reliance on Unitemized Donations"
)
sample_table %<>%
  mutate(
    `Past Reliance on Unitemized Donations` =
      ifelse(`Past Reliance on Unitemized Donations`, "High", "Low")
  )

## Table I.5, formerly pnas_sample_table.tex
print(
  xtable(
    sample_table,
    type = "latex",
    label = "tab:pnas-sample",
    align = c("l", "l", "c", "c"),
    caption = paste0(
      "Sample of Congressional WinRed Adoptors ",
      "Matched to Weekly Recurring Donations"
    )
  ),
  file = here("tab", "TableI5.tex"),
  include.rownames = FALSE,
  booktabs = TRUE
)

refunds_by_day %<>% left_join(pnas_campaigns)
refunds_by_day %>% filter(!is.na(treat)) %$% summary(treat)
refunds_by_day %>% filter(!is.na(treat)) %$% table(weeks_to_treat)
refunds_by_day %>% filter(!is.na(treat)) %$% table(week)
refunds_by_day %<>% mutate(
  weeks_to_treat = case_when(
    treat == 0 ~ 0,
    treat == 1 & weeks_to_treat >= 0 ~ weeks_to_treat + 1,
    treat == 1 & weeks_to_treat < 0 ~ 0,
    TRUE ~ NA_integer_
  )
)
refunds_summ <-
  refunds_by_day %>%
  filter(!is.na(pre_pct_unitem_high)) %>%
  group_by(cand_id_int, date) %>%
  summarize(
    weeks_to_treat = first(weeks_to_treat),
    senate = first(senate),
    incumbent = first(incumbent),
    open = first(open),
    no_election = first(no_election),
    PVI = first(PVI),
    pct_indv = first(pct_indv),
    mean_pre_pct_unitem = first(mean_pre_pct_unitem),
    pre_pct_unitem_high = first(pre_pct_unitem_high),
    total_amount = sum(
      donation_amount * (source == "Individual Donation"),
      na.rm = TRUE
    ),
    total_weekly_chain = sum(
      donation_amount * (source == "Part of a weekly chain"),
      na.rm = TRUE
    ),
    total_monthly_chain = sum(
      donation_amount * (source == "Part of a monthly chain"),
      na.rm = TRUE
    ),
    total_chain = sum(
      donation_amount * (str_detect(source, "chain")),
      na.rm = TRUE
    ),
    total_refund = sum(
      refunded * donation_amount * (source == "Individual Donation"),
      na.rm = TRUE
    ),
    weekly_refund = sum(
      refunded * donation_amount * (source == "Part of a weekly chain"),
      na.rm = TRUE
    ),
    monthly_refund = sum(
      refunded * donation_amount * (source == "Part of a monthly chain"),
      na.rm = TRUE
    )
  ) %>%
  ungroup()
refunds_summ %<>% mutate(
  refund_rate = total_refund / total_amount,
  weekly_refund_rate = weekly_refund / total_amount,
  monthly_refund_rate = monthly_refund / total_amount,
  weekly_chain_rate = total_weekly_chain / total_amount,
  weekly_chain_rate = ifelse(weekly_chain_rate > 1, 1, weekly_chain_rate),
  monthly_chain_rate = total_monthly_chain / total_amount,
  monthly_chain_rate = ifelse(monthly_chain_rate > 1, 1, monthly_chain_rate),
  chain_rate = total_chain / total_amount,
  chain_rate = ifelse(chain_rate > 1, 1, chain_rate)
)
refunds_summ %$% summary(refund_rate)
refunds_summ %$% summary(monthly_chain_rate)

refunds_summ %<>% mutate(
  treat = weeks_to_treat > 0
)

refunds_summ %>%
  group_by(cand_id_int) %>%
  summarize(
    sum_weeks_to_treat = sum(weeks_to_treat, na.rm = TRUE),
    pre_pct_unitem_high = first(pre_pct_unitem_high)
  ) %>%
  ungroup() %>%
  print()

# 0.366 (***)
refunds_summ %>%
  feols(
    treat ~ pre_pct_unitem_high +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = c("cand_id_int", "date")
  ) %>%
  summary()
mean(refunds_summ$treat, na.rm = TRUE) # 0.212

### The effect of treat on refunds may be lagged...
# 0.0276 (***)
refunds_summ %>%
  feols(
    refund_rate ~ pre_pct_unitem_high + weeks_to_treat +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = c("cand_id_int", "date")
  ) %>%
  summary()
# n.s.
refunds_summ %>%
  feols(
    weekly_refund_rate ~ pre_pct_unitem_high + weeks_to_treat +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = c("cand_id_int", "date")
  ) %>%
  summary()
# n.s.
refunds_summ %>%
  feols(
    monthly_refund_rate ~ pre_pct_unitem_high + weeks_to_treat +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = c("cand_id_int", "date")
  ) %>%
  summary()

# 0.0748 (***)
# 0.0305 (***)
refunds_summ %>%
  feols(
    chain_rate ~ pre_pct_unitem_high + weeks_to_treat +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = c("cand_id_int", "date")
  ) %>%
  summary()
# 0.0265 (**)
# 0.00436 (*)
refunds_summ %>%
  feols(
    weekly_chain_rate ~ pre_pct_unitem_high + weeks_to_treat +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = c("cand_id_int", "date")
  ) %>%
  summary()
mean(refunds_summ$weekly_chain_rate, na.rm = TRUE) # 0.0392
# 0.0510 (+)
# 0.0301 (**)
refunds_summ %>%
  feols(
    monthly_chain_rate ~ pre_pct_unitem_high + weeks_to_treat +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = c("cand_id_int", "date")
  ) %>%
  summary()
mean(refunds_summ$monthly_chain_rate, na.rm = TRUE) # 0.220

mod_treat <-
  refunds_summ %>%
  mutate(pct_indv = pct_indv * 100) %>%
  feols(
    treat ~ pre_pct_unitem_high +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = "cand_id_int"
  ) %>%
  summary()
mod_treat

refunds_summ %>%
  group_by(date) %>%
  mutate(
    mean_treat = mean(treat, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    diff_mean_treat = treat - mean_treat
  ) %$% summary(diff_mean_treat) # 0.000

refunds_summ %$% mean(treat, na.rm = TRUE) # 0.227

mod_weekly_chain <-
  refunds_summ %>%
  mutate(
    pct_indv = pct_indv * 100,
    weekly_chain_rate = weekly_chain_rate * 100
  ) %>%
  feols(
    weekly_chain_rate ~ pre_pct_unitem_high + treat +
      senate + incumbent + open + no_election + PVI + pct_indv |
      date,
    data = .,
    cluster = "cand_id_int"
  ) %>%
  summary()
mod_weekly_chain
setFixest_dict(c(
  pre_pct_unitem_highTRUE = "High Past Reliance on Unitemized Donations",
  weekly_chain_rate = "% WinRed Fundraising From Weekly Recurring Donations",
  senate = "Senate",
  incumbent = "Incumbent",
  open = "Open",
  no_election = "No Election",
  PVI = "Cook PVI",
  pct_indv = "% Fundraising From Individual Donors",
  date = "Date",
  cand_id_int = "Candidate",
  treat = "I(Campaign Added Weekly Defaults)",
  treatTRUE = "I(Campaign Added Weekly Defaults)"
))

setFixest_etable(fitstat = ~ n + r2, page.width = "a4")

tablestyle <- style.tex(
  main = "aer",
  fixef.suffix = " FEs",
  fixef.where = "var",
  fixef.title = "",
  stats.title = "\\midrule",
  tablefoot = FALSE,
  yesNo = "\\checkmark"
)

## Table I.6, formerly unitem_heterogeneity_pnas_treat.tex
etable(
  rep(mod_treat, cluster = list("Candidate", "Date", ~ cand_id_int + date)),
  file = here("tab", "TableI6.tex"),
  title = paste0(
    "Fixed-Effect Estimates of Prior Reliance on Unitemized Contributions on",
    " Use of Weekly Defaults on WinRed"
  ),
  label = "tab:heterogeneity-effect-unitem-pnas-treat",
  cluster = ~ cand_id_int + date,
  drop = "(Intercept)",
  digits = 3,
  digits.stats = 2,
  replace = TRUE,
  page.width = "a4"
)
