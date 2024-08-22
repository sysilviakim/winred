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
