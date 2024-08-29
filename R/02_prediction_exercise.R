source(here::here("R", "utilities.R"))

# Import/create data ===========================================================
df <- loadRData(here("data", "raw", "panel_filtered.Rda"))$full
treated_df <- desc_df_create_fxn(df) %>%
  mutate(y = factor(treated, levels = c("No", "Yes")))
timing_df <- desc_df_create_fxn(df, timing = TRUE) %>%
  ## More granular than rpt: increase means joined later
  ## 0 = joined first day
  mutate(y = as.numeric(first_date) - as.numeric(min(.$first_date)))

chamb_vec <- c(house = "House", senate = "Senate")

# Jointly predicting treatment status in a logit regression ====================
## Full data, all variables ----------------------------------------------------
treated_all <- treated_df %>%
  glm(predict_formula("full"), data = ., family = "binomial")

## Full data, no cfscore variables ---------------------------------------------
treated_all_nocfscore <- treated_df %>%
  glm(predict_formula("full", cfscore = FALSE), data = ., family = "binomial")

## By chamber/election, all variables ------------------------------------------
treated_chamb <- chamb_vec %>%
  imap(
    ~ treated_df %>%
      filter(office == str_sub(.x, 1, 1)) %>%
      glm(
        predict_formula("senate"),
        data = ., family = "binomial", maxit = 100
      )
  )

## By chamber/election, no cfscore ---------------------------------------------
treated_chamb_nocfscore <- chamb_vec %>%
  imap(
    ~ treated_df %>%
      filter(office == str_sub(.x, 1, 1)) %>%
      glm(
        predict_formula("senate", cfscore = FALSE),
        data = ., family = "binomial", maxit = 100
      )
  )

# Jointly predicting treatment timing in an ordered logit regression ===========
hist(timing_df$first_date, breaks = "week")

## Full data, all variables ----------------------------------------------------
timing_all <- timing_df %>%
  lm(predict_formula("full", timing = TRUE), data = .)

## Full data, no cfscore variables ---------------------------------------------
timing_all_nocfscore <- timing_df %>%
  lm(predict_formula("full", cfscore = FALSE, timing = TRUE), data = .)

## By chamber/election, all variables ------------------------------------------
timing_chamb <- chamb_vec %>%
  imap(
    ~ timing_df %>%
      filter(office == str_sub(.x, 1, 1)) %>%
      lm(predict_formula("senate", timing = TRUE), data = .)
  )

## By chamber/election, no cfscore ---------------------------------------------
timing_chamb_nocfscore <- chamb_vec %>%
  imap(
    ~ timing_df %>%
      filter(office == str_sub(.x, 1, 1)) %>%
      lm(predict_formula("senate", cfscore = FALSE, timing = TRUE), data = .)
  )

# Multiple models export =======================================================
temp <- c(list(all = treated_all_nocfscore), treated_chamb_nocfscore) %>%
  imap(
    ~ {
      names(.x$coefficients) <-
        tibble(term = names(.x$coefficients)) %>%
        rename_lmterm() %>%
        .$term
      return(.x)
    }
  ) %>%
  imap(
    ~ list(
      model = .x,
      se_robust = coeftest(.x, vcov = vcovHC(.x, type = "HC0"))[, "Std. Error"]
    )
  )

## Table H.1, formerly treated_three_groups.tex
stargazer(
  temp %>% map("model"),
  se = temp %>% map("se_robust"),
  omit = "Constant", dep.var.labels.include = FALSE,
  header = FALSE, model.numbers = FALSE, digits = 3,
  column.labels = c("All", "House", "Senate"),
  dep.var.caption = "Dependent Variable: Decision to Join WinRed",
  out = here("tab", "TableH1.tex"), float = FALSE,
  omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001)
)

temp <- c(list(all = timing_all_nocfscore), timing_chamb_nocfscore) %>%
  imap(
    ~ {
      names(.x$coefficients) <-
        tibble(term = names(.x$coefficients)) %>%
        rename_lmterm() %>%
        .$term
      return(.x)
    }
  ) %>%
  imap(
    ~ list(
      model = .x,
      se_robust = coeftest(.x, vcov = vcovHC(.x, type = "HC0"))[, "Std. Error"]
    )
  )

## Table H.2, formerly timing_three_groups.tex
stargazer(
  temp %>% map("model"),
  se = temp %>% map("se_robust"),
  omit = "Constant", dep.var.labels.include = FALSE,
  header = FALSE, model.numbers = FALSE, digits = 3,
  column.labels = c("All", "House", "Senate"),
  dep.var.caption = "Dependent Variable: Timing of Joining WinRed",
  out = here("tab", "TableH2.tex"), float = FALSE,
  omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001)
)

# Misc. ========================================================================
prop(treated_df, "treated")
