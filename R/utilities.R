# Libraries ====================================================================
library(plyr)
library(tidyverse)
library(lubridate)
library(rvest)
library(here)
library(assertthat)
library(jsonlite)
library(janitor)
library(data.table)
library(caret)
library(fastDummies)
library(panelView)
library(PanelMatch)
library(Kmisc)
library(fixest)
library(magrittr)
library(gridExtra)
library(grid)
library(lmtest)
library(sandwich)
library(broom)
library(xtable)
library(stargazer)
library(estimatr)
library(scales)
library(fastLink)
library(quanteda)
## remotes::install_github("quanteda/quanteda.corpora")
library(quanteda.corpora)
library(keyATM) 
library(stm)
library(tidytext)

# Color schemes ================================================================
platform_color <- c(
  actblue = "#67a9cf", ActBlue = "#67a9cf", actblue_truncated = "#67a9cf",
  winred = "#b2182b", WinRed = "#b2182b",
  actblue_v2 = "#67a9cf", actblue_truncated_v2 = "#67a9cf",
  winred_v2 = "#b2182b"
)

topic_color <- c(
  party = "#ef8a62", Party = "#ef8a62",
  report = "#808080", Reporting = "#808080"
)

# Functions ====================================================================
desc_df_create_fxn <- function(df, timing = FALSE) {
  if (timing == FALSE) {
    out <- df %>%
      filter(grepl("2020", rpt)) %>%
      select(
        cand_id, last_name, first_name, state, region, inc, office, treated,
        avg_past_ttl, avg_past_ttl_log, recipient_cfscore, dwnom1,
        seniority, no_election, office_election, seniority, gender,
        proximity_recipient_cfscore, proximity_dwnom1, PVI, PVI_raw,
        avg_ttl_opp, avg_ttl_opp_log, avg_indv_opp, avg_indv_opp_log,
        avg_past_ttl_opp_log, first_year, last_year, 
        state_club_size, contains("cash_on_hand")
      ) %>%
      dedup() %>%
      group_by(across(setdiff(names(.), c("treated")))) %>%
      summarise(treated = sum(treated)) %>%
      mutate(treated = case_when(treated > 0 ~ "Yes", TRUE ~ "No")) %>%
      mutate(
        proximity_recipient_cfscore = case_when(
          is.nan(proximity_recipient_cfscore) ~ NA_real_,
          TRUE ~ proximity_recipient_cfscore
        ),
        proximity_dwnom1 = case_when(
          is.nan(proximity_dwnom1) ~ NA_real_,
          TRUE ~ proximity_dwnom1
        )
      ) %>%
      ungroup()
  } else {
    out <- df %>%
      filter(treated_sum > 0 & treated == 1) %>%
      group_by(cand_id) %>%
      arrange(rpt) %>%
      slice(1)
  }
  out %>%
    mutate(
      treated_lab = ifelse(treated == "Yes", "WinRed Adopters", "Non-Adopters"),
      treated_lab =
        factor(treated_lab, levels = c("Non-Adopters", "WinRed Adopters")),
      office_lab = factor(ifelse(office == "H", "House", "Senate")),
      inc_lab = case_when(
        inc == "INCUMBENT" ~ "Incumbent",
        inc == "CHALLENGER" ~ "Challenger",
        inc == "OPEN" ~ "Open Seat"
      ),
      inc_lab = factor(
        inc_lab,
        levels = c("Incumbent", "Open Seat", "Challenger"),
        labels = c("Incumbent\n", "Open Seat\n", "Challenger\n")
      ),
      office_election_lab = factor(
        office_election,
        levels = c("House", "Senate, Re-election", "Senate, No election"),
        labels = c("House", "Senate,\nRe-election", "Senate,\nNo election")
      ),
      senior_bin = cut(
        seniority,
        breaks = c(-.5, .5, 4, 8, 12, 50),
        labels = paste0(
          c("None", "1-4 years", "5-8 years", "9-12 years", "12+ years"),
          "\n"
        )
      ),
      PVI_bin = cut(
        PVI, 
        breaks = rev(c(-45, -15, -10, -5, 5, 10, 45)),
        labels = c(
          ## ranges from -33 to 43
          "D+43 to\nD+10", "D+10 to\nD+5", "D+5 to\nR+5",
          "R+5 to\nR+10", "R+10 to\nR+15", "R+15 to\nR+33"
        )
      )
    ) %>%
    ungroup() %>%
    droplevels() %>%
    mutate(
      proximity_cfscore_ntile = cut(
        proximity_recipient_cfscore,
        quantile(
          proximity_recipient_cfscore,
          probs = c(0, 0.005, 0.01, 1), na.rm = TRUE
        ),
        labels = c("0--0.5%", "0.5%--1%", "1%+")
      ),
      proximity_dwnom1_ntile = cut(
        proximity_dwnom1,
        quantile(
          proximity_dwnom1,
          probs = c(0, 0.005, 0.01, 1), na.rm = TRUE
        ),
        labels = c("0--0.5%", "0.5%--1%", "1%+")
      ),
      region = factor(
        region,
        levels = c("North Central", "Northeast", "South", "West")
      )
    )
}

minrpt_filter <- function(x, min_rpt) {
  x %>%
    map(
      ~ .x %>%
        filter(rpt_int >= min_rpt) %>%
        mutate(rpt_int = rpt_int - (min_rpt - 1)) %>%
        mutate(rpt_int = as.integer(rpt_int)) %>%
        droplevels()
    )
}

PanelMatch_short <- function(df,
                             outcome.var,
                             covs.formula,
                             treatment = "treated",
                             time.id = "rpt_int",
                             unit.id = "cand_id_int",
                             lag = 3,
                             refinement.method = "mahalanobis",
                             data = df,
                             match.missing = TRUE,
                             size.match = 10,
                             listwise.delete = FALSE,
                             qoi = "att",
                             lead = 0:2,
                             forbid.treatment.reversal = FALSE,
                             seed = 123,
                             use.diagonal.variance.matrix = TRUE,
                             moderator = "none",
                             ...) {
  set.seed(123)
  out_pm <- PanelMatch(
    data = df,
    covs.formula = covs.formula,
    treatment = treatment,
    time.id = time.id,
    unit.id = unit.id,
    lag = lag,
    refinement.method = refinement.method,
    match.missing = match.missing,
    size.match = size.match,
    qoi = qoi,
    outcome.var = outcome.var,
    lead = lead,
    forbid.treatment.reversal = forbid.treatment.reversal,
    listwise.delete = listwise.delete,
    ...
  )
  if (moderator == "none") {
    results_pm <- PanelEstimate(out_pm, data = df)
  } else {
    results_pm <- PanelEstimate(out_pm, data = df, moderator = moderator)
  }
  
  return(list(out = out_pm, est = results_pm))
}

covbal_short <- function(out,
                         df,
                         diffdv = "diff_ttl_log",
                         plot = TRUE,
                         ylim = c(-1, 1),
                         covariates = c(
                           "senate", "incumbent", "open",
                           "no_election", "PVI",
                           "pct_indv" 
                         ),
                         ...) {
  covariates <- c(diffdv, covariates)
  
  temp <- df %>%
    mutate(
      across(
        all_of(c(xvar, lag_xvar)),
        ~ case_when(
          is.Date(.x) ~ as.numeric(.x) - 18307,
          TRUE ~ as.numeric(.x)
        )
      )
    )
  
  if (plot) {
    get_covariate_balance(
      out$out$att,
      data = temp,
      covariates = covariates,
      plot = TRUE,
      ylim = ylim,
      ...
    )
  } else {
    get_covariate_balance(
      out$out$att,
      data = temp,
      covariates = covariates,
      ...
    )
  }
}

ggCB <- function(df, ylim = c(-2, 2), legend = "bottom", nrow = 3) {
  p <- df %>%
    rowid_matrix_to_df(colname = "period") %>%
    pivot_longer(cols = 2:ncol(.)) %>%
    mutate(
      period = factor(
        period,
        levels = c("t_3", "t_2", "t_1", "t_0"),
        labels = c("t-3", "t-2", "t-1", "t-0")
      ),
      name = case_when(
        name == "ttl_log" ~ "Logged Total Fundraising",
        name == "indv_ttl_log" ~ "Logged Individual Fundraising",
        name == "oth_log" ~ "Logged Non-individual Fundraising",
        name == "ttl_opp_log" ~ "Logged Total Fundraising of Opponent",
        name == "senate" ~ "Senate",
        name == "incumbent" ~ "Incumbent",
        name == "open" ~ "Open Seat",
        name == "no_election" ~ "Not up for Re-election",
        name == "PVI" ~ "Cook PVI",
        name == "diff_ttl_log" ~ "Change in Logged Total Fundraising",
        name == "diff_indv_ttl_log" ~ "Change in Logged Individual Fundraising",
        name == "diff_oth_log" ~ "Change in Logged Other Fundraising",
        name == "diff_item_log" ~ "Change in Logged Itemized Fundraising",
        name == "diff_unitem_log" ~ "Change in Logged Unitemized Fundraising",
        name == "diff_corp_log" ~ "Change in Logged Corp PAC Fundraising",
        name == "pct_indv" ~ "Pct. Individual Fundraising"
      ),
      name = factor(
        name,
        levels = c(
          "Change in Logged Total Fundraising",
          "Change in Logged Individual Fundraising",
          "Change in Logged Other Fundraising",
          "Change in Logged Itemized Fundraising",
          "Change in Logged Unitemized Fundraising",
          "Change in Logged Corp PAC Fundraising",
          "Logged Total Fundraising",
          "Logged Individual Fundraising",
          "Logged Non-individual Fundraising",
          "Senate", "Incumbent", "Open Seat", "Not up for Re-election",
          "Cook PVI", "Logged Total Fundraising of Opponent",
          "Pct. Individual Fundraising"
        )
      )
    ) %>%
    rename(Covariates = name) %>%
    ggplot(
      aes(
        x = period, y = value,
        group = Covariates, colour = Covariates, linetype = Covariates
      )
    ) +
    geom_line() +
    scale_y_continuous(limits = ylim) +
    ylab("Std. Dev.") +
    scale_colour_viridis_d(end = .9)
  pdf_default(p) +
    theme(
      legend.position = legend,
      axis.title.x = element_blank(),
      legend.title = element_blank()
    ) +
    guides(
      colour = guide_legend(nrow = nrow),
      linetype = guide_legend(nrow = nrow)
    )
}

ggPM <- function(x, ylab = "Estimated Effect of Treatment", xlab = "Time",
                 main = "Estimated Effects of Treatment Over Time",
                 ylim = NULL, breaks = 2,
                 pdf = FALSE) {
  pe.object <- x
  plot.data <- summary(pe.object, verbose = F, bias.corrected = F)
  if (is.null(ylim)) {
    ylim <- c(
      min(plot.data[, "2.5%"]) - abs(mean(plot.data[, "2.5%"])),
      ## max at the second term in the original PanelMatch:::plot.PanelEstimate
      ## will displose; too large a margin
      max(plot.data[, "97.5%"]) + abs(mean(plot.data[, "97.5%"]))
    )
  }
  
  df <- data.frame(
    x = 1:(nrow(plot.data)),
    x_label = rownames(plot.data),
    y = plot.data[, "estimate"],
    ymin = plot.data[, "2.5%"],
    ymax = plot.data[, "97.5%"]
  )
  
  p <- ggplot(df, aes(x = x_label, y = y)) +
    xlab(xlab) +
    ylab(ylab) +
    scale_y_continuous(
      limits = ylim, labels = scales::comma,
      breaks = seq(ylim[1], ylim[2], by = breaks)
    ) +
    ggtitle(main) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(aes(ymin = ymin, ymax = ymax)) +
    scale_x_discrete(expand = c(0.02, 0.05)) +
    theme_bw()
  
  if (pdf) {
    p <- pdf_default(p)
  }
  return(p)
}

ggPM_var <- function(x, choice = "full", target = NULL,
                     dodge = 0.5, end = 0.8, ylim = NULL, breaks = NULL) {
  if (str_detect(x, "corp")) { 
    temp <- pm_list[paste0(x, c(0, 16, 17, 18, 19))] %>%
      map("att_summ")
  }
  else {
    temp <- pm_list[paste0(x, c(0, 16, 17, 18, 19))] %>%
      map("pm")
  }
  
  if (!is.null(target)) {
    temp <- temp %>%
      map_dfr(
        ~ summary(.x$est[[target]])$summary %>%
          as_tibble(rownames = "period"),
        .id = "min_rpt"
      )
  } 
  if (str_detect(x, "corp")) { 
    temp <- temp %>%
      map_dfr(
        ~ .x[[choice]] %>%
          as_tibble(rownames = "period"),
        .id = "min_rpt"
      )
  }
  if (!str_detect(x, "corp"))  {
    temp <- temp %>%
      map_dfr(
        ~ summary(.x[[choice]]$est)$summary %>%
          as_tibble(rownames = "period"),
        .id = "min_rpt"
      )
  }
  
  temp <- temp %>%
    mutate(min_rpt = readr::parse_number(min_rpt)) %>%
    mutate(
      Truncation = factor(
        min_rpt,
        levels = c(0, 16, 17, 18, 19),
        labels = c(
          "Not Truncated", "From 2018 Q4",
          "From 2019 Q1", "From 2019 Q2", "From 2019 Q3"
        )
      )
    )
  
  p <- ggplot(
    temp,
    aes(
      x = period, y = estimate, 
      group = Truncation, color = Truncation, shape = Truncation
    )
  ) +
    geom_point(position = position_dodge(width = dodge)) +
    xlab("Time") +
    ylab("Estimated Effect of Treatment") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(
      aes(ymin = `2.5%`, ymax = `97.5%`),
      position = position_dodge(width = dodge)
    ) +
    scale_x_discrete(expand = c(0.02, 0.05)) +
    theme_bw() +
    scale_colour_viridis_d(end = end, direction = -1)
  
  p <- pdf_default(p) +
    theme(legend.position = "bottom") +
    guides(
      group = guide_legend(nrow = 2, byrow = TRUE),
      color = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  if (!is.null(ylim)) {
    if (is.null(breaks)) {
      stop("Specify the breaks.")
    }
    p <- p +
      scale_y_continuous(
        limits = ylim, labels = scales::comma,
        breaks = seq(ylim[1], ylim[2], by = breaks)
      )
  }
  
  return(p)
}

ggPM_var2 <- function(x, choice = "full", target = NULL,
                      dodge = 0.5, end = 0.8, ylim = NULL, breaks = NULL) {
  if (str_detect(x, "corp")) { 
    temp <- pm_list[paste0(x, c(0, 16, 17, 18, 19))] %>%
      map("att_summ")
  }
  else {
    temp <- pm_list[paste0(x, c(0, 16, 17, 18, 19))] %>%
      map("pm")
  }
  
  if (!is.null(target)) {
    temp <- temp %>%
      map_dfr(
        ~ summary(.x$est[[target]])$summary %>%
          as_tibble(rownames = "period"),
        .id = "min_rpt"
      )
  }
  if (is.null(target) & str_detect(x, "corp")) { 
    temp <- temp %>%
      map_dfr(
        ~ .x[[choice]] %>%
          as_tibble(rownames = "period"),
        .id = "min_rpt"
      )
  }
  if (is.null(target) & str_detect(x, "corp")) { 
    temp <- temp %>%
      map_dfr(
        ~ summary(.x[[choice]]$est)$summary %>%
          as_tibble(rownames = "period"),
        .id = "min_rpt"
      )
  }
  
  temp <- temp %>%
    mutate(min_rpt = readr::parse_number(min_rpt)) %>%
    mutate(
      Truncation = factor(
        min_rpt,
        levels = c(0, 16, 17, 18, 19),
        labels = c(
          "Not Truncated", "From 2018 Q4",
          "From 2019 Q1", "From 2019 Q2", "From 2019 Q3"
        )
      )
    )
  
  p <- ggplot(
    temp,
    aes(
      x = period, y = estimate, 
      group = Truncation, color = Truncation, shape = Truncation
    )
  ) +
    geom_point(position = position_dodge(width = dodge)) +
    xlab("Time") +
    ylab("Estimated Effect of Treatment") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(
      aes(ymin = `2.5%`, ymax = `97.5%`),
      position = position_dodge(width = dodge)
    ) +
    scale_x_discrete(expand = c(0.02, 0.05)) +
    theme_bw() +
    scale_colour_viridis_d(end = end, direction = -1)
  
  p <- pdf_default(p) +
    theme(legend.position = "bottom") +
    guides(
      group = guide_legend(nrow = 2, byrow = TRUE),
      color = guide_legend(nrow = 2, byrow = TRUE)
    )
  
  if (!is.null(ylim)) {
    if (is.null(breaks)) {
      stop("Specify the breaks.")
    }
    p <- p +
      scale_y_continuous(
        limits = ylim, labels = scales::comma,
        breaks = seq(ylim[1], ylim[2], by = breaks)
      )
  }
  
  return(p)
}

axis_unify <- function(p, ymin = "2.5%", ymax = "97.5%") {
  ymin <- p %>%
    map_dbl(~ min(.x$data[, ymin])) %>%
    min()
  temp <- seq(floor(ymin), ceiling(ymin), by = 0.5) %>%
    map_dbl(~ .x - ymin)
  ymin <- seq(floor(ymin), ceiling(ymin), by = 0.5)[
    which(temp < 0 & abs(temp) == min(abs(temp[temp < 0])))[1]
  ]
  
  ymax <- p %>%
    map_dbl(~ max(.x$data[, ymax])) %>%
    max()
  temp <- seq(floor(ymax), ceiling(ymax), by = 0.5) %>%
    map_dbl(~ .x - ymax)
  ymax <- seq(floor(ymax), ceiling(ymax), by = 0.5)[
    which(temp > 0 & abs(temp) == min(abs(temp[temp > 0])))[1]
  ]
  return(list(ymin = ymin, ymax = ymax))
}


ggPM_3ys <- function(x = "pm_ttl_log_minrpt_", item = FALSE, dodge = 0.7,
                     ymin = NULL, ymax = NULL, breaks = NULL,
                     ...) {
  if (item == FALSE) {
    p1 <- ggPM_var(x, dodge = dodge, ...) +
      theme(axis.title = element_blank()) +
      ggtitle("All Contributions")
    p2 <- ggPM_var(gsub("ttl", "indv", x), dodge = dodge, ...) +
      theme(axis.title = element_blank()) +
      ggtitle("Individual Cont.")
    p3 <- ggPM_var(gsub("ttl", "oth", x), dodge = dodge, ...) +
      theme(axis.title = element_blank()) +
      ggtitle("Other Cont.")
    p <- list(p1, p2, p3)
  } else {
    p1 <- ggPM_var(gsub("ttl", "item", x), dodge = dodge, ...) +
      theme(axis.title = element_blank()) +
      ggtitle("Itemized Contributions")
    p2 <- ggPM_var(gsub("ttl", "unitem", x), dodge = dodge, ...) +
      theme(axis.title = element_blank()) +
      ggtitle("Unitemized Contributions")
    p <- list(p1, p2)
  }
  
  if (is.null(ymin) | is.null(ymax)) {
    ymin <- axis_unify(p)$ymin
    ymax <- axis_unify(p)$ymax
  }
  
  if (!is.null(breaks)) {
    p <- p %>%
      map(
        ~ .x +
          scale_y_continuous(
            limits = c(ymin, ymax),
            breaks = seq(floor(ymin) + breaks, ceiling(ymax), by = breaks)
          )
      )
  } else {
    p <- p %>%
      map(~ .x + scale_y_continuous(limits = c(ymin, ymax)))
  }
  return(grid_arrange_shared_legend(list = p, nrow = 1))
}

predict_formula <- function(x, cfscore = TRUE, state = TRUE, timing = FALSE) {
  covariates <- c(
    "office", "inc", "seniority", "no_election", "gender", "PVI",
    "avg_past_ttl_log", "avg_past_ttl_opp_log",
    "recipient_cfscore", "proximity_recipient_cfscore", "state_club_size",
    "region"
  )
  if (x == "senate") {
    covariates <- setdiff(covariates, "office")
  } else if (x == "house") {
    covariates <- setdiff(covariates, c("office", "no_election"))
  }
  
  if (cfscore == FALSE) {
    covariates <- setdiff(
      covariates,
      c(
        "recipient_cfscore", "proximity_recipient_cfscore",
        "avg_first_date_proximity_recipient_cfscore"
      )
    )
  }
  
  if (state == FALSE) {
    covariates <- c(setdiff(covariates, "state"), "region")
  }
  
  if (timing) {
    if (cfscore == FALSE) {
      covariates <- setdiff(
        covariates,
        "avg_first_date_proximity_recipient_cfscore"
      )
    }
  }
  return(
    as.formula(paste0("y ~ ", covariates %>% paste0(collapse = " + ")))
  )
}

cor.test.fct <- function(df, A, Aval, B, Bval) {
  cor.test(
    as.numeric(df[[A]] == Aval),
    as.numeric(df[[B]] == Bval)
  )
}

cond_all <- function(df, x) {
  temp <- levels(df[[x]]) %>%
    map(~ pretty_condprob(df, "treated", "Yes", x, .x))
}

barplot_quick <- function(df, var, label) {
  p <- df %>%
    group_by(!!as.name(var)) %>%
    summarize(
      share.WinRed = sum(treated == "Yes") / n(),
      sd.share.WinRed = sd(as.integer(treated == "Yes"))
    ) %>%
    ungroup() %>%
    ggplot(aes(x = !!as.name(var), y = share.WinRed)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    geom_bar(
      position = "dodge", stat = "identity", width = .75, fill = "#ca0020"
    ) +
    labs(x = label, y = "Fraction of Candidates on WinRed")
  pdf_default(p) +
    theme(
      axis.title.x = element_blank()
    )
}

boxplot_quick <- function(df, var, label, lp = c(0.25, 0.5), timing = FALSE) {
  if (timing == FALSE) {
    p <- df %>%
      ggplot(aes(y = !!as.name(var), x = treated_lab, fill = treated_lab)) +
      geom_boxplot(width = .5) +
      labs(y = label, x = "") +
      ## https://stackoverflow.com/questions/34227967/
      coord_flip() +
      scale_x_discrete(limits = rev(levels(df$treated_lab)))
    pdf_default(p) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = lp,
        legend.background = element_rect(fill = alpha("white", 0)) # transparent
      ) +
      ## https://colorbrewer2.org/#type=diverging&scheme=RdBu&n=5
      scale_fill_manual(values = c("#f4a582", "#ca0020"))
  } else {
    p <- df %>%
      mutate(
        first_quarter = case_when(
          first_quarter == 2019.1 ~ 2019.3,
          first_quarter == 2019.2 ~ 2019.3,
          first_quarter == 2020.4 ~ 2020.3,
          TRUE ~ first_quarter
        ),
        first_quarter = factor(
          first_quarter,
          levels = c("2019.3", "2019.4", "2020.1", "2020.2", "2020.3"),
          labels = c(
            "2019 Q3\nor before", "2019 Q4", "2020 Q1", "2020 Q2",
            "2020 Q3\nor after"
          )
        )
      ) %>%
      ggplot(
        aes(y = !!as.name(var), x = first_quarter, fill = first_quarter)
      ) +
      geom_boxplot(width = .5) +
      labs(y = label, x = "") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(df$first_quarter)))
    pdf_default(p) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = lp,
        legend.background = element_rect(fill = alpha("white", 0)) # transparent
      ) +
      scale_fill_brewer(type = "seq", direction = -1, palette = "Reds")
  }
}

pval_export <- function(df, x, val = NULL, fname = NULL, timing = FALSE) {
  if (timing == FALSE) {
    if (is.null(fname)) {
      fname <- paste0("treatment_status_", gsub("_lab$", "", x), "_cortest.tex")
    } else {
      fname <- paste0("treatment_status_", fname, "_cortest.tex")
    }
    if (class(df[[x]]) %in% c("character", "factor")) {
      if (is.null(val)) {
        message("Specify a level.")
      } else {
        out <- df %>%
          cor.test.fct(x, val, "treated", "Yes")
      }
    } else {
      out <- cor.test(as.numeric(df[[x]]), as.numeric(df$treated == "Yes"))
    }
    
    print(out)
    out$p.value %>%
      formatC(format = "f", digits = 3) %>%
      write(here("tex", fname))
  } else {
    if (is.null(fname)) {
      fname <- paste0("treatment_timing_", gsub("_lab$", "", x), "_cortest.tex")
    } else {
      fname <- paste0("treatment_timing_", fname, "_cortest.tex")
    }
    if (class(df[[x]]) %in% c("character", "factor")) {
      if (is.null(val)) {
        message("Specify a level.")
      } else {
        out <- cor.test(as.numeric(df[[x]] == val), as.numeric(df$first_date))
      }
    } else {
      out <- cor.test(as.numeric(df[[x]]), as.numeric(df$first_date))
    }
    
    print(out)
    out$p.value %>%
      formatC(format = "f", digits = 3) %>%
      write(here("tex", fname))
  }
}

group_treated_prop <- function(df, x, timing = FALSE) {
  if (timing == FALSE) {
    df %>%
      group_by(!!as.name(x)) %>%
      summarize(frac_treated = sum(treated == "Yes") / n()) %>%
      ungroup() %>%
      print()
  } else {
    df %>%
      group_by(!!as.name(x)) %>%
      summarize(first_date_mean = mean(first_date)) %>%
      ungroup() %>%
      arrange(desc(first_date_mean))
  }
}

stackedplot_quick <- function(df, x, timing = TRUE) {
  if (timing) {
    p <- df %>%
      mutate(
        first_quarter = case_when(
          first_quarter == 2019.1 ~ 2019.3,
          first_quarter == 2019.2 ~ 2019.3,
          first_quarter == 2020.4 ~ 2020.3,
          TRUE ~ first_quarter
        )
      ) %>%
      count(!!as.name(x), first_quarter) %>%
      pivot_wider(names_from = c("first_quarter"), values_from = "n") %>%
      replace(is.na(.), 0) %>%
      mutate(n = rowSums(across(where(is.numeric)))) %>%
      mutate(across(contains("20"), ~ .x / n)) %>%
      pivot_longer(cols = names(.)[2:(ncol(.) - 1)], names_to = "var") %>%
      mutate(
        Quarter = factor(
          var,
          levels = c("2019.3", "2019.4", "2020.1", "2020.2", "2020.3"),
          labels = c(
            "2019 Q3\nor before", "2019 Q4", "2020 Q1", "2020 Q2",
            "2020 Q3\nor after"
          )
        )
      ) %>%
      ggplot(aes(fill = Quarter, y = value, x = !!as.name(x))) +
      geom_bar(position = "fill", stat = "identity") +
      # scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26")) +
      scale_fill_brewer(type = "seq", direction = -1, palette = "Reds") +
      labs(y = "Fraction Joining WinRed by Quarter", x = "") +
      scale_y_continuous(labels = scales::percent)
    pdf_default(p) +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(1.5, "lines")
      )
  } else {
    p <- df %>%
      count(!!as.name(x), treated) %>%
      pivot_wider(names_from = c("treated"), values_from = "n") %>%
      replace(is.na(.), 0) %>%
      mutate(n = rowSums(across(where(is.numeric)))) %>%
      pivot_longer(cols = c("Yes", "No"), names_to = "var") %>%
      mutate(
        var = factor(
          var,
          levels = c("No", "Yes"),
          labels = c("Non-Adopters", "WinRed Adopters")
        )
      ) %>%
      ggplot(aes(fill = var, y = value, x = !!as.name(x))) +
      geom_bar(position = "fill", stat = "identity") +
      labs(y = "Fraction Joining WinRed", x = "") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("#f4a582", "#ca0020"))
    pdf_default(p) +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      theme(
        axis.title.x = element_blank(),
        legend.title = element_blank()
      )
  }
}

pct_nonmissing <- function(df, x, fname = NULL, timing = FALSE) {
  if (timing == FALSE) {
    if (is.null(fname)) {
      fname <- paste0("pct_nonmissing_", gsub("_lab$", "", x), ".tex")
    } else {
      fname <- paste0("pct_nonmissing_", fname, ".tex")
    }
    out <- sum(!is.na(df[[x]])) / nrow(df)
    out <- round(out * 100, digits = 1)
    out %<>% as.character() %>% paste0(., "\\%")
    print(out)
    out %>% write(here("tex", fname))
  } else {
    print("Do not use this function with panel data.")
    break
  }
}

rename_lmterm <- function(x) {
  x %>%
    mutate(
      term = case_when(
        term == "officeS" ~ "Senate",
        term == "incINCUMBENT" ~ "Incumbent",
        term == "incOPEN" ~ "Open Seat",
        term == "no_election" ~ "No Election",
        term == "PVI" ~ "Cook PVI",
        term == "avg_past_ttl_log" ~
          "Avg. Past Fundraising (Logged)",
        term == "avg_past_ttl_opp_log" ~
          "Avg. Opponent Past Fundraising (Logged)",
        term == "recipient_cfscore" ~ "Recipient CFscore",
        term == "proximity_recipient_cfscore" ~
          "Proximity to Adopters in Recipient CFscores",
        term == "state_club_size" ~
          "Proportion of Same-State Adopters",
        term == "recipient_cfscore" ~ "Recipient CFscore",
        term == "proximity_recipient_cfscore" ~
          "Proximity to Adopters in Recipient CFscores",
        term == "avg_first_date_proximity_recipient_cfscore" ~
          "Average Adoption Date Among Proximate Candidates (Recipient CFscores)",
        term == "avg_first_date_state" ~
          "Average Adoption Date Among Same-State Candidates",
        str_detect(term, "diff_ttl_log_lag") ~
          paste0(
            "Change in (Logged) Fundraising (2019 Q",
            (4 - as.integer(str_sub(term, -1, -1))),
            "-Q",
            (4 + 1 - as.integer(str_sub(term, -1, -1))),
            ")"
          ),
        str_detect(term, "diff_ttl_opp_log_lag") ~
          paste0(
            "Change in Opponent's (Logged) Fundraising (2019 Q",
            (4 - as.integer(str_sub(term, -1, -1))),
            "-Q",
            (4 + 1 - as.integer(str_sub(term, -1, -1))),
            ")"
          ),
        term == "regionSouth" ~ "Region: South",
        term == "regionNorth Central" ~ "Region: North Central",
        term == "regionNortheast" ~ "Region: Northeast",
        term == "regionWest" ~ "Region: West",
        TRUE ~ str_to_title(term)
      )
    ) %>%
    mutate(
      term = factor(
        term,
        levels = c(
          "(Intercept)", "Senate", "Incumbent", "Open Seat", "Gender",
          "No Election", "Cook PVI",
          "Avg. Opponent Past Fundraising (Logged)",
          "Change in (Logged) Fundraising (2019 Q3-Q4)",
          "Change in (Logged) Fundraising (2019 Q2-Q3)",
          "Change in (Logged) Fundraising (2019 Q1-Q2)",
          "Change in Opponent's (Logged) Fundraising (2019 Q3-Q4)",
          "Change in Opponent's (Logged) Fundraising (2019 Q2-Q3)",
          "Change in Opponent's (Logged) Fundraising (2019 Q1-Q2)",
          "Avg. Past Fundraising (Logged)",
          "Seniority",
          "Recipient CFscore", "Proximity to Adopters in Recipient CFscores",
          paste0(
            "Average Adoption Date Among Proximate Candidates ",
            "(Recipient CFscores)"
          ),
          "Proportion of Same-State Adopters",
          "Average Adoption Date Among Same-State Candidates",
          "Region: North Central", "Region: Northeast",
          "Region: South", "Region: West", state.name
        )
      )
    )
}

coeftest_summ <- function(x, fname = NULL, export = FALSE, align = "llc",
                          timing = FALSE) {
  ## Robust SE
  tab <- coeftest(x, vcov = vcovHC(x, type = "HC0")) %>%
    tidy() %>%
    ## Drop region FEs
    filter(!(str_sub(term, 1, 6) == "region")) %>%
    rename_lmterm() %>%
    mutate(
      estimate = formatC(estimate, format = "f", digits = 3),
      estimate = case_when(
        p.value < 0.10 & p.value >= 0.05 ~ paste0(estimate, "^{*}"),
        p.value < 0.05 & p.value >= 0.01 ~ paste0(estimate, "^{**}"),
        p.value < 0.01 ~ paste0(estimate, "^{***}"),
        TRUE ~ estimate
      ),
      estimate = paste0("$", estimate, "$"),
      std.error = formatC(std.error, format = "f", digits = 3),
    ) %>%
    select(term, estimate, std.error) %>%
    pivot_longer(cols = c("estimate", "std.error")) %>%
    arrange(term) %>%
    mutate(
      value = if_else(name == "std.error", paste0("(", value, ")"), value),
      term = as.character(term),
      term = case_when(
        name == "std.error" ~ "",
        TRUE ~ term
      )
    ) %>%
    select(-name)
  
  ## LaTeX adjustments
  midbar <- nrow(tab)
  
  fe_app <- cbind(
    c("Region FEs", "Observations"),
    c("\\checkmark", format(nobs(x), big.mark = ","))
  )
  tab %<>% as.matrix() %>% rbind(., fe_app)
  colnames(tab) <- NULL
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 0
  addtorow$pos[[2]] <- midbar
  
  if (timing == FALSE) {
    addtorow$command <- c(
      "\\toprule \\\\[-1.8ex] \n
    & \\multicolumn{1}{c}{DV: I(WinRed)} \\\\  
    \\midrule \\\\[-1.8ex] \n",
      "\\midrule \n"
    )
  } else {
    addtorow$command <- c(
      "\\toprule \\\\[-1.8ex] \n
    & \\multicolumn{1}{c}{DV: Date Joined WinRed} \\\\  
    \\midrule \\\\[-1.8ex] \n",
      "\\midrule \n"
    )
  }
  
  ## Export
  if (export) {
    if (is.null(fname)) {
      stop("Specify the file name, including the .tex extension.")
    }
    tab <- xtable(tab, align = align)
    ## align(output) <- "llcc"
    ## has an extra "l" because xtable has an added column for row
    tab %>%
      print.xtable(
        include.rownames = FALSE,
        comment = FALSE,
        type = "latex",
        sanitize.text.function = function(x) x,
        add.to.row = addtorow,
        hline.after = c(nrow(tab)),
        file = here("tab", fname),
        caption.placement = "top",
        include.colnames = FALSE,
        floating = FALSE,
        booktabs = TRUE
      )
  } else {
    tab <- xtable(tab, align = align)
    ## align(output) <- "llcc"
    ## has an extra "l" because xtable has an added column for row
    tab %>%
      print.xtable(
        include.rownames = FALSE,
        comment = FALSE,
        type = "latex",
        sanitize.text.function = function(x) x,
        add.to.row = addtorow,
        hline.after = c(nrow(tab)),
        caption.placement = "top",
        include.colnames = FALSE,
        floating = FALSE,
        booktabs = TRUE
      )
  }
}

xtable_summary <- function(x, fname, digits = 3) {
  out <- rowid_matrix_to_df(x) %>%
    mutate(
      across(where(is.numeric), ~ formatC(.x, format = "f", digits = digits))
    ) %>%
    rename(` ` = rownames, Estimate = estimate, `Std. Error` = std.error) %>%
    xtable()
  print(
    out,
    file = here("tab", fname),
    include.rownames = FALSE,
    comment = FALSE,
    floating = FALSE,
    booktabs = TRUE
  )
}

## https://juliasilge.com/blog/sherlock-holmes-stm/
visualize_word_list <- function(x, topn = 10) {
  x %>%
    tidy() %>%
    group_by(topic) %>%
    top_n(topn, beta) %>%
    ungroup() %>%
    mutate(
      topic = paste0("Topic ", topic),
      term = reorder_within(term, beta, topic)
    ) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(
      x = NULL, y = expression(beta),
      title = "Highest word probabilities for each topic",
      subtitle = "Different words are associated with different topics"
    ) +
    scale_fill_viridis_d()
}

fixest_form <- function(y, x, fe = "0", x_re = FALSE, df = NULL) {
  if (x_re & !is.null(df)) {
    x <- x %>%
      map(~ grep(., colnames(df), value = T)) %>%
      reduce(c)
  }
  
  xpart <- paste(x, collapse = " + ")
  fepart <- paste(fe, collapse = " + ")
  
  y %>%
    paste(paste(xpart, fepart, sep = "|"), sep = " ~ ") %>%
    as.formula()
}

# Variables ====================================================================
## all xvar unit-invariant
yvar <- "ttl"
tvar <- "treated"

xvar_int <- xvar <- 
  c("senate", "incumbent", "open", "no_election", "PVI", "pct_indv")
lag_xvar <- c("diff_ttl_log")

# Benchmark formula ============================================================
fm_bench <-
  as.formula(paste0(yvar, " ~ ", c(tvar, xvar_int) %>% paste(collapse = " + ")))

cov_bench <- function(
    dv = "diff_ttl_log",
    xvar = xvar 
) {
  return(as.formula(
    setdiff(xvar, c("state")) %>% paste(collapse = " + ") %>%
      paste0(
        ., "+ I(lag(", paste0(dv, collapse = ", 1:3)) + I(lag("), ", 1:3))"
      ) %>%
      paste("~", .)
  ))
}

# Other options ================================================================
options(scipen = 999)
