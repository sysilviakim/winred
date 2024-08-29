source(here::here("R", "utilities.R"))
categories <- c("senate", "house")
load(here("data", "raw", "portfolio.Rda"))

portfolio_summ <- function(df,
                           values_from = "portfolio",
                           order_vars = c("name", "url")) {
  # First, take some necessary steps to trim and clean
  df <- df %>%
    mutate(across(everything(), trimws)) %>%
    # If it is 25-100-250-100-NA, for example, NA was for "Other Values"
    # So delete
    group_by(across(-contains(values_from))) %>%
    filter(
      !(n_distinct(!!as.name(values_from)) > 1 &
          is.na(!!as.name(values_from)))
    ) %>%
    group_by(across(-(contains(values_from) | contains("date")))) %>%
    filter(
      # There are other values than NA; NAs are only in the beginning
      # That indicates scraper that was incomplete
      # Delete these
      !(n_distinct(rleid(!!as.name(values_from))) > 2 &
          rleid(!!as.name(values_from)) == 1 &
          is.na(!!as.name(values_from)))
    ) %>%
    ungroup() %>%
    group_by(across(all_of(order_vars))) %>%
    group_split() %>%
    map_dfr(
      ~ .x %>%
        arrange(date) %>%
        filter(
          # Before and after have no NA values; just this one
          !(!is.na(lag(portfolio)) & (lag(date) != date) &
              !is.na(lead(portfolio)) & (lead(date) != date) &
              is.na(portfolio))
        ) %>%
        filter(
          # If previously non-NA values, no last NA value
          !(!is.na(lag(portfolio)) & (lag(date) != date) &
              is.na(portfolio) & date == last(date))
        )
    )
  
  # Substitute NA values
  df[[values_from]][is.na(df[[values_from]])] <- -999
  
  # Pivot from long to wide, create date-columns
  df <- df %>%
    pivot_wider(
      names_from = date,
      values_from = !!as.name(values_from),
      names_prefix = "date_",
      values_fn = list
    ) %>%
    clean_names() %>%
    arrange(across(all_of(order_vars))) %>%
    ungroup()
  
  # Using all combinations of entities (name-URL) and dates, create nested list
  temp <- cross2(
    df %>% select(contains("date_")) %>% names(),
    seq(nrow(df))
  ) %>%
    map(
      ~ {
        out <- list()
        out[[paste0("row_", str_pad(.x[[2]], width = 5, pad = "0"))]] <-
          df[.x[[2]], .x[[1]], drop = TRUE]
        return(out)
      }
    )
  
  # Derive unique menus for each entity, excluding NULL and NA values
  menu_list <- seq(nrow(df)) %>%
    map(
      ~ unique(
        temp[
          seq(
            1, length(temp),
            by = df %>% select(contains("date_")) %>% ncol()
          )[.x]:
            seq(
              df %>% select(contains("date_")) %>% ncol(), length(temp),
              by = df %>% select(contains("date_")) %>% ncol()
            )[.x]
        ]
      ) %>%
        bind_rows() %>%
        .[, 1, drop = TRUE]
    ) %>%
    map(~ Filter(Negate(is.null), .x)) %>%
    map(~ Filter(function(x) any(!is.na(x)), .x))
  
  # For each entity, derive first date a given menu appears in data,
  # + last date, and the menu itself. Bind it with identifiers
  out <- list()
  for (i in seq(nrow(df))) {
    indv <- df[i, ] %>%
      select(contains("date_")) %>%
      map(~ paste(na.omit(unlist(.x)), collapse = "-")) %>%
      unlist() %>%
      .[. != ""] %>%
      .[. != "NA"] %>%
      na.omit()
    
    out[[i]] <- seq(unique(rleid(indv))) %>%
      set_names(., .) %>%
      map(
        ~ tibble(
          min = names(indv)[min(which(.x == rleid(indv)))],
          max = names(indv)[max(which(.x == rleid(indv)))],
          amount = df[
            i, names(indv)[min(which(.x == rleid(indv)))],
            drop = TRUE
          ] %>%
            unlist() %>%
            paste0(collapse = "-")
        )
      ) %>%
      bind_rows() %>%
      arrange(min) %>%
      row_seq(row = "seq_url")
    
    out[[i]] <- bind_cols(
      df[i, ] %>%
        select(-contains("date_")) %>%
        slice(rep(1:n(), each = nrow(out[[i]]))),
      out[[i]]
    )
  }
  return(
    out %>%
      bind_rows() %>%
      mutate(across(c("min", "max"), ~ ymd(gsub("date|_", "", .x)))) %>%
      group_by(
        across(setdiff(names(.), c("min", "max", "amount", "url", "seq_url")))
      ) %>%
      group_split() %>%
      map_dfr(
        ~ .x %>%
          arrange(min) %>%
          row_seq(row = "seq")
      ) %>%
      ungroup() %>%
      arrange(across(all_of(c(setdiff(order_vars, "url"), "min"))))
  )
}

fed_exception_vars <- function(x) {
  if (x == "president") {
    ex <- c("last_name", "url", "year")
  } else if (x == "senate") {
    ex <- c("state", "last_name", "url", "year")
  } else {
    ex <- c("state", "state_cd", "last_name", "url", "year")
  }
  return(ex)
}

summ_calc_fxn <- function(df) {
  df %>%
    rename(min_date = min, max_date = max) %>%
    mutate(
      amount = case_when(
        amount == "0-1-2-3" ~ NA_character_,
        amount == "-999" ~ NA_character_,
        amount == "No\nSuggested\nAmounts" ~ NA_character_,
        TRUE ~ gsub("^0-", "", amount)
      )
    ) %>%
    {
      bind_rows(
        filter(., is.na(amount)) %>%
          rowwise() %>%
          mutate(
            min = NA, max = NA, mean = NA, median = NA,
            q1 = NA, q3 = NA, first = NA, last = NA,
            choices = 0
          ),
        filter(., !is.na(amount)) %>%
          rowwise() %>%
          mutate(
            min = amount_split(amount) %>% min(., na.rm = TRUE),
            max = amount_split(amount) %>% max(., na.rm = TRUE),
            mean = amount_split(amount) %>% mean(., na.rm = TRUE),
            median = amount_split(amount) %>% median(., na.rm = TRUE),
            q1 = amount_split(amount) %>% summary() %>% .[["1st Qu."]],
            q3 = amount_split(amount) %>% summary() %>% .[["3rd Qu."]],
            first = amount_split(amount) %>% .[1],
            last = amount_split(amount) %>% .[length(.)],
            choices = amount_split(amount) %>% length(),
            ineff_2700 = case_when(
              2700 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            ineff_2800 = case_when(
              2800 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            ineff_2900 = case_when(
              2900 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            sanders = case_when(
              27 %in% amount_split(amount) ~ 1,
              TRUE ~ 0
            ),
            beyond_max = case_when(
              !is.na(max) & max > 2800 ~ 1,
              TRUE ~ 0
            )
          )
      )
    }
}

amount_split <- function(amount) {
  out <- str_split(gsub("--999|^-999", "", amount), pattern = "-") %>%
    unlist() %>%
    as.numeric() %>%
    na.omit() %>%
    as.vector()
  if (length(out) == 0) {
    return(NA)
  } else {
    return(out)
  }
}

df_ls %>%
  map(~ assert_that(max(.x$date, na.rm = TRUE) > as.Date("2021-01-01")))

# Note that Greene, LaTurner, and Mace's WinRed links were not scraped
# due to a flaw in the scraper (when both Anedot and WinRed existed,
# scraped only Anedot)

# Create portfolio summaries ===================================================
dl <- df_ls %>%
  imap(
    ~ {
      ex <- fed_exception_vars(.y)
      .x %>%
        ungroup() %>%
        mutate(year = 2022, portfolio = as.numeric(portfolio)) %>%
        filter(!is.na(url) & url != "") %>%
        group_split(across(all_of(ex))) %>%
        map_dfr(~ portfolio_summ(.x, order_vars = ex)) %>%
        arrange(across(c(setdiff(all_of(ex), "url"), "min")))
    }
  )

dl %>% map(~ assert_that(all(.x$min <= .x$max)))

# Manual corrections ===========================================================
dl$house <- dl$house %>%
  mutate(
    max = case_when(
      last_name == "Greene" &
        url == "https://secure.winred.com/marjorie-greene-for-congress/donate" &
        amount == "25-50-100-250-500-1000-2800" ~
        # https://web.archive.org/web/20210131090143/https://secure.winred.com/marjorie-greene-for-congress/donate
        as.Date("2021-01-31"),
      TRUE ~ max
    ),
    min = case_when(
      last_name == "Greene" &
        url == "https://secure.winred.com/marjorie-greene-for-congress/donate" &
        amount == "25-50-100-250-500-1000-2900" ~
        # https://web.archive.org/web/20210205002844/https://secure.winred.com/marjorie-greene-for-congress/donate
        as.Date("2021-02-05"),
      TRUE ~ min
    )
  )

# Create summary statistics ====================================================
dl <- dl %>%
  map(
    ~ .x %>%
      mutate(
        amount = case_when(
          amount == "0-1-2-3" ~ NA_character_,
          amount == "-999" ~ NA_character_,
          TRUE ~ amount
        )
      ) %>%
      mutate(
        amount = case_when(
          last_name == "Sewell" & grepl("2020-", amount) ~
            gsub("2020-", "20.20-", amount),
          TRUE ~ amount
        )
      )
  ) %>%
  map(summ_calc_fxn) %>%
  imap(
    ~ left_join(
      .x,
      df_ls[[.y]] %>%
        select(
          c(
            starts_with("state"), ends_with("name"),
            FEC_ID_cand, party, incumbent, contains("class")
          )
        ) %>%
        dedup() %>%
        filter(!is.na(incumbent))
    )
  )

dl %>% map(~ assert_that(all(!is.na(.x$incumbent))))
save(dl, file = here("data/tidy/portfolio_summ_federal_2022.Rda"))

# Did candidates shift their maximum amounts to 2,900, if previously 2,800? ====
temp <- dl %>%
  map(
    ~ .x %>%
      ## On Feb 2, 2021, the FEC announced an increase in the individual 
      ## contributor’s donation capacity from $2,800 to $2,900 
      ## for candidate committees.
      ungroup() %>%
      filter(max_date >= as.Date("2021-02-02")) %>%
      group_by(across(c(contains("state"), contains("name"), "party"))) %>%
      # e.g. Loeffler "thank you" page after loss
      filter(!is.na(amount)) %>%
      mutate(
        adjusted_date = case_when(
          grepl("2900", amount) ~ min_date,
          TRUE ~ NA_Date_
        )
      ) %>%
      summarise(
        orig_2800 = ifelse(sum(ineff_2800, na.rm = TRUE) > 0, TRUE, FALSE),
        adjust = ifelse(
          sum(ineff_2900, na.rm = TRUE) > 0 & sum(ineff_2800, na.rm = TRUE) > 0,
          "Adjusted $2,800 to $2,900",
          "Did Not Adjust $2,800 Maximum"
        ),
        # e.g. Bryan Steil
        # added 5,800 soon after 2,900:
        # first date shifted to 2,900 should be the first one by date
        adjusted_date = min(adjusted_date, na.rm = TRUE),
        winred = ifelse(any(grepl("winred", url)), "WinRed", "Other"),
        actblue = ifelse(any(grepl("actblue", url)), "ActBlue", "Other")
      ) %>%
      mutate(
        party = factor(party, levels = c("Dem", "Rep", "Ind")),
        actblue = factor(actblue, levels = c("ActBlue", "Other")),
        winred = factor(winred, levels = c("WinRed", "Other")),
        adjusted_date = case_when(
          is.infinite(adjusted_date) ~ as.Date("2021-01-31"),
          TRUE ~ adjusted_date
        )
      ) %>%
      ungroup()
  )

## Sanity check: difference is those without any prompts
dl %>% map_dbl(~ nrow(.x %>% group_by(fec_id_cand) %>% slice(1)))
temp %>% map_dbl(nrow)

# Compare by party =============================================================
# Conditional probability by platform 
# aka how much of this is a platform effect?
tab <- cross2(c("Dem", "Rep"), c("senate", "house")) %>%
  map(
    ~ prop(
      temp[[.x[[2]]]] %>% filter(orig_2800 == TRUE & party == .x[[1]]),
      c("adjust", ifelse(.x[[1]] == "Rep", "winred", "actblue")),
      print = FALSE
    ) %>%
      rowid_matrix_to_df() %>%
      mutate(type = paste0(.x[[1]], " (", simple_cap(.x[[2]]), ")")) %>%
      select(rownames, type, everything())
  )

out <- bind_cols(
  tab[[1]] %>% select(-type) %>% rename(" " = rownames),
  tab[[2]] %>% select(-rownames, -type),
  tab[[3]] %>% select(-rownames, -type),
  tab[[4]] %>% select(-rownames, -type),
  .name_repair = "minimal"
) %>%
  xtable(align = "llll|ll|ll|ll")

addtorow <- list(
  pos = list(-1),
  command = paste0(
    tab %>%
      map_chr(
        ~ paste0("& \\multicolumn{2}{c}{", .x$type[1], "}", collapse = "")
      ) %>%
      paste0(collapse = ""),
    "\\\\"
  )
)

## Table G.1, formerly max_adjust_congress_by_platform_2022.tex
print(
  out,
  add.to.row = addtorow, hline.after = c(0),
  file = here("tab", "TableG1.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)

# When did the shift happen? 
# Conditional on switching, right after the FEC announcement on Feb 2, 2021
temp %>%
  map(~ .x %>% filter(grepl("Adjusted", adjust))) %>%
  map(
    ~ list(
      Dem = prop(
        .x %>% filter(party == "Dem"), "adjusted_date",
        sort = TRUE, head = 3, print = FALSE
      ),
      Rep = prop(
        .x %>% filter(party == "Rep"), "adjusted_date",
        sort = TRUE, head = 3, print = FALSE
      )
    )
  )

max_date <- temp %>% 
  map_dbl(~ max(.x$adjusted_date)) %>% 
  max() %>% 
  as.Date(., origin = "1970-01-01")

## 2024 addition: due to Dropbox/external hard drive issues, I lost the data
## after Jul 29, 2021. 

## Delete Cedric Richmond, who was retired Dec 2020
temp$house <- temp$house %>%
  filter(!(last_name == "Richmond" & state == "LA"))

## Jun 30 is the last date that any change occurred;
## actual last date in this remaining data is Jul 29, 2021
last_date <- as.Date(max(df_ls$senate$date))

# Visualize it: Within party by platform =======================================
p <- temp %>%
  bind_rows(.id = "Race") %>%
  filter(orig_2800 == TRUE) %>%
  mutate(
    adjust = case_when(
      grepl("Did Not", adjust) ~ 0,
      TRUE ~ 1
    ),
    platform = case_when(
      winred == "WinRed" ~ "WinRed",
      actblue == "ActBlue" ~ "ActBlue",
      TRUE ~ "Other"
    ),
    platform = factor(platform, levels = c("WinRed", "ActBlue", "Other"))
  ) %>%
  arrange(adjusted_date) %>%
  group_split(party) %>%
  `names<-`({.} %>% map(~ .x$party[1]) %>% unlist()) %>%
  imap(
    ~ {
      p <- .x %>%
        droplevels() %>%
        group_by(platform, adjusted_date) %>%
        summarise(adjust = sum(adjust), n = n()) %>%
        ungroup() %>%
        pivot_wider(
          id_cols = adjusted_date, names_from = platform, 
          values_from = c(adjust, n)
        ) %>%
        mutate(across(where(is.integer), ~ as.numeric(.x))) %>%
        mutate(
          across(
            where(is.numeric), 
            ~ case_when(is.na(.x) ~ 0, TRUE ~ .x)
          )
        )
      
      if (.y == "Dem") {
        p <- p %>%
          mutate(
            adjust_ActBlue = adjust_ActBlue / sum(n_ActBlue),
            adjust_Other = adjust_Other / sum(n_Other)
          )
      } else {
        p <- p %>%
          mutate(
            adjust_WinRed = adjust_WinRed / sum(n_WinRed),
            adjust_Other = adjust_Other / sum(n_Other)
          )
      }
      
      ## Add a vacuous row to adjust the last date
      if (.y == "Dem") {
        p <- bind_rows(
          p,
          tibble(
            adjusted_date = last_date,
            adjust_ActBlue = 0,
            adjust_Other = 0,
            n_ActBlue = 0,
            n_Other = 0
          )
        )
      } else {
        p <- bind_rows(
          p,
          tibble(
            adjusted_date = last_date,
            adjust_WinRed = 0,
            adjust_Other = 0,
            n_WinRed = 0,
            n_Other = 0
          )
        )        
      }
      
      p <- p %>%
        select(-contains("n_")) %>%
        pivot_longer(
          cols = matches("adjust_"),
          names_to = "Platform", values_to = "v"
        ) %>%
        mutate(Platform = gsub("adjust_", "", Platform)) %>%
        group_by(Platform) %>%
        arrange(adjusted_date) %>%
        mutate(cm = cumsum(v)) %>%
        ggplot(
          aes(x = adjusted_date, y = cm, colour = Platform, linetype = Platform)
        ) +
        geom_line()
      
      if (.y == "Dem") {
        p <- p +
          scale_linetype_manual(
            values = c("ActBlue" = "solid", "Other" = "twodash")
          ) +
          scale_colour_manual(
            values = c("ActBlue" = "#74add1", "Other" = "gray")
          )
      } else {
        p <- p +
          scale_linetype_manual(
            values = c("WinRed" = "solid", "Other" = "twodash")
          ) +
          scale_colour_manual(
            values = c("WinRed" = "#d73027", "Other" = "gray")
          )
      }
      p <- p +
        labs(colour = "Platform", group = "Platform", linetype = "Platform") +
        xlab("Date Adjusted to $2,900") +
        ylab("Cumulative %") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
        scale_x_date(
          breaks = "1 month", date_labels = "%b",
          limits = c(as.Date("2021-01-31"), last_date)
        )
    }
  )

p %>%
  imap(
    ~ {
      pdf(
        here("fig", paste0("adjust_2900_", .y, ".pdf")),
        width = 3.5, height = 2.5
      )
      print(
        pdf_default(.x) +
          theme(
            legend.position = c(.8, .3),
            legend.background = element_blank()
          )
      )
      dev.off()
    }
  )

## Rename files
## adjust_2900_Dem.pdf to Fig4a.pdf, adjust_2900_Rep.pdf to Fig4b.pdf

file.rename(
  here("fig", "adjust_2900_Dem.pdf"),
  here("fig", "Fig4a.pdf")
)

file.rename(
  here("fig", "adjust_2900_Rep.pdf"),
  here("fig", "Fig4b.pdf")
)
