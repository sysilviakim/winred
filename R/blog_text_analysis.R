source(here::here("R", "utilities.R"))
set.seed(12345)

template <- c(
  actblue = "actblue", winred = "winred",
  actblue_v2 = "actblue_v2", winred_v2 = "winred_v2"
)

# Import scraped outputs =======================================================
blog <- list(
  actblue = loadRData(
    max(list.files(
      path = here("data", "raw"), pattern = "actblue_blog_co", full.names = TRUE
    ))
  ),
  winred = loadRData(
    max(list.files(
      path = here("data", "raw"), pattern = "winred_blog_co", full.names = TRUE
    ))
  )
)

## No content in this particular post
blog$actblue$`2013-not-bad-if-we-do-say-so-ourselves` <- NULL

## merch order manual filter
blog$winred$christmas$modified <- paste0(
  "Gear up for Christmas with the official WinRed Christmas catalog. ",
  "We're bringing you tons of festive merch options you can sell directly to ",
  "your donors to boost your end-of-year fundraising program. ",
  "Get the Catalog. How It Works WinRed's zero-touch e-commerce operation ",
  "gives you the ability to sell festive campaign merchandise without having ",
  "to buy in bulk. We leverage a direct-to-garment merchandise process, ",
  "meaning there are no upfront costs. When an order comes in, WinRed takes ",
  "the cost of the product out of the donation and you keep the rest. ",
  "We take care of everything from printing to shipping to customer service ",
  "— all you need to do is send us your design files."
)

## to make things searchable
blog_df <- blog %>%
  map(
    ~ .x %>%
      map_chr("modified") %>%
      as_tibble()
  )

# Descriptives for data section ================================================
## 419 actblue, 108 winred
blog %>% map_dbl(length)

## in the same period, 91 posts
blog$actblue %>%
  ## as.Date(parse_date_time(blog$winred[[1]]$date, orders = "mdy"))
  keep(~ ymd(.x$date) >= as.Date("2019-06-23")) %>%
  length()

# Corpus prep ==================================================================
corpus_all <- blog %>%
  map(
    function(x) {
      x %>%
        map("modified") %>%
        ## clean nonalphanumberic, clean extra spaces
        map(~ str_replace_all(.x, "[^[:alnum:]]", " ")) %>%
        map(~ gsub("\\s+", " ", str_trim(.x))) %>%
        map_chr(~.x) %>%
        corpus()
    }
  )

tokens_all <- corpus_all %>%
  map(
    ~ tokens(.x) %>%
      tokens(
        remove_url = TRUE,
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_separators = TRUE,
        include_docvars = TRUE
      ) %>%
      tokens_tolower() %>%
      tokens_remove(
        c(
          stopwords("english"), ## quanteda,
          stop_words$word ## tidytext
        )
      ) %>%
      tokens_remove(stopwords("spanish")) %>%
      tokens_remove(letters) %>%
      ## no tokens_compound
      tokens_wordstem() %>%
      tokens_select(min_nchar = 3)
  )

## to quickly view; doesn't take `min_docfreq` into account
tokens_filtered <- tokens_all %>%
  map(
    ~ as_tibble(table(unlist(.x)), .name_repair = "unique") %>%
      rename(token = `...1`) %>%
      filter(n >= 5) %>%
      arrange(desc(n))
  )

dfm_all <- tokens_all %>%
  map(dfm) %>%
  map(~ dfm_trim(.x, min_termfreq = 5, min_docfreq = 2))

dfm_all

# Not keyword assisted; stm without covariates =================================
stm_all <- dfm_all %>%
  map(~ stm(.x, K = 5, verbose = FALSE, init.type = "Spectral"))

## top keywords: topic categorization?
stm_all %>% map(visualize_word_list)

## note that there are also FREX, Lift, and Score words
stm_all %>% map(summary)

## semantic coherence
stm_all %>%
  imap(~ semanticCoherence(.x, dfm_all[[.y]]))

## semantic coherence: average
stm_all %>%
  imap_dbl(~ mean(semanticCoherence(.x, dfm_all[[.y]])))


# Keyword assisted topic models ================================================
dfm_all %>% map(~ topfeatures(x = .x, n = nfeat(.x) * 0.1))

## keyatm keywords -------------------------------------------------------------
keywords_stemmed <- list(
  winred = list(
    party =
      c(
        "donald", "trump", "republican", "gop", "chairman",
        "party", "partisan", "endorse", "conservative"
      ) %>%
      corpus() %>% tokens() %>% tokens_wordstem() %>% unlist(),
    report = c("total", "amount", "rais", "top", "number", "report")
  ),
  actblue = list(
    party =
      c(
        "barack", "obama", "democrat", "chairman",
        "party", "partisan", "endorse", "progressive", "liberal"
      ) %>%
      corpus() %>% tokens() %>% tokens_wordstem() %>% unlist(),
    report = c("total", "amount", "rais", "top", "number", "report")
  ),
  winred_v2 = list(
    party =
      c("republican", "gop", "party", "endorse", "conservative") %>%
      corpus() %>% tokens() %>% tokens_wordstem() %>% unlist(),
    report = c("total", "amount", "rais", "top", "number", "report")
  ),
  actblue_v2 = list(
    party =
      c("democrat", "party", "endorse", "progressive", "liberal") %>%
      corpus() %>% tokens() %>% tokens_wordstem() %>% unlist(),
    report = c("total", "amount", "rais", "top", "number", "report")
  )
)

## Read texts ------------------------------------------------------------------
keyatm_doc_all <- dfm_all %>%
  map(
    ~ keyATM_read(
      text = dfm_subset(.x, ntoken(.x) > 0),
      keep_docnames = TRUE
    )
  )

keyatm_doc_all$actblue_v2 <- keyatm_doc_all$actblue
keyatm_doc_all$winred_v2 <- keyatm_doc_all$winred

## Visualize keyword frequencies -----------------------------------------------
key_viz_list <- template %>%
  map(
    ~ visualize_keywords(
      docs = keyatm_doc_all[[.x]],
      keywords = keywords_stemmed[[.x]]
    )
  )
key_viz_list
key_viz_list %>% map(values_fig)

## Choice of K -----------------------------------------------------------------
keyatm_out_all <- template %>%
  map(
    ~ seq(0, 8) %>%
      set_names(., paste0("K+", .)) %>%
      map(
        function(x) keyATM(
          docs = keyatm_doc_all[[.x]],
          no_keyword_topics = x,
          keywords = keywords_stemmed[[.x]],
          model = "base",
          options = list(seed = 12345)
        )
      )
  )

semantic_all <- keyatm_out_all %>%
  imap(
    ~ .x %>%
      map_dbl(function(x) mean(semantic_coherence(x, keyatm_doc_all[[.y]])))
  )

p <- 
  tibble(actblue = semantic_all$actblue_v2, winred = semantic_all$winred_v2) %>%
  mutate(K = 2:10) %>%
  pivot_longer(!K, names_to = "Platform", values_to = "value") %>% 
  ggplot(aes(x = K, y = value, group = Platform, color = Platform)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = platform_color) + 
  labs(
    title = "Semantic Coherence of KeyATM Models",
    x = "Number of Topics",
    y = "Semantic Coherence"
  ) +
  theme_minimal()

## Legend position bottom left
pdf_default(p) + theme(legend.position = c(0.15, 0.15))

## Figure F.1, previously named semantic_coherence_K.pdf
ggsave(here("fig", "FigF1.pdf"), width = 6, height = 4)

## Run keyATM model ------------------------------------------------------------
keyatm_out_all <- template %>%
  map(
    ~ keyATM(
      docs = keyatm_doc_all[[.x]],
      no_keyword_topics = 3,
      keywords = keywords_stemmed[[.x]],
      model = "base",
      options = list(seed = 12345)
    )
  )

# Diagnostics ==================================================================
## Word list -------------------------------------------------------------------
## Export
## Table F.4(a), renamed from actblue_keyatm_k5_top10.tex
print(
  xtable(
    top_words(keyatm_out_all$actblue, show_keyword = FALSE) %>%
      set_names(
        ., c(
          "Party", "Reporting", "Platform Features",
          "Mobilization", "Recurring Donations"
        )
      )
  ),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "TableF4a.tex")
)

## Table F.4(b), renamed from winred_keyatm_k5_top10.tex
print(
  xtable(
    top_words(keyatm_out_all$winred, show_keyword = FALSE) %>%
      set_names(
        ., c(
          "Party", "Reporting", "Platform Features",
          "Mobilization", "Conservative Values"
        )
      )
  ),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "TableF4b.tex")
)

## Table 2(a), formerly actblue_keyatm_k5_top10_v2.tex
print(
  xtable(
    top_words(keyatm_out_all$actblue_v2, show_keyword = FALSE) %>%
      set_names(
        ., c(
          "Party", "Reporting", "Campaign Management",
          "Recurring Donations", "Small Donations"
        )
      )
  ),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "Table2a.tex")
)

## Table 2(b), formerly winred_keyatm_k5_top10_v2.tex
print(
  xtable(
    top_words(keyatm_out_all$winred_v2, show_keyword = FALSE) %>%
      set_names(
        ., c(
          "Party", "Reporting", "Platform Features",
          "Mobilization", "Conservative Values"
        )
      )
  ),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "Table2b.tex")
)

## Top documents ---------------------------------------------------------------
## https://keyatm.github.io/keyATM/articles/pkgdown_files/keyATM_base.html
top_docs_all <- keyatm_out_all %>% map(top_docs)

blog$actblue[top_docs_all$actblue$`1_party`[1]][[1]]$modified
blog$actblue[top_docs_all$actblue$`1_party`[2]][[1]]$modified
blog$actblue[top_docs_all$actblue$`1_party`[3]][[1]]$modified

blog$winred[top_docs_all$winred$`1_party`[1]][[1]]$modified
blog$winred[top_docs_all$winred$`1_party`[2]][[1]]$modified
blog$winred[top_docs_all$winred$`1_party`[3]][[1]]$modified

blog$actblue[top_docs_all$actblue$`2_report`[1]][[1]]$modified
blog$winred[top_docs_all$winred$`2_report`[1]][[1]]$modified

blog$actblue[top_docs_all$actblue_v2$`1_party`[1]][[1]]$modified
blog$actblue[top_docs_all$actblue_v2$`1_party`[2]][[1]]$modified
blog$actblue[top_docs_all$actblue_v2$`1_party`[3]][[1]]$modified

blog$winred[top_docs_all$winred_v2$`1_party`[1]][[1]]$modified
blog$winred[top_docs_all$winred_v2$`1_party`[2]][[1]]$modified
blog$winred[top_docs_all$winred_v2$`1_party`[3]][[1]]$modified

blog$actblue[top_docs_all$actblue_v2$`2_report`[1]][[1]]$modified
blog$winred[top_docs_all$winred_v2$`2_report`[1]][[1]]$modified

## Model fit -------------------------------------------------------------------
p_list <- keyatm_out_all %>%
  map(plot_modelfit) %>%
  map(
    ~ .x$figure +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(values = c("#244082", "#732982"))
  )

## Figure F.2(a), formerly keyatm_actblue_model_fit_v2.pdf
print(pdf_default(p_list$actblue_v2))
ggsave(here("fig", "FigF2a.pdf"), width = 5, height = 3)

## Figure F.2(b), formerly keyatm_winred_model_fit_v2.pdf
print(pdf_default(p_list$winred_v2))
ggsave(here("fig", "FigF2b.pdf"), width = 5, height = 3)

## Alpha -----------------------------------------------------------------------
p_list <- keyatm_out_all %>%
  map(plot_alpha) %>%
  map(
    ~ {
      out <- .x$figure
      out$data <- out$data %>%
        mutate(
          Topic = case_when(Topic == "1_party" ~ "Party", TRUE ~ "Reporting")
        )
      return(out)
    }
  )

## Figure F.2(c), formerly keyatm_actblue_alpha_v2.pdf
print(plot_notitle(pdf_default(p_list$actblue_v2)))
ggsave(here("fig", "FigF2c.pdf"), width = 5, height = 3)

## Figure F.2(d), formerly keyatm_winred_alpha_v2.pdf
print(plot_notitle(pdf_default(p_list$winred_v2)))
ggsave(here("fig", "FigF2d.pdf"), width = 5, height = 3)

## Pi --------------------------------------------------------------------------
p_list <- keyatm_out_all %>%
  map(plot_pi) %>%
  map(
    ~ {
      out <- .x$figure +
        scale_y_continuous(limits = c(0, .25))
      out$data <- out$data %>%
        mutate(
          Topic = case_when(Topic == "1_party" ~ "Party", TRUE ~ "Reporting")
        )
      return(out)
    }
  )

## Probability of words drawn from keyword topic-word distribution
## Figure F.4(a), formerly keyatm_actblue_pi.pdf
print(plot_notitle(pdf_default(p_list$actblue)))
ggsave(here("fig", "FigF4a.pdf"), width = 2.5, height = 2.5)

## Figure F.4(b), formerly keyatm_winred_pi.pdf
print(plot_notitle(pdf_default(p_list$winred)))
ggsave(here("fig", "FigF4b.pdf"), width = 2.5, height = 2.5)

## Semantic coherence but for keyATM -------------------------------------------
semantic_all <- template %>%
  set_names(., .) %>%
  map(~ semantic_coherence(keyatm_out_all[[.x]], keyatm_doc_all[[.x]]))

semantic_df <- semantic_all %>%
  bind_rows(.id = "Platform") %>%
  set_names(
    ., c(
      "Platform", "Party", "Report", "No-keyword Topic 1",
      "No-keyword Topic 2", "No-keyword Topic 3"
    )
  ) %>%
  mutate(
    V2 = case_when(grepl("v2", Platform) ~ "v2", TRUE ~ "v1"),
    Platform = case_when(
      grepl("actblue", Platform) ~ "ActBlue",
      TRUE ~ "WinRed"
    )
  )

## Figure F.2(e), formerly semantic_coherence_keyatm_v2.tex
print(
  xtable(semantic_df %>% filter(V2 == "v2") %>% select(-V2)),
  include.rownames = FALSE, booktabs = TRUE, floating = FALSE,
  file = here("tab", "FigF2e.tex")
)

## Figure 2 of keyATM Mar 2021 draft -------------------------------------------

## proportion of keywords
prop_keyword <- keyatm_doc_all %>%
  imap_dfr(
    function(x, y) {
      x$W_raw %>%
        map_dfr(
          ~ tibble(
            party = .x %>%
              ## for each keyword in document
              map_dbl(function(z) z %in% keywords_stemmed[[y]][["party"]]) %>%
              sum(),
            report = .x %>%
              map_dbl(function(z) z %in% keywords_stemmed[[y]][["report"]]) %>%
              sum()
          ) %>%
            mutate(all = length(.x))
        )
    },
    .id = "platform"
  ) %>%
  mutate(party = party / all, report = report / all) %>%
  select(-all) %>%
  pivot_longer(!platform, names_to = "topic", values_to = "value") %>%
  mutate(
    V2 = case_when(grepl("v2", platform) ~ "v2", TRUE ~ "v1"),
    platform =
      case_when(grepl("actblue", platform) ~ "ActBlue", TRUE ~ "WinRed"),
    Topic = case_when(topic == "party" ~ "Party", TRUE ~ "Reporting")
  )

## density plot (keyATM fig 2 left panel)
p_list <- list(
  v1 = ggplot(prop_keyword %>% filter(V2 == "v1")),
  v2 = ggplot(prop_keyword %>% filter(V2 == "v2"))
) %>%
  map(
    ~ .x +
      stat_density(
        aes(x = value, group = Topic, color = Topic, linetype = Topic),
        geom = "line", position = "identity", linewidth = 1.2
      ) +
      facet_wrap(~platform) +
      scale_color_manual(values = topic_color) +
      theme_bw() +
      ylab("Density") +
      xlab("")
  )

## Figure F.5(a), formerly prop_keywords_doc.pdf
print(pdf_default(p_list[["v1"]]) + theme(legend.position = c(0.85, 0.75)))
ggsave(here("fig", "FigF5a.pdf"), width = 5, height = 3)

## Figure 3(a), formerly prop_keywords_doc_v2.pdf
print(pdf_default(p_list[["v2"]]) + theme(legend.position = c(0.85, 0.75)))
ggsave(here("fig", "Fig3a.pdf"), width = 5, height = 3)

## number of unique keywords by document
unique_keywords <- keyatm_doc_all[c("actblue", "winred")] %>%
  map(
    function(x) {
      x$W_raw %>%
        map_dbl(~ length(unique(.x))) %>%
        as_tibble()
    }
  ) %>%
  bind_rows(.id = "Platform") %>%
  mutate(
    Platform = case_when(
      grepl("actblue", Platform) ~ "ActBlue",
      TRUE ~ "WinRed"
    )
  )

## summary
unique_keywords %>%
  group_by(Platform) %>%
  summarise(value = mean(value))


p <- ggplot(unique_keywords) +
  stat_density(
    aes(x = value, group = Platform, color = Platform, linetype = Platform),
    geom = "line", position = "identity", linewidth = 1.2
  ) +
  scale_color_manual(values = platform_color) +
  theme_bw() +
  ylab("Density") +
  xlab("")

## Figure F.5(b), formerly number_unique_keywords_doc.pdf
print(pdf_default(p) + theme(legend.position = c(0.85, 0.75)))
ggsave(here("fig", "FigF5b.pdf"), width = 5, height = 3)

unique_keywords_v2 <- keyatm_doc_all[c("actblue_v2", "winred_v2")] %>%
  map(
    function(x) {
      x$W_raw %>%
        map_dbl(~ length(unique(.x))) %>%
        as_tibble()
    }
  ) %>%
  bind_rows(.id = "Platform") %>%
  mutate(
    Platform = case_when(
      grepl("actblue", Platform) ~ "ActBlue",
      TRUE ~ "WinRed"
    )
  )

p <- ggplot(unique_keywords_v2) +
  stat_density(
    aes(x = value, group = Platform, color = Platform, linetype = Platform),
    geom = "line", position = "identity", linewidth = 1.2
  ) +
  scale_color_manual(values = platform_color) +
  theme_bw() +
  ylab("Density") +
  xlab("")

## Figure 3(b), formerly number_unique_keywords_doc_v2.pdf
print(pdf_default(p) + theme(legend.position = c(0.85, 0.75)))
ggsave(here("fig", "Fig3b.pdf"), width = 5, height = 3)

# keyATM plot ==================================================================
keyatm_out_all$winred_v2$theta
keyatm_out_all$winred_v2$theta["launch", ]

keyatm_obj_all <- keyatm_out_all %>%
  imap(
    ~ {
      out <- .x$theta %>%
        as_tibble(rownames = "doc_id") %>%
        mutate(number = row_number())
      if (grepl("actblue", .y)) {
        out$date <- blog[[gsub("_v2", "", .y)]] %>%
          map_chr("date") %>%
          ymd()
      } else {
        out$date <- blog[[gsub("_v2", "", .y)]] %>%
          map_chr("date") %>%
          mdy()
      }
      out <- out %>%
        select(number, date, everything()) %>%
        mutate(month = round_date(date, unit = "3 months"))
      return(out)
    }
  )

keyatm_obj_all %>%
  imap(
    ~ {
      .x %>%
        group_by(month) %>%
        summarize(
          party = mean(`1_party`, na.rm = TRUE)
        ) %>%
        print()
    }
  )

keyatm_obj_all_2 <- keyatm_obj_all
keyatm_obj_all_2[["actblue_truncated"]] <- keyatm_obj_all$actblue %>%
  filter(date >= min(keyatm_obj_all$winred$date))
keyatm_obj_all_2[["actblue_truncated_v2"]] <- keyatm_obj_all$actblue_v2 %>%
  filter(date >= min(keyatm_obj_all$winred$date))

p_list <- keyatm_obj_all_2 %>%
  imap(
    ~ {
      out <- .x %>%
        group_by(month) %>%
        summarise(party = mean(`1_party`, na.rm = TRUE)) %>%
        ggplot() +
        geom_col(
          aes(x = month, y = party),
          colour = platform_color[[.y]], fill = platform_color[[.y]]
        ) +
        xlab("Year") +
        ylab("Avg. Topic Proportion for Party") +
        scale_y_continuous(limits = c(0, 1)) +
        scale_x_date(breaks = "6 months", date_labels = "%b\n%Y")
      return(out)
    }
  )

## Figure F.3(a), formerly actblue_blog_post_party_keyatm_full_v2.pdf
pdf(here("fig", "FigF3a.pdf"), width = 4, height = 3)
print(pdf_default(
  p_list$actblue_v2 + scale_x_date(breaks = "2 years", date_labels = "%Y\n")
))
dev.off()

## Figure F.3(b), formerly actblue_blog_post_party_keyatm_v2.pdf
pdf(here("fig", "FigF3b.pdf"), width = 4, height = 3)
print(pdf_default(p_list$actblue_truncated_v2))
dev.off()

## Figure F.3(c), formerly winred_blog_post_party_keyatm_full_v2.pdf
pdf(here("fig", "FigF3c.pdf"), width = 4, height = 3)
print(pdf_default(p_list$winred_v2))
dev.off()

## Summary
summary(keyatm_obj_all$actblue_v2$`1_party`) 
summary(keyatm_obj_all$winred_v2$`1_party`) 

keyatm_obj_all$actblue_v2 %>%
  filter(date >= min(keyatm_obj_all$winred_v2$date)) %>%
  .$`1_party` %>%
  summary()
