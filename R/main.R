# Install the packages as it was last replicated + use here package ============
## Uncheck the following lines if the packages are not installed
# install.packages("renv")
# install.packages("here")
# renv::restore()

## If renv::restore() failes, use remotes package after uncommenting
# install.packages("remotes")
# library(remotes) ## 2.5.0
# install_version("tidytext", version = "0.4.2")
# install_version("stm", version = "1.3.7")
# install_version("keyATM", version = "0.5.2")
# install_version("quanteda", version = "4.0.2")
# install_version("fastLink", version = "0.6.1")
# install_version("scales", version = "1.3.0")
# install_version("estimatr", version = "1.0.4")
# install_version("stargazer", version = "5.2.3")
# install_version("xtable", version = "1.8-4")
# install_version("broom", version = "1.0.6")
# install_version("sandwich", version = "3.1-0")
# install_version("lmtest", version = "0.9-40")
# install_version("zoo", version = "1.8-12")
# install_version("gridExtra", version = "2.3")
# install_version("magrittr", version = "2.0.3")
# install_version("fixest", version = "0.12.1")
# install_version("Kmisc", version = "0.2.0")
# install_version("PanelMatch", version = "2.2.0")
# install_version("panelView", version = "1.1.18")
# install_version("fastDummies", version = "1.7.4")
# install_version("caret", version = "6.0-94")
# install_version("lattice", version = "0.22-6")
# install_version("data.table", version = "1.16.0")
# install_version("janitor", version = "2.2.0")
# install_version("jsonlite", version = "1.8.8")
# install_version("assertthat", version = "0.2.1")
# install_version("here", version = "1.0.1")
# install_version("rvest", version = "1.0.4")
# install_version("lubridate", version = "1.9.3")
# install_version("forcats", version = "1.0.0")
# install_version("stringr", version = "1.5.1")
# install_version("dplyr", version = "1.1.4")
# install_version("purrr", version = "1.0.2")
# install_version("readr", version = "2.1.5")
# install_version("tidyr", version = "1.3.1")
# install_version("tibble", version = "3.2.1")
# install_version("ggplot2", version = "3.5.1")
# install_version("tidyverse", version = "2.0.0")
# install_version("plyr", version = "1.8.9")

## None-CRAN packages
# install_github(
#   "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
# )
# install_github(
#   "quanteda/quanteda.corpora",
#   INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
# )

# Run scripts ==================================================================
source(here::here("R", "01_descriptives.R"))
source(here::here("R", "02_prediction_exercise.R"))
source(here::here("R", "03_platform_adjustment.R"))
source(here::here("R", "04_blog_text_analysis.R"))
source(here::here("R", "05_panel_benchmark.R"))
source(here::here("R", "06_synthetic_panel.R"))
source(here::here("R", "07_heterogeneous_effects.R"))
source(here::here("R", "08_placebo_corporate.R"))
source(here::here("R", "09_recurring_donations.R"))
