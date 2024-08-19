renv::init()
install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()
install_github(
  "wch/extrafont", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install_github(
  "wch/fontcm", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)

# Install typically used libraries
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("assertthat")
install.packages("janitor")
install.packages("data.table")
install.packages("xtable")
install.packages("styler")
install.packages("fastLink")
install.packages("lmtest")
install.packages("estimatr")
install.packages("stargazer")

# For panel
install.packages("future")
install.packages("PanelMatch")
install.packages("panelView")
install.packages("fixest")
install.packages("caret")
install.packages("fastDummies")

# Text
install.packages("quanteda")
install_github(
  "quanteda/quanteda.corpora",
  INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install.packages("keyATM")
install.packages("stm")
install.packages("tidytext")

renv::snapshot()