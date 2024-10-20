#Call Methods:

#setup 
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  ggplot2,
  dplyr, 
  usethis)

install.packages("groundhog")
library("groundhog")
pkgs <- c("rio","metafor")
groundhog.library(pkgs, "2021-09-01")

update.packages(ask = FALSE, checkBuilt = TRUE)