## Load packages + functions for project


library(plyr)
library(reshape2)
library(ggplot2)
library(rstan)
library(brms)


source("./scripts/functions.R")

dir.create("./outputs", showWarnings = FALSE)
dir.create("./figures", showWarnings = FALSE)

cols <- as.vector(palette.colors(palette = "Okabe-Ito"))
theme_set(theme_bw())
