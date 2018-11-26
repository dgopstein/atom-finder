library(data.table)
library(stringr)
library(scales)
library(ggplot2)
library(extrafont)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

operator.spacing <- data.table(read.csv("data/operator-spacing_2018-11-26_03_individual-exprs.csv", header=TRUE))

operator.spacing
operator.spacing[, .(sum(mixed.spacing), sum(confusing.spacing), sum(nested.binary))]
