library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

atom.committers.gcc <- data.table(read.csv("data/atom-committers_gcc_2017-10-31_1-3.csv")); atom.committers <- atom.committers.gcc
atom.committers.linux <- data.table(read.csv("data/atom-committers_linux_2017-11-01_01.csv")); atom.committers <- atom.committers.linux

atom.committers[, rate := added.atoms/added.non.atoms]
atom.committers <- atom.committers[rev.str > 5 & added.non.atoms > 10 & rate < 1]

ggplot(atom.committers, aes(x = rev.str, y = rate)) + geom_point() + labs(x="# commits") + ggtitle("Commiter's atom rate by commits")
ggplot(atom.committers, aes(x = rev.str, y = rate)) + geom_point() + labs(x="# commits (log)") + ggtitle("Commiter's atom rate (log) by commits (log)") + scale_x_log10() + scale_y_log10()

  #stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  #scale_x_log10() + scale_y_log10() +
  #geom_density_2d()

ggplot(atom.committers, aes(x = added.non.atoms, y = rate)) + geom_point() + labs(x="# AST nodes added") + ggtitle("Commiter's atom rate by amount of code")
ggplot(atom.committers, aes(x = added.non.atoms, y = rate)) + geom_point() + labs(x="# AST nodes added (log)") + ggtitle("Commiter's atom rate (log) by amount of code (log)") + scale_x_log10() + scale_y_log10()

ggplot(atom.committers, aes(rate)) + stat_bin(bins=500) + geom_histogram()
#ggplot(atom.committers, aes(rate)) + geom_density()
