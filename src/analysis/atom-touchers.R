# Which committers own atoms in master

library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

atom.touchers.csv <- data.table(read.csv("data/atom-touchers_2018-01-05_01.csv_partial"))
atom.touchers.csv[is.na(atom.touchers.csv)] <- 0

atom.touchers.sum <- atom.touchers.csv[, -c('file', 'email')][, as.list(c(lapply(.SD, sum), all.atoms = base::sum(.SD[,-c('non.atoms')]), n.file = .N)), by=name]

ggplot(atom.touchers.sum) +
  geom_point(aes(non.atoms, all.atoms)) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')

summary(lm(all.atoms ~ non.atoms, atom.touchers.sum))

atom.touchers.rate <- atom.touchers.sum[, .SD[, -c('name', 'non.atoms', 'n.file')] / non.atoms]
atom.touchers.rate

hist(atom.touchers.rate[all.atoms < 0.05]$all.atoms, breaks=seq(0,0.05,l=20))

atom.touchers.sum[all.atoms == 0, max(non.atoms)]
atom.touchers.sum[all.atoms > 0, max(non.atoms)]

atom.touchers.sum[non.atoms > 10000 & all.atoms == 0,]

atom.touchers.csv[name=="David Zhang"]

