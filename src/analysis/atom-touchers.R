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

atoms <- c("post.increment", "preprocessor.in.statement", "operator.precedence", 
           "literal.encoding", "omitted.curly.braces", "pre.increment", 
           "logic.as.control.flow", "reversed.subscript", "comma.operator", 
           "type.conversion", "repurposed.variable", "macro.operator.precedence", 
           "implicit.predicate", "conditional", "assignment.as.value", "all.atoms")
atoms <- c("post.increment", "preprocessor.in.statement")

my.scatter <- function(atom) {
  ggplot(atom.touchers.sum) +
    geom_point(aes_string("non.atoms", atom)) +
    labs(title=atom)
}

my.hist <- function(atom) {
  pct <- quantile(atom.touchers.rate[[atom]], c(0.98))
  pct <- ifelse(pct > 0, pct, max(atom.touchers.rate[[atom]]))
  ggplot(atom.touchers.rate) +
    scale_x_continuous(limits = c(0.01*pct, pct)) +
    geom_histogram(aes_string(atom), bins = 50) +
    labs(title=atom)
}

grid.arrange(grobs=lapply(atoms, my.hist), ncol=5)

