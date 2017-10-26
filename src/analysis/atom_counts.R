library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

atom.counts <- data.table(read.csv("data/atom_counts.csv"))
colnames(atom.counts) <- sapply(colnames(atom.counts), function(s) substr(s,3,99))
atom.rates <- cbind(atom.counts[, .(project)], sapply(atom.counts[, -"project"], function(col) stdize(col / atom.counts$all.nodes)))

mrgn <- unit(c(0,.2,0,.2), "cm")
chart.project <- function (proj) {
  mat <- t(atom.rates[as.character(project)==proj][, !c("project", "all.nodes", "non.atoms")])
  df <- data.table(atom = rownames(mat), count = mat[,1])
  ggplot(data=df, aes(x=atom, y=count)) +
    geom_bar(stat="identity") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(plot.margin = mrgn) +
    labs(y=proj)
}

p1 <- chart.project('linux')
p2 <- chart.project('freebsd')
p3 <- chart.project('gcc')
p4 <- chart.project('clang')


p.labels <- ggplot(data=df, aes(x=atom, y=0)) + coord_fixed(ratio = 0) +
  theme(axis.title.y=element_text(colour="white", size=16), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.ticks.x=element_blank(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(0,.2,2,.2), "cm"))
grid.arrange(p1, p2, p3, p4, p.labels, ncol = 1)
