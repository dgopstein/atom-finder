library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

atom.counts <- data.table(read.csv("data/atom_counts.csv"))
colnames(atom.counts) <- sapply(colnames(atom.counts), function(s) substr(s,3,99))
atom.rates <- cbind(atom.counts[, .(project)], sapply(atom.counts[, -"project"], function(col) stdize(col / atom.counts$all.nodes)))

mrgn <- unit(c(-.1,.2,0,.2), "cm")

wide.to.long <- function(wide, proj) {
  mat <- t(wide[as.character(project)==proj][, !c("project", "all.nodes", "non.atoms")])
  data.table(atom = rownames(mat), count = mat[,1])
}


chart.bar.project <- function (proj) {
  df <- wide.to.long(atom.rates, proj)
  df$zero <- 0
  ggplot(data=df, aes(atom, count)) +
    geom_bar(stat="identity") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_text(angle=0), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(plot.margin = mrgn) +
    labs(y=proj)
}

chart.spot.project <- function (proj) {
  df <- wide.to.long(atom.rates, proj)
  df$zero <- 0.0
  df$one <- 1.0
  df$count.neg <- 1.0 - df$count
  ggplot(data=df, aes(atom, zero)) +
    geom_point(aes(size = one, colour=zero)) +
    geom_point(aes(size = 0.8*count, colour=one)) +
    #scale_size_continuous(range = c(2,4)) +
    scale_colour_gradientn(colours=c("#444444", "darkolivegreen1")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_text(angle=0), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(axis.line=element_blank()) +
    theme(legend.position="none") +
    labs(y=proj) +
    theme(plot.margin = mrgn)
}

p.labels <- ggplot(data=wide.to.long(atom.rates, 'linux'), aes(x=atom, y=0)) + coord_fixed(ratio = 0) +
  theme(axis.title.y=element_text(colour="white", size=16), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.ticks.x=element_blank(), axis.text.x = element_text(size=10, angle = 90, hjust = 1)) +
  theme(plot.margin = unit(c(5.0, .2, 0, .2), "cm")) # change first number to lower bottom labels

plots <- lapply(atom.rates[1:4]$project, chart.spot.project)
pg <- do.call(plot_grid, c(plots, list(p.labels, align = "v", nrow = 17, rel_heights = c(rep(1, 15), 4.0))))
pg + theme(plot.margin = unit(c(.5,0,.5,0), "cm"))
