library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

atom.counts <- data.table(read.csv("data/atom_counts.csv"))
colnames(atom.counts) <- sapply(colnames(atom.counts), function(s) substr(s,3,99))
proj.order <- c("linux", "freebsd", "gecko-dev", "webkit",
  "gcc", "clang", "mongo", "mysql-server", "subversion", "git",
  "emacs", "vim", "httpd", "nginx")
proj.domain <- factor(c("os", "os", "browser", "browser", "compiler", "compiler", "db", "db", "vcs", "vcs", "editor", "editor", "webserver", "webserver"),
                      levels=c("os", "browser", "compiler", "db", "vcs", "editor", "webserver"),
                      ordered=TRUE)
atom.counts <- atom.counts[match(proj.order, atom.counts$project),]
atom.counts$domain <- proj.domain
atom.count.nums <- atom.counts[, -c("project")][, order(-colSums(atom.counts[, -c("project", "domain")])), with=FALSE]
atom.rates.nums <- sapply(atom.count.nums, function(col) stdize(col / atom.counts$all.nodes))
atom.rates <- cbind(atom.counts[, .(project, domain)], atom.rates.nums)

wide.to.long <- function(wide, proj) {
  mat <- t(wide[as.character(project)==proj][, !c("project", "domain", "all.nodes", "non.atoms")])
  data.table(atom = rownames(mat), count = mat[,1])
}


mrgn <- unit(c(-.7,.2,-.7,.2), "cm")
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

# https://experience.sap.com/fiori-design-web/values-and-names/
sap.qualitative.palette <- c('#5cbae6', '#b6d957', '#fac364', '#8cd3ff', '#d998cb', '#f2d249', '#93b9c6')

atom.count.order <- scale_x_discrete(limits=tail(names(atom.rates), -4)) # for ggplot sort the columns by their position in the dataframe

proj <- 'httpd'
chart.spot.project <- function (proj) {
  df <- wide.to.long(atom.rates, proj)
  df$zero <- 0.0
  df$one <- 1.0
  df$count.neg <- 1.0 - df$count
  domain <- atom.rates[as.character(project)==proj]$domain
  df$size <- 1 # log(1*atom.count.sums + 1)
  ggplot(data=df, aes(atom, zero)) +
    geom_point(aes(size = size, colour=zero)) +
    geom_point(aes(size = size*1.05*count, colour=one)) +
    scale_size_continuous(range = c(-.4,6)) +
    scale_colour_gradientn(colours=c("#222222", sap.qualitative.palette[domain])) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_text(angle=0), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(axis.line=element_blank()) +
    theme(legend.position="none") +
    labs(y=proj) +
    theme(plot.margin = mrgn) +
    atom.count.order
}

p.labels <- ggplot(data=wide.to.long(atom.rates, 'linux'), aes(x=atom, y=0)) + coord_fixed(ratio = 0) +
  theme(axis.title.y=element_text(colour="white"), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.ticks.x=element_blank(), axis.text.x = element_text(size=10, angle = 90, hjust = 1, vjust = 0.3)) + # vjust shifts atoms labels left/right
  theme(plot.margin = unit(c(0.0, 0, 0, 0), "cm")) + # change first number to lower bottom labels
  theme(axis.line=element_blank()) +
  atom.count.order

# t, r, b, l
atom.count.sums <- tail(colSums(atom.count.nums), -2)
count.hist.df <- atom.count.sums # log(0.02*atom.count.sums)
count.hist <-
  ggplot(data=data.frame(atom = names(count.hist.df), count = count.hist.df), aes(x = atom, y = count, group = 1)) +
  geom_line(stat = "identity", colour="#222222") +
  geom_area(fill="#98aafb") +
  geom_point(size=1, fill="black") +
  coord_cartesian(ylim = c(-.2*max(count.hist.df), 1.1*max(count.hist.df))) + # don't cut off top of point
  theme(axis.title.y=element_text(angle=0), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.line=element_blank()) +
  theme(legend.position="none") +
  theme(plot.margin = unit(c(-0.1,.2,0,.2), "cm")) +
  labs(y="raw count") +
  atom.count.order

plots <- lapply(atom.rates$project, chart.spot.project)
#pg <- do.call(plot_grid, c(plots, list(count.hist, p.labels, align = "v", nrow = 16, rel_heights = c(rep(1, 14), 1.5, 6.0))))
pg <- do.call(plot_grid, c(plots, list(p.labels, align = "v", nrow = 15, rel_heights = c(rep(1, 14), 6.0))))
pg + theme(plot.margin = unit(c(.5,0,.5,0), "cm"))
