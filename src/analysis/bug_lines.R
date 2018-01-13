library(data.table)
library(stringr)
library(scales)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

file.ext <- function(file.name) strsplit(file.name, ".*\\.")[[1]][2]

#bugs.lines.csv <- data.table(read.csv("data/bug-lines_gcc_2017-11-09_combined.csv", header=TRUE))
bugs.lines.csv <- data.table(read.csv("data/bug-lines_gcc_2018-01-04_03.csv", header=TRUE))
nrow(bugs.lines.csv)
bugs.lines.csv <- bugs.lines.csv[!is.na(n.bugs),]
bugs.lines.csv$all.atoms <- bugs.lines.csv[, -c("non.atom", "file", "n.bugs", "rev.str", "all.changed")][, rowSums(.SD)]
bugs.lines.csv[, bug := n.bugs > 0]
bugs.lines.csv[, file.ext := sapply(as.character(file), file.ext)]

#View(bugs.lines.csv[all.changed > 1 & all.atoms > 0 & bug])
bugs.lines.csv[rev.str == "3b106d3b1cc260082ada69d12f65a5f7d547ac77"]

atom.existence.by.bugs <- bugs.lines.csv[all.changed > 0][, .(type = factor(c(2, 1), labels=c("No", "Yes")),
                                                              count = c(sum(all.atoms > 0), sum(all.atoms == 0))), by=bug]
# effect size - phi
X2 <- atom.existence.by.bugs[, chisq.test(count)]
p.value <- X2$p.value
es.phi <- atom.existence.by.bugs[, sqrt(X2$statistic/sum(count))]
list(p.value = p.value, es.phi = es.phi)

# Is any atom changed at all - bug vs non-bug commit
p <- ggplot(atom.existence.by.bugs, aes(x=bug,y=count)) +
  geom_col(aes(fill=type), position="fill") +
  scale_fill_manual(values = rev(colors2)) +
  labs(title="Commits that edited atoms", x="Bug-fix Commit", y="Rate") +
  theme(legend.position = c(1.0, 0.7), plot.margin = unit(c(5,40,1,0), "mm")) +
  annotate("text", x=2.6, y=0.35, label='bold("p-value      < 1e-10")', parse=TRUE, hjust=-0.05, size=4.0) +
  annotate("text", x=2.6, y=0.23, label=paste0('bold("Effect Size φ: ', round(es.phi, 2), '")'), parse=TRUE, hjust=-0.05, size=4.0) +
  guides(fill=guide_legend(title="Edited Atoms"))

no.clip(p)

################################
#   Atom existence mosaic
################################
library(Cairo)
# library(ggmosaic)
# atom.existence.by.bugs[, type := factor(type, levels = rev(levels(type)))]
 # ggplot(atom.existence.by.bugs, aes(x=bug,y=count)) +
 #   geom_mosaic(aes(weight = count, x = product(type, bug), fill=type))

atom.existence.widths <- atom.existence.by.bugs[, .(width=sum(count)), by=bug][, .(bug, width = width/sum(width))]
atom.existence.by.bugs <- merge(atom.existence.by.bugs, atom.existence.widths, by="bug")

# Atom edit rate for buggy commits, also for non-buggy commits
atom.existence.by.bugs[bug==TRUE & type=='Yes', count] / atom.existence.by.bugs[bug==TRUE, sum(count)]
atom.existence.by.bugs[bug==FALSE & type=='Yes', count] / atom.existence.by.bugs[bug==FALSE, sum(count)]

atom.existence.by.bugs[bug==TRUE, sum(count)]
atom.existence.by.bugs[bug==FALSE, sum(count)]


p <- ggplot(atom.existence.by.bugs, aes(x=bug,y=count, width=1.93*width)) +
  theme_classic() +
  geom_bar(aes(fill=type), position = "fill", stat = "identity", colour="white", lwd = 1.5) +
  coord_cartesian(xlim = c(0.7, 1.7)) +
  scale_fill_manual(values = rev(colors2)) +
  #labs(title="Commits that edited atoms") +
  labs(x="Bug-fix Commit") +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = c(1.16, 0.7), plot.margin = unit(c(5,40,1,1), "mm")) +
  annotate("text", x=2.3, y=0.35, label='bold("p-value      < 1e-10")', parse=TRUE, hjust=-0.05, size=4.0) +
  annotate("text", x=2.3, y=0.25, label=paste0('bold("Effect Size φ: ', round(es.phi, 2), '")'), parse=TRUE, hjust=-0.05, size=4.0) +
  guides(fill=guide_legend(title="Edited Atoms"))

atom.existence.by.bugs.mosaic <- no.clip(p)
ggsave("img/atom_existence_by_bugs_mosaic.pdf", atom.existence.by.bugs.mosaic, width=(width<-130), height=width*0.7, units = "mm", device=cairo_pdf)

################################
#   Atom density in commits
################################

density.bugs.lines <- bugs.lines.csv[all.changed > 0 & all.atoms > 0 & all.atoms < 1]
hist(density.bugs.lines[bug==TRUE]$all.atoms, breaks = 20)
hist(density.bugs.lines[bug==FALSE]$all.atoms, breaks = 20)

density.bugs.lines[bug==TRUE, median(all.atoms)]
density.bugs.lines[bug==FALSE, median(all.atoms)]

density.bugs.t <- t.test(density.bugs.lines[bug==FALSE,all.atoms],density.bugs.lines[bug==TRUE,all.atoms], alternative = "less")
density.bugs.t$p.value
lsr::cohensD(density.bugs.lines[bug==FALSE,all.atoms],density.bugs.lines[bug==TRUE,all.atoms])

# For commits that do change atoms, how many atoms do they change - bug vs non-bug
# i.e. what is the composition of the commit, is the commit 50% atoms, or 1% atoms?
# atom.rate.probability.by.bug <-
#   ggplot(density.bugs.lines, aes(all.atoms)) +
#   #geom_segment(aes(x=density.mean.atoms.no.bug,xend=density.mean.atoms.no.bug,y=0,yend=3.15)) +
#   geom_density(aes(group=bug, fill=bug), size=1, alpha=.6, adjust=1.5, n=8192) +
#   #geom_segment(aes(x=density.mean.atoms.bug,xend=density.mean.atoms.bug,y=0,yend=3.7)) +
#   coord_cartesian(xlim = c(0, .38)) +
#   #geom_segment(aes(x=density.mean.rate,xend=density.mean.rate,y=0,yend=4.5)) +
#   #annotate("text", x=0.035, y=4.4, label="Mean atom rate", hjust=0) +
#   scale_fill_manual(values = colors2, labels=c("Non-bug-fix", "Bug-fix"), name="Commit type") +
#   theme(legend.position = c(0.65, 0.7)) +
#   labs(#title="Atom composition of commits",
#      #subtitle="How many of the AST nodes in the commits are atoms",
#      x="Rate of atoms per AST node in commit edits",
#      y="Probability")
#
# ggsave("img/atom_rate_probability_by_bug.pdf", atom.rate.probability.by.bug, width=(width<-130), height=width*0.8, units = "mm")

################################
#   Bug Rates by Commit size
################################
library(zoo)
lines.with.bugs    <- bugs.lines.csv[bug == TRUE, all.atoms > 0]
lines.without.bugs <- bugs.lines.csv[bug == FALSE]
#
# smoothed.bugs <- rollmean(zoo(lines.with.bugs$all.changed, lines.with.bugs$all.atoms), 10)
# smoothed.no.bugs <- rollmean(zoo(lines.without.bugs$all.changed, lines.without.bugs$all.atoms), 10)
#
# ggplot() +
#   geom_smooth(data = fortify.zoo(smoothed.bugs), aes(Index, smoothed.bugs), color="green", se=FALSE) +
#   geom_smooth(data = fortify.zoo(smoothed.no.bugs), aes(Index, smoothed.no.bugs), se=FALSE) +
#   coord_cartesian(xlim = c(0, 12))

ggplot() +
  geom_line(data = lines.with.bugs[, .(all.atoms=median(all.atoms)), by=all.changed], aes(all.changed, all.atoms)) +
  geom_line(data = lines.without.bugs[, .(all.atoms=mean(all.atoms)), by=all.changed], aes(all.changed, all.atoms), color = "green") +
  coord_cartesian(xlim = c(0, 200))
lines.without.bugs[, mean(all.atoms), by=all.changed]

lines.with.bugs[, .(all.atoms=median(all.atoms)), by=all.changed][order(all.changed)]

################################
#     Bug Rates by Atom (Relative)
################################

atom.rates.by.bug <- bugs.lines.csv[all.changed > 0, -c("file", "n.bugs", "rev.str", "all.atoms", "file.ext", "all.changed")
               ][, lapply(.SD, mean), by=bug]

# How many samples do we have for each atom?
atom.counts.by.bug <- bugs.lines.csv[all.changed > 0, -c("file", "n.bugs", "rev.str", "all.atoms", "file.ext", "all.changed")
               ][, lapply(.SD, function(x) sum(x > 0)), by=bug]

atom.rates.bug.change <- as.data.table(t(atom.rates.by.bug[bug==TRUE, -c("bug")] / atom.rates.by.bug[bug==FALSE, -c("bug")]), keep.rownames=TRUE)
atom.rates.bug.change <- merge(atom.rates.bug.change, as.data.table(t(atom.counts.by.bug), keep.rownames=T), by="rn")
names(atom.rates.bug.change) <- c("atom", "rate", "count.no.bug", "count.bug")
atom.rates.bug.change[, atom := convert.atom.names(atom)]
atom.rates.bug.change$atom <- with(atom.rates.bug.change, reorder(atom, rate))

atom.bug.rate.relative <- ggplot(atom.rates.bug.change[!is.nan(rate) & atom!="Non-Atom"], aes(atom, rate)) +
  #theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.4)) +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks=element_blank(), axis.line.x=element_blank()) +
  geom_segment(aes(y = 1, x = atom, yend = rate, xend = atom, color=rate>1), show.legend=F, size=5) +
  geom_text(color="black", size=3, aes(label=round(rate, digits=2), hjust = ifelse(rate >= 1, -.3, 1.5))) +
  geom_hline(yintercept=1) +
  labs(title="Correlation of atoms with bugs",
       subtitle="Relative to all other AST nodes",
       x="Atom", y="Relative rate of atoms in bug fixes") +
  scale_y_continuous(limits=c(-0.2, 3.5)) +
  scale_colour_manual(values = colors2) +
  coord_flip()

ggsave("img/atom_bug_rate_relative.pdf", atom.bug.rate.relative, width=(width<-140), height=width*0.6, units = "mm")


################################
#   Bug Rates by Atom (Absolute)
################################

atom.rates.bug.change[, count := count.no.bug + count.bug]
atom.bug.rate <-
  ggplot(atom.rates.bug.change, aes(atom, rate)) +
  theme_gray() +
  geom_bar(aes(fill = atom!="Non-Atom", width=0.11*log(0.18*count)), stat="identity") +
  #geom_text(aes(label=ifelse(is.na(odds), '', sprintf('%0.2f', round(odds, 2)))), angle = 90, vjust=0.5, hjust=-0.3, size=3) +
  #coord_cartesian(ylim = c(0, 0.13)) +
  scale_fill_manual(values = colors2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #labs(title = "Comment Rates Inside Functions") +
  guides(fill=FALSE) +
  labs(x = "Atom", y="Bug-fix Commit Rate")

ggsave("img/atom_bug_rate.pdf", atom.bug.rate, width=(width<-140), height=width*0.7, units = "mm")


## The least frequent, most bug-predictive atoms
## ** uses data from atom_counts.R
# atom.counts.bugs <- merge(all.atom.rates, atom.rates.bug.change, by="atom", suffixes=c(".count", ".bug"))
#
# ggplot(atom.counts.bugs, aes(rate.count, rate.bug)) +
#   geom_point() + geom_text(aes(label=atom)) +
#   scale_x_sqrt() + scale_y_sqrt()

