library(data.table)
library(stringr)
library(scales)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

file.ext <- function(file.name) strsplit(file.name, ".*\\.")[[1]][2]

#bugs.lines.csv <- data.table(read.csv("../../tmp/bug-lines-2017-09-26_merged_2_3_clean.csv", header=TRUE))
bugs.lines.csv <- data.table(read.csv("data/bug-lines_gcc_2017-11-09_combined.csv", header=TRUE))
bugs.lines.csv <- bugs.lines.csv[!is.na(n.bugs),]
bugs.lines.csv$all.atoms <- bugs.lines.csv[, -c("non.atom", "file", "n.bugs", "rev.str", "all.changed")][, rowSums(.SD)]
bugs.lines.csv[, bug := n.bugs > 0]
bugs.lines.csv[, file.ext := sapply(as.character(file), file.ext)]

#View(bugs.lines.csv[all.changed > 1 & all.atoms > 0 & bug])
bugs.lines.csv[rev.str == "3b106d3b1cc260082ada69d12f65a5f7d547ac77"]


# Is any atom changed at all - bug vs non-bug commit
bugs.lines.csv[all.changed > 1][, .(type = factor(c(2, 1), labels=c("didnt change atoms", "changed atoms")),
                                    count = c(sum(all.atoms > 0), sum(all.atoms == 0))), by=bug] %>%
  ggplot(aes(x=bug,y=count)) + geom_col(aes(fill=type), position="fill")

# For commits that do change atoms, how many atoms do they change - bug vs non-bug
# i.e. what is the composition of the commit, is the commit 50% atoms, or 1% atoms?
ggplot(bugs.lines.csv[all.changed > 100], aes(all.atoms)) +
  geom_density(aes(group=bug, fill=bug, alpha=0.5), adjust=1.5, n=8192) + coord_cartesian(xlim = c(0, .6)) +
  #geom_histogram(aes(group=bug, fill=bug, alpha=0.5), bins=200) + coord_cartesian(xlim = c(0, 0.1))
  labs(title="Atom composition of commits",
     subtitle="How many of the AST nodes in the commits are atoms")

atom.rates.by.bug <- bugs.lines.csv[all.changed > 0, -c("non.atom", "file", "n.bugs", "rev.str", "all.atoms", "file.ext", "all.changed")
               ][, lapply(.SD, mean), by=bug]

atom.rates.bug.change <- as.data.table(t(atom.rates.by.bug[bug==TRUE, -c("bug")] / atom.rates.by.bug[bug==FALSE, -c("bug")]), keep.rownames=TRUE)
names(atom.rates.bug.change) <- c("atom", "rate")
atom.rates.bug.change$atom <- with(atom.rates.bug.change, reorder(atom, -rate))

ggplot(atom.rates.bug.change, aes(atom, rate)) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_segment(aes(y = 1,
                   x = atom,
                   yend = rate,
                   xend = atom)) +
  geom_point(stat='identity', size=6, aes(color=rate>1), show.legend=F)  +
  geom_text(color="black", size=2, aes(label=round(rate, digits=2))) +
  labs(title="Correlation of individual atoms with bugs",
       subtitle="Relative to all other AST nodes") +
  ylim(-0.1, 3.5) #+  coord_flip()

## The least frequent, most bug-predictive atoms
## ** uses data from atom_counts.R
# atom.counts.bugs <- merge(all.atom.rates, atom.rates.bug.change, by="atom", suffixes=c(".count", ".bug"))
#
# ggplot(atom.counts.bugs, aes(rate.count, rate.bug)) +
#   geom_point() + geom_text(aes(label=atom)) +
#   scale_x_sqrt() + scale_y_sqrt()
