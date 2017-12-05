library(data.table)
library(stringr)
library(scales)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

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

density.bugs.lines <- bugs.lines.csv[all.changed > 100]
density.mean.rate <- mean(bugs.lines.csv$all.atoms)

colors2 <- sap.qualitative.palette[c(3,4)]

# For commits that do change atoms, how many atoms do they change - bug vs non-bug
# i.e. what is the composition of the commit, is the commit 50% atoms, or 1% atoms?
atom.rate.probability.by.bug <- ggplot(density.bugs.lines, aes(all.atoms)) +
  geom_density(aes(group=bug, fill=bug), size=1, alpha=.6, adjust=1.5, n=8192) +
  coord_cartesian(xlim = c(0, .38)) +
  geom_segment(aes(x=density.mean.rate,xend=density.mean.rate,y=0,yend=4.5)) +
  annotate("text", x=0.035, y=4.4, label="Mean atom rate", hjust=0) +
  scale_fill_manual(values = colors2, labels=c("Non-bug", "Bug"), name="Commit type") +
  labs(title="Atom composition of commits",
     subtitle="How many of the AST nodes in the commits are atoms",
     x="Fraction of edited AST nodes that are atoms",
     y="Probability Density accross all commits")

ggsave("img/atom_rate_probability_by_bug.pdf", atom.rate.probability.by.bug, width=(width<-180), height=width*0.6, units = "mm")


atom.rates.by.bug <- bugs.lines.csv[all.changed > 0, -c("non.atom", "file", "n.bugs", "rev.str", "all.atoms", "file.ext", "all.changed")
               ][, lapply(.SD, mean), by=bug]

# How many samples do we have for each atom?
bugs.lines.csv[all.changed > 0, -c("non.atom", "file", "n.bugs", "rev.str", "all.atoms", "file.ext", "all.changed")
               ][, lapply(.SD, function(x) sum(x > 0)), by=bug]

atom.rates.bug.change <- as.data.table(t(atom.rates.by.bug[bug==TRUE, -c("bug")] / atom.rates.by.bug[bug==FALSE, -c("bug")]), keep.rownames=TRUE)
names(atom.rates.bug.change) <- c("atom", "rate")
atom.rates.bug.change[, atom := unlist(atom.name.conversion[atom])]
atom.rates.bug.change$atom <- with(atom.rates.bug.change, reorder(atom, rate))

atom.bug.rate <- ggplot(atom.rates.bug.change[!is.nan(rate)], aes(atom, rate)) +
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

ggsave("img/atom_bug_rate.pdf", atom.bug.rate, width=(width<-140), height=width*0.6, units = "mm")


## The least frequent, most bug-predictive atoms
## ** uses data from atom_counts.R
# atom.counts.bugs <- merge(all.atom.rates, atom.rates.bug.change, by="atom", suffixes=c(".count", ".bug"))
#
# ggplot(atom.counts.bugs, aes(rate.count, rate.bug)) +
#   geom_point() + geom_text(aes(label=atom)) +
#   scale_x_sqrt() + scale_y_sqrt()
