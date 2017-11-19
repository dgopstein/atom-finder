library(data.table)
library(stringr)
library(scales)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

file.ext <- function(file.name) strsplit(file.name, ".*\\.")[[1]][2]

#bugs.lines.csv <- data.table(read.csv("../../tmp/bug-lines-2017-09-26_merged_2_3_clean.csv", header=TRUE))
bugs.lines.csv <- data.table(read.csv("data/bug-lines_gcc_2017-11-09_02.csv", header=TRUE))
bugs.lines.csv <- bugs.lines.csv[!is.na(n.bugs),]
bugs.lines.csv$all.atoms <- bugs.lines.csv[, -c("non.atom", "file", "n.bugs", "rev.str", "all.changed")][, rowSums(.SD)]
bugs.lines.csv[, bug := n.bugs > 0]
bugs.lines.csv[, file.ext := sapply(as.character(file), file.ext)]

# Is any atom changed at all - bug vs non-bug commit
bugs.lines.csv[all.changed > 0][, .(type = factor(c(2, 1), labels=c("didnt change atoms", "changed atoms")),
                                    count = c(sum(all.atoms > 0), sum(all.atoms == 0))), by=bug] %>%
  ggplot(aes(x=bug,y=count)) + geom_col(aes(fill=type), position="fill")

# For commits that do change atoms, how many atoms do they change - bug vs non-bug
ggplot(bugs.lines.csv[all.changed > 20], aes(all.atoms)) +
  geom_density(aes(group=bug, fill=bug, alpha=0.5), adjust=0.00001) + coord_cartesian(xlim = c(0, 0.3))
  #geom_histogram(aes(group=bug, fill=bug, alpha=0.5), bins=200) + coord_cartesian(xlim = c(0, 0.1))

atom.rates.by.bug <- bugs.lines.csv[all.changed > 0, -c("non.atom", "file", "n.bugs", "rev.str", "all.atoms", "file.ext", "all.changed")
               ][, lapply(.SD, mean), by=bug]

atom.rates.bug.change <- as.data.table(t(atom.rates.by.bug[bug==TRUE, -c("bug")] / atom.rates.by.bug[bug==FALSE, -c("bug")]), keep.rownames=TRUE)
names(atom.rates.bug.change) <- c("atom", "rate")
atom.rates.bug.change$atom <- with(atom.rates.bug.change, reorder(atom, -rate))

ggplot(atom.rates.bug.change, aes(atom, rate, fill=rate > 1)) + geom_col() +
  #scale_y_sqrt() +
  theme(axis.text.x=element_text(angle=90, hjust=1))

