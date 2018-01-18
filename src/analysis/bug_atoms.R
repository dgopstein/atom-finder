library(data.table)
library(stringr)
library(scales)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

atoms.removed <- data.table(read.csv("data/atoms-in-bugs_gcc_2018-01-11_removed.csv_partial", header=TRUE))
atoms.added   <- data.table(read.csv("data/atoms-in-bugs_gcc_2018-01-11_added.csv_partial", header=TRUE))

atoms.removed[, bug := n.bugs > 0]
atoms.added[, bug := n.bugs > 0]

atoms.removed[is.na(atoms.removed)] <- 0
atoms.added[is.na(atoms.added)] <- 0

atoms.removed[, all.atoms.removed := n.removed - removed.non.atoms] 
atoms.added[, all.atoms.added := n.added - added.non.atoms] 

atoms.removed[, any.atoms.removed := (all.atoms.removed > 0)]
atoms.added[, any.atoms.added := (all.atoms.added > 0)]

atoms.removed.contingency <- atoms.removed[, .(count = mean(all.atoms.removed / n.removed, na.rm = TRUE)), by=c('bug')]
atoms.added.contingency <- atoms.added[, .(count = mean(all.atoms.added / n.added, na.rm = TRUE)), by=c('bug')]

added.removed.contingency <- merge(atoms.removed.contingency, atoms.added.contingency, by='bug', suffixes = c('.removed', '.added'))
added.removed.contingency[, .(count.removed / count.added), by='bug']

colnames(atoms.added)

added.removed.wide <- merge(atoms.removed, atoms.added, suffixes = c('.removed', '.added'), by=c('author.name', 'author.email', 'file', 'n.bugs', 'added.non.atoms', 'n.added', 'rev.str', 'removed.non.atoms', 'n.removed', 'bug'))

atoms.removed.sums <- atoms.removed[, -c('author.name', 'author.email', 'file', 'rev.str')][, lapply(.SD, sum), by='bug']
atoms.added.sums <- atoms.added[, -c('author.name', 'author.email', 'file', 'rev.str')][, lapply(.SD, sum), by='bug']

#################
# Type conversion is one of the most-removed atoms in non.bug.fix commits. Why?
#################
atoms.removed.sums[, .(bug, type.conversion, n.removed, rate = type.conversion/n.removed)]
atoms.added.sums[, .(bug, type.conversion, n.added, rate = type.conversion/n.removed)]

atoms.removed[type.conversion > 0, .(rev.str, file, bug, n.added, n.removed, all.atoms.removed, type.conversion, rate = type.conversion/n.removed)]
atoms.added[type.conversion > 0, .(rev.str, file, bug, n.added, n.removed, all.atoms.added, type.conversion, rate = type.conversion/n.added)]


#################

atoms.removed.rate <- atoms.removed.sums[, lapply(.SD, function(x) x / n.removed), by='bug']
atoms.added.rate <- atoms.added.sums[, lapply(.SD, function(x) x / n.added), by='bug']

removed.rate <- data.table(t(mapply(function(rem, add) rem/add, atoms.removed.rate, atoms.added.rate)), keep.rownames = TRUE)
colnames(removed.rate) <- c('atom', 'non.bug', 'bug')
removed.rate[, rate := bug/non.bug]
removed.rate

intercept <- 1
ggplot(removed.rate[!(atom %in% c("bug","n.bugs","any.atoms.removed","n.removed",'added.non.atoms','n.added'))], aes(x = reorder(atom, rate), y = rate)) +
  geom_segment(aes(y = intercept,yend = rate, xend = atom, color=ifelse(atom %in% c("removed.non.atoms",'all.atoms.removed'), "3", rate>intercept)),
               show.legend=F, size=4) +
  geom_text(color="black", size=3, aes(label=round(ifelse(rate >= intercept, rate, 1/rate), digits=2), hjust = ifelse(rate >= intercept, -.3, 1.5))) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  geom_hline(yintercept=intercept) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.4)) +
  scale_y_log10(expand = c(0.1,0.2)) +
  annotate("text", x=16.5, y=0.12, label='bold("Non-bug")', parse=TRUE, hjust=-0.05, size=5.0) +
  annotate("text", x=16.5, y=2, label='bold("Bug")', parse=TRUE, hjust=-0.05, size=5.0) +
  labs(title="Atoms removed more in...", x="Relative Rate", y="Atom") +
  coord_flip()

?geom_segment

