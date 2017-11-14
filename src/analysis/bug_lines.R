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

bugs.lines.csv[all.changed > 0][, .(type = factor(c(2, 1), labels=c("didnt change atoms", "changed atoms")), count = c(sum(all.atoms > 0), sum(all.atoms == 0))), by=bug] %>%
  ggplot(aes(x=bug,y=count)) + geom_col(aes(fill=type), position="fill")

ggplot(bugs.lines.csv[all.changed > 20], aes(all.atoms)) +
  geom_density(aes(group=bug, fill=bug, alpha=0.5), adjust=0.00001) + coord_cartesian(xlim = c(0, 0.3))
  #geom_histogram(aes(group=bug, fill=bug, alpha=0.5), bins=200) + coord_cartesian(xlim = c(0, 0.1))


# bugs.lines.csv[!is.na(n.bugs) & n.changed.non.atom.lines > 0,
#                .(atom = mean(n.atom.lines, na.rm=T),
#                  non.atom = mean(n.non.atom.lines, na.rm=T),
#                  changed.atom = mean(n.changed.atom.lines, na.rm=T),
#                  changed.non.atom = mean(n.changed.non.atom.lines, na.rm=T),
#                  changed = mean(n.changed.lines, na.rm=T)), by=c('bug')]
#
# bugs.by.ext.long <- bugs.lines.csv[!is.na(n.bugs), .(count = .N) ,by=c("bug", "file.ext")]
# bugs.by.ext <- reshape(bugs.by.ext.long, idvar = "file.ext", timevar = "bug", direction = "wide")[order(-count.FALSE)]
#
# bugs.atoms.by.ext.long <- bugs.lines.csv[!is.na(n.bugs),
#                     .(count = .N,
#                       atom = mean(n.atom.lines, na.rm=T),
#                       non.atom = mean(n.non.atom.lines, na.rm=T),
#                       changed.atom = mean(n.changed.atom.lines, na.rm=T),
#                       changed.non.atom = mean(n.changed.non.atom.lines, na.rm=T)
#                       ) ,by=c("bug", "file.ext")]
#
# bugs.atoms.by.ext <- reshape(bugs.atoms.by.ext.long, idvar = "file.ext", timevar = "bug", direction = "wide")[order(-count.FALSE)]
# bugs.atoms.by.ext
# #write.csv(bugs.atoms.by.ext, file = "bugs-atoms-by-ext_2017-09-28.csv")
