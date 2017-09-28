library(data.table)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

file.ext <- function(file.name) strsplit(file.name, ".*\\.")[[1]][2]

bugs.lines.csv <- data.table(read.csv("../../tmp/bug-lines-2017-09-26_merged_2_3_clean.csv", header=TRUE))
bugs.lines.csv[, bug := n.bugs > 0]
bugs.lines.csv[, file.ext := sapply(as.character(file), file.ext)]
bugs.lines.csv

bugs.lines.csv[!is.na(n.bugs) & n.changed.non.atom.lines > 0,
               .(atom = mean(n.atom.lines, na.rm=T),
                 non.atom = mean(n.non.atom.lines, na.rm=T),
                 changed.atom = mean(n.changed.atom.lines, na.rm=T),
                 changed.non.atom = mean(n.changed.non.atom.lines, na.rm=T),
                 changed = mean(n.changed.lines, na.rm=T)), by=c('bug')]

bugs.by.ext.long <- bugs.lines.csv[!is.na(n.bugs), .(count = .N) ,by=c("bug", "file.ext")]
bugs.by.ext <- reshape(bugs.by.ext.long, idvar = "file.ext", timevar = "bug", direction = "wide")[order(-count.FALSE)]

bugs.atoms.by.ext.long <- bugs.lines.csv[!is.na(n.bugs),
                    .(count = .N,
                      atom = mean(n.atom.lines, na.rm=T),
                      non.atom = mean(n.non.atom.lines, na.rm=T),
                      changed.atom = mean(n.changed.atom.lines, na.rm=T),
                      changed.non.atom = mean(n.changed.non.atom.lines, na.rm=T)
                      ) ,by=c("bug", "file.ext")]

bugs.atoms.by.ext <- reshape(bugs.atoms.by.ext.long, idvar = "file.ext", timevar = "bug", direction = "wide")[order(-count.FALSE)]
bugs.atoms.by.ext
write.csv(bugs.atoms.by.ext, file = "bugs-atoms-by-ext_2017-09-28.csv")
