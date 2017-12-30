library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

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
atom.rates.wide <- data.table(cbind(atom.counts[, .(project, domain)], atom.rates.nums))[, -c("all.nodes")]

atom.key.order <- tail(names(atom.count.nums), -2)
atom.display.order <- unlist(atom.name.conversion[atom.key.order])

atom.rates <- data.table(melt(atom.rates.wide[,-c("non.atoms")], id.vars=c("project", "domain"), variable.name="atom", value.name = "rate"))
atom.rates[, atom := convert.atom.names(atom)]

atom.rate.per.project <- ggplot(data=atom.rates, aes(project, atom)) +
  geom_point(colour="black", aes(size=1)) +
  geom_point(colour="white", aes(size=0.8)) +
  geom_point(aes(size = 0.81*rate, colour=domain)) +
  scale_size_continuous(range = c(-.4,6)) +
  scale_colour_manual(values = sap.qualitative.palette) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.4), axis.ticks.x=element_blank()) +
  theme(axis.ticks.y=element_blank(), axis.title.y=element_blank()) +
  theme(axis.line=element_blank()) +
  theme(legend.position="none") +
  scale_y_discrete(limits=rev(atom.display.order)) +
  scale_x_discrete(limits=proj.order) +
  labs(x="Project") +
  ggtitle("Atom Rate Per Project")

ggsave("img/atom_rate_per_project.pdf", atom.rate.per.project, width=(width<-132), height=width*0.92, units = "mm")

##################################
#     Clustered Spot Matrix
##################################
atom.rates.mat <- as.matrix(atom.rates.wide[,-c("project","domain", "non.atoms")])
rownames(atom.rates.mat) <- atom.rates.wide$project

h <- heatmap(atom.rates.mat)

proj.to.domain <- as.list(as.character(proj.domain))
names(proj.to.domain) <- proj.order

atom.rates.clustered <- data.table(melt(atom.rates.mat[h$rowInd,h$colInd], varnames=c("project", "atom"), value.name = "rate"))
atom.rates.clustered$domain <- unlist(proj.to.domain[as.character(atom.rates.clustered$project)])

#atom.rate.per.project.clustered <-
  ggplot(data=atom.rates.clustered, aes(project, atom)) +
  geom_point(colour="black", aes(size=1)) +
  geom_point(colour="white", aes(size=0.8)) +
  geom_point(aes(size = 0.81*rate, colour=domain)) +
  scale_size_continuous(range = c(-.4,6)) +
  scale_colour_manual(values = sap.qualitative.palette) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.4), axis.ticks.x=element_blank()) +
  theme(axis.ticks.y=element_blank(), axis.title.y=element_blank()) +
  theme(axis.line=element_blank()) +
  theme(legend.position="none") +
  #scale_y_discrete(limits=rev(atom.display.order)) +
  # scale_x_discrete(limits=proj.order) +
  labs(x="Project")

############################
#  all projects combined
############################
library(dplyr)
all.atom.counts <- atom.counts[, -c('project','domain')][, lapply(.SD, sum)]
all.atom.rates.wide <- all.atom.counts[, -c('all.nodes', 'non.atoms')] / all.atom.counts$all.nodes
all.atom.rates <- data.frame(atom = unlist(atom.name.conversion[names(all.atom.rates.wide)]), rate = t(all.atom.rates.wide))

atom.occurrence.rate <- ggplot(all.atom.rates, aes(x = reorder(atom, -rate), y = rate)) + geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.4)) +
  labs(x="Atom", y="Occurrence Rate")

ggsave("img/atom_occurrence_rate.pdf", atom.occurrence.rate, width=(width<-130), height=width*0.88, units = "mm")

#################################
#  all atoms by effect size
##################################

atom.names <- unlist(atom.name.conversion[c("assignment.as.value", "comma.operator", "conditional", "implicit.predicate", "literal.encoding", "logic.as.control.flow",
  "macro.operator.precedence", "omitted.curly.braces", "operator.precedence", "post.increment", "pre.increment", "preprocessor.in.statement",
  "repurposed.variable", "reversed.subscript", "type.conversion")])

effect.size <- c(0.52, 0.30, 0.36, 0.24, 0.63, 0.48, 0.53, 0.22, 0.33, 0.45, 0.28, 0.54, 0.22, 0.40, 0.42)

atom.effect <- merge(all.atom.rates, cbind.data.frame(atom = atom.names, effect.size))
confusingness.vs.prevalence.correlation <- with(atom.effect, cor(rate, effect.size)) # correlation: -0.45

confusingness.vs.prevalence <-
  ggplot(atom.effect, aes(effect.size, rate)) +
  geom_point(size=2.5, color=sap.qualitative.palette[5]) +
  geom_smooth(method="lm", se=FALSE, fullrange=TRUE, color=sap.qualitative.palette[1]) + #, aes(color="Exp Model"), formula= (y ~ x^2+1)) +
  scale_x_continuous(limits = c(0.2, 0.75)) +
  scale_y_log10(expand = c(0.2, 0)) +
  geom_text(aes(label=atom), hjust=-0.1, angle=-14, size=3) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  annotate("text", x=0.55, y=0.01, label=paste0("r = ", round(confusingness.vs.prevalence.correlation, 2))) +
  #ggtitle("Confusingness vs Prevalence", subtitle="Do less confusing patterns occur more often?") +
  labs(x="Effect Size\n(amount of confusion)", y="Occurrence Rate\n[log scaled]")

ggsave("img/confusingness_vs_prevalence.pdf", confusingness.vs.prevalence, width=(width<-150), height=width*0.89, units = "mm")

################################################
#  all projects by raw confusion of C question
#     (not, the difference between C/NC)
################################################

## from snippet_study/results.R
# dput(atom.contingencies[, .(atom.name, correct.rate.C = round((TT + TF) / (TT + TF + FT + FF), digits=2))][order(atom.name)][,correct.rate.C])

correct.rate.C <- c(0.45, 0.48, 0.76, 0.78, 0.25, 0.3, 0.57, 0.62, 0.75, 0.54, 0.64, 0.3, 0.47, 0.52, 0.58)
atom.correct.C <- merge(all.atom.rates, cbind.data.frame(atom = atom.names, correct.rate.C))
with(atom.correct.C, cor(rate, 1-correct.rate.C))

ggplot(atom.correct.C, aes(rate, correct.rate.C)) + geom_point() +
  geom_text(aes(label=atom), hjust=-0.1, angle=45, size=2) +
  #geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ x^2+1)) +
  theme(axis.text.x=element_text(angle=90, hjust=1))




