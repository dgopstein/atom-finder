library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

stdize <- function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

atom.counts <- data.table(read.csv("data/atom-counts_2018-09-05_for-debugging-esem.csv"))

#colnames(atom.counts) <- sapply(colnames(atom.counts), function(s) substr(s,3,99))
proj.order <- c("linux", "freebsd", "gecko-dev", "webkit",
  "gcc", "clang", "mongo", "mysql-server", "subversion", "git",
  "emacs", "vim", "httpd", "nginx")
proj.domain <- factor(c("os", "os", "browser", "browser", "compiler", "compiler", "db", "db", "vcs", "vcs", "editor", "editor", "webserver", "webserver"),
                      levels=domain.levels,
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

atom.rates[atom=='Reversed Subscripts']
sum(atom.counts[, reversed.subscript])


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
atom.rates.clustered$domain <- factor(unlist(proj.to.domain[as.character(atom.rates.clustered$project)]), domain.levels)
atom.rates.clustered[, atom := convert.atom.names(atom)]

clustered.project.order <- rev(rownames(atom.rates.mat[h$rowInd,]))
clustered.atom.order <- rev(convert.atom.names(colnames(atom.rates.mat[,h$colInd])))

atom.rate.per.project.clustered <-
  ggplot(data=atom.rates.clustered, aes(project, atom)) +
  theme_classic() +
  geom_point(colour="black", aes(size=1)) +
  geom_point(colour="white", aes(size=0.8)) +
  geom_point(aes(size = 0.81*rate, colour=domain)) +
  scale_size_continuous(range = c(-.4,7)) +
  scale_colour_manual(values = domain.colors) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.4), axis.ticks.x=element_blank()) +
  theme(axis.ticks.y=element_blank(), axis.title.y=element_blank()) +
  theme(axis.line=element_blank()) +
  theme(legend.position="none") +
  scale_y_discrete(limits=clustered.atom.order) +
  scale_x_discrete(limits=clustered.project.order) +
                   #, labels=paste(clustered.project.order, substring(proj.to.domain[clustered.project.order],1,3), sep=' - ')) +
  labs(x="Project")
atom.rate.per.project.clustered

ggsave("img/atom_rate_per_project_clustered.pdf", atom.rate.per.project.clustered, width=(width<-128), height=width*0.91, units = "mm")

############################
#  all projects combined
############################
library(dplyr)
all.atom.counts.by.project <- atom.counts[, .(project, all.atoms = Reduce(`+`, .SD)),.SDcols=atom.names.dot]

all.atom.counts <- atom.counts[, -c('project','domain')][, lapply(.SD, sum)]
all.atom.rates.wide <- all.atom.counts[, -c('all.nodes', 'non.atoms')] / all.atom.counts$all.nodes
all.atom.rates <- data.table(data.frame(atom = unlist(atom.name.conversion[names(all.atom.rates.wide)]), rate = t(all.atom.rates.wide)))

atom.occurrence.rate <- ggplot(all.atom.rates, aes(x = reorder(atom, rate), y = rate)) +
  theme_classic() +
  geom_bar(stat="identity", fill=colors2[1]) +
  geom_text(aes(y=0.0010, label=formatC(signif(rate,digits=2), digits=2, flag="#"),
                color=atom %in% c('Omitted Curly Brace','Operator Precedence')), angle=0, hjust=0) +
  theme(#axis.text.x=element_text(angle=90, hjust=1, vjust=.4), axis.text.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank()) +
  scale_y_continuous(limits = c(0.0,0.0073)) +
  guides(color=FALSE) +
  coord_flip() +
  scale_color_manual(values=c('black', 'white')) +
  labs(x="Atom", y="Occurrence Rate")
atom.occurrence.rate

ggsave("img/atom_occurrence_rate.pdf", atom.occurrence.rate, width=(width<-140), height=width*0.7, units = "mm")

# overall atom rate for paper
all.atom.ast.rate <- all.atom.counts[, (all.nodes - non.atoms) / all.nodes]
1/all.atom.ast.rate

#################################
#  all atoms by effect size
##################################

atom.effect <- data.table(merge(all.atom.rates, atom.effect.sizes[, .(atom = convert.atom.names(atom), effect.size)]))

confusingness.vs.prevalence.correlation <- with(atom.effect, cor(rate, effect.size)) # correlation: -0.45

atom.effect$offset.x <- atom.effect$offset.y <- 0
atom.effect[atom=="Preprocessor in Statement", c("offset.x", "offset.y") := .(0, .15)]
atom.effect[atom=="Conditional Operator", c("offset.x", "offset.y") := .(-1, -1.5)]
atom.effect[atom=="Comma Operator", c("offset.x", "offset.y") := .(-.5, .5)]
atom.effect[atom=="Repurposed Variable", c("offset.x", "offset.y") := .(0, -.5)]
atom.effect[atom=="Type Conversion", c("offset.x", "offset.y") := .(-3.5, -.17)]



confusingness.vs.prevalence <-
  ggplot(atom.effect, aes(effect.size, rate)) +
  theme_classic() +
  geom_point(size=2.5, color=colors2dark[2]) +
  geom_smooth(method="lm", se=FALSE, fullrange=TRUE, color=colors2dark[1], size=1) + #, aes(color="Exp Model"), formula= (y ~ x^2+1)) +
  scale_x_continuous(limits = c(0.2, 0.75)) +
  scale_y_log10(limits = c(5*10^-8, 9*10^-3)) +
  geom_text(aes(label=atom, x=.009+effect.size+.003*offset.x, y=rate+0.0001*offset.y), hjust=0, vjust=.6, angle=-15, size=3) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  annotate("text", x=0.35, y=3*10^-6, label=paste0("r = ", round(confusingness.vs.prevalence.correlation, 2))) +
  #ggtitle("Confusingness vs Prevalence", subtitle="Do less confusing patterns occur more often?") +
  labs(x="Effect Size", y="Occurrence Rate (log)")
confusingness.vs.prevalence

ggsave("img/confusingness_vs_prevalence.pdf", confusingness.vs.prevalence, width=(width<-150), height=width*0.6, units = "mm")

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

################################################
#    atom count vs LOC in project
################################################

# cat ~/atom-finder/file_sizes_sorted.txt | sed 's,/home/dgopstein/opt/src/atom-finder/\([^/]*\)/,\1 ,' | ruby -lane 'BEGIN{h=Hash.new{|x| 0}}; count, proj, _ = $_.split; h[proj] += count.to_i; END{ p h}'
proj.loc <- data.table(proj=c("clang", "freebsd", "gcc", "gecko-dev", "linux", "mongo", "webkit", "emacs", "git", "subversion", "vim", "mysql-server", "nginx", "httpd"),
  loc=c(1969346, 20252205, 5450514, 11380215, 22626962, 3864455, 4954408, 480268, 253422, 707786, 451820, 2979215, 186760, 317717))

loc.rate <- merge(proj.loc, atom.counts, by.x="proj", by.y="project")
ggplot(loc.rate, aes(loc, atom.rate)) +
  geom_point() +
  scale_x_log10()

################################################
# average atoms per line, and lines per atom
################################################

# github.com/AlDanial/cloc v 1.80  T=801.49 s (640.5 files/s, 141127.1 lines/s)
# ----------------------------------------------------------------------------------------
# Language                              files          blank        comment           code
# ----------------------------------------------------------------------------------------
# C                                     85648        5324180        5970700       29452420
# C++                                   53421        2226119        2239534       12034944
# C/C++ Header                          74057        2152838        4175043       11390728

atom.finder.corpus.sloc <- 29452420 + 12034944 + 11390728
total.n.atoms <- sum(all.atom.counts[, -c('all.nodes', 'non.atoms')])
total.n.atoms

# line rates for paper
atoms.per.line <- total.n.atoms/atom.finder.corpus.sloc
lines.per.atom <- 1/atoms.per.line

################################################
#     combined atom counts per project
################################################
all.atom.proj.rates <- atom.counts[, -c('non.atoms')][, .(rate = (base::sum(.SD) - all.nodes) / all.nodes), by=c('project', 'domain')]

all.atom.proj.rates.plot <- ggplot(all.atom.proj.rates, aes(x = reorder(project, rate), y = rate)) +
  theme_classic() +
  theme(plot.margin = margin(l=18, unit="mm")) +
  geom_bar(stat="identity", aes(fill=domain)) +
  scale_fill_manual(values=domain.colors) +
  geom_text(aes(y=0.0005, label=sprintf("%0.3f", round(rate, digits=3))),
                color='black', angle=0, hjust=0, size=2.5) +
  theme(axis.text.x=element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), axis.title.x = element_blank()) +
  theme(axis.text.y=element_text(margin=margin(r=-7,"pt"), vjust=0.4)) +
  theme(legend.position = c(0.87, 0.36), legend.key.size = unit(0.58,"line")) +
  guides(color=FALSE) +
  coord_flip() +
  labs(x="Project", fill="Domain")
all.atom.proj.rates.plot

ggsave("img/all_atom_proj_rates.pdf", all.atom.proj.rates.plot, width=(width<-130), height=width*.3, units = "mm")

########################################
#   Plot of Atom Effect Size (for slide deck)
########################################
atom.effect
ggplot(atom.effect, aes(reorder(atom, effect.size), effect.size)) +
  geom_bar(stat="identity", fill=colors2[1]) +
  geom_text(aes(y=0.05, label=sprintf("%.02f", signif(effect.size, digits=2))), color="#FFFFFF", angle=0, hjust=0, fontface='bold') +
  theme_classic() +
  theme(axis.line=element_blank(), axis.ticks = element_blank()) +
  theme(axis.text.y = element_text(hjust = 1, vjust=.4, size=16)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title = element_text(size=20)) +
  theme(axis.title.y = element_text(margin = margin(t=0, r=20, b=0, l=0))) +
  labs(x = 'Atom of Confusion', y = 'Effect Size (Confusingness)') +
  coord_flip()

########################################
#   Compare atom rates with regular node rates
########################################
all.node.counts <- data.table(read.csv('data/all-node-counts_2018-08-31_for-esem.csv'))
all.node.total <- all.node.counts[, sum(count)]
all.node.counts[, rate := count / all.node.total]
print(all.node.counts, nrows=200)

selected.node.counts <- all.node.counts[node.type %in% c('<IfStatement>', ':not', ':multiply', ':divide', ':multiplyAssign', ':divideAssign', ':throw')]

node.occurrence.rate <- ggplot(selected.node.counts, aes(x = reorder(node.type, rate), y = rate)) +
  theme_classic() +
  geom_bar(stat="identity", fill=colors2[1]) +
  geom_text(aes(y=0.0010, label=formatC(signif(rate,digits=2), digits=2, flag="#"),
                color=node.type %in% c('Omitted Curly Brace','Operator Precedence')), angle=0, hjust=0) +
  theme(#axis.text.x=element_text(angle=90, hjust=1, vjust=.4), axis.text.y = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks = element_blank(), axis.line = element_blank()) +
  scale_y_continuous(limits = c(0.0,0.013)) +
  guides(color=FALSE) +
  coord_flip() +
  scale_color_manual(values=c('black', 'white')) +
  labs(x="Node", y="Occurrence Rate")
node.occurrence.rate

ggsave("img/node_occurrence_rate.pdf", node.occurrence.rate, width=(width<-140), height=width*0.7, units = "mm")

atom.node.rates <- rbind(selected.node.counts[, .(name = node.type, rate, type="node")],
                         all.atom.rates[, .(name = atom, rate, type="atom")])

atom.node.occurrence.rate <- ggplot(atom.node.rates, aes(x = reorder(name, rate), y = rate)) +
  theme_classic() +
  geom_bar(stat="identity", aes(fill=colors2[as.integer(as.factor(type))])) +
  geom_text(aes(y=0.0010, label=formatC(signif(rate,digits=2), digits=2, flag="#"),
                color=rate>0.001), angle=0, hjust=0) +
  theme(#axis.text.x=element_text(angle=90, hjust=1, vjust=.4), axis.text.y = element_blank(),
    axis.text.x=element_blank(),
    axis.ticks = element_blank(), axis.line = element_blank()) +
  scale_y_continuous(limits = c(0.0,0.013)) +
  guides(color=FALSE, fill=FALSE) +
  coord_flip() +
  scale_color_manual(values=c('black', 'white')) +
  labs(x="Node", y="Occurrence Rate")
atom.node.occurrence.rate
as.integer(factor(atom.node.rates$type))


