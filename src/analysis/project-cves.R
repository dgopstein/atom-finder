# Do security issues (CVEs) correlate with atoms (or vice-versa)?

library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

atom.counts <- data.table(read.csv("data/atom_counts.csv"))
atom.counts$fair.rate <- atom.counts[, -c('X.project', 'X.non.atoms')][, lapply(.SD, function(x) (x/X.all.nodes)/sum(x))][, -c('X.all.nodes')][, .(fair.rate=base::sum(.SD)), by=1:14][, fair.rate]

# https://docs.google.com/spreadsheets/d/1b06cxKocM2z34QHoBipxnoDE8PpOqwNNAy4C4kqk4mA/edit#gid=0
# https://docs.google.com/spreadsheets/d/1_5E8ICuxDP1hwJO354Fydmuik2SQkyNL_v6VEqdjG6A/edit#gid=1420865776
project.cves <- data.table(read.csv("data/project_cves.csv"))
project.cves[, cves.per.year := cves/(2017-since)]

cves.atoms <- merge(project.cves, atom.counts[, .(project=X.project, fair.rate, all.nodes=X.all.nodes, atom.rate=(X.all.nodes-X.non.atoms)/X.all.nodes)], by='project')


#projects.high.rate <- project.cves[project.cves, on="domain"][project != i.project & cve_rate > 1 & i.cve_rate > 1]
cves.atoms[, domain := factor(as.character(domain), c("os", "browser", "compiler", "db", "vcs", "editor", "webserver"))]

cves.atoms$offset.x <- cves.atoms$offset.y <- 0
cves.atoms[project=="nginx", c("offset.x", "offset.y") := .(-1.14, 8)]
cves.atoms[project=="subversion", c("offset.x", "offset.y") := .(1, 5)]
cves.atoms[project=="webkit", c("offset.x", "offset.y") := .(-7.5, 3.5)]
cves.atoms[project=="mysql-server", c("offset.x", "offset.y") := .(0, -3)]
cves.atoms[project=="emacs", c("offset.x", "offset.y") := .(0, 1.5)]
cves.atoms[project=="git", c("offset.x", "offset.y") := .(0, -1.5)]

project.cves.plot <- ggplot(cves.atoms, aes(x=atom.rate, y=cves.per.year/all.nodes, group=domain)) +
  theme_classic() +
  geom_path(aes(color = domain, linetype=domain%in%c('vcs','browser','compiler')), size=1.2) +
  geom_point(size=2.0) + # aes(size=log(cve_rate))) +
  geom_text(aes(label=paste(" ", project), x=atom.rate+0.00003*offset.x, y=(cves.per.year/all.nodes)+0.00000003*offset.y), hjust=0, vjust=0.4) + #, angle=25) +
  #geom_text(aes(label=project, hjust=ifelse(atom.rate>0.02, 1.7, 0)), vjust=0.4, nudge_x = 0.0005) +
  scale_x_continuous(limits = c(0.006, 0.025), breaks=c(.01,.02)) +
  scale_y_log10(limits = c(3*10^-9, 2*10^-5)) +
  scale_colour_manual(values = domain.colors) +
  #ggtitle("CVEs") +
  labs(x = "Atom rate", y="CVEs per year per AST node (log)", color="Domain") +
  guides(colour=FALSE, linetype=FALSE) +
  theme(axis.text.y=element_text(angle=90, hjust=0.5)) +
  theme(legend.position = c(0.85, 0.70), plot.margin = margin(0, r=2, b=0.1, l=0.1, "mm"))
project.cves.plot

ggsave("img/project_cves.pdf", project.cves.plot, width=(width<-90), height=width*1.10, units = "mm")

summary(lm(cves.per.year/all.nodes ~ atom.rate, cves.atoms))

binom.test(6,7, alternative = "greater")
binom.test(5,7, alternative = "greater")
binom.test(11,14, alternative = "greater")


# Is there a correlation between atoms and cves within domain
cves.atoms
summary(lm(log(cves.per.year) ~ atom.rate + domain, cves.atoms))
