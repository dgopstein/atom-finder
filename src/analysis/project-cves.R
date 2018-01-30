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


project.cves.plot <- ggplot(cves.atoms, aes(x=atom.rate, y=cves.per.year/all.nodes, group=domain)) +
  theme_classic() +
  geom_path(aes(color = domain, linetype=domain%in%c('vcs','browser','compiler')), size=1.2) +
  geom_point(size=2.0) + # aes(size=log(cve_rate))) +
  geom_text(aes(label=paste(" ", project)), hjust=0, angle=25) +
  #geom_text(aes(label=project, hjust=ifelse(atom.rate>0.02, 1.7, 0)), vjust=0.4, nudge_x = 0.0005) +
  scale_x_continuous(limits = c(0.005, 0.025), breaks=c(.01,.02)) +
  scale_y_log10(limits = c(3*10^-9, 2*10^-5)) +
  scale_colour_manual(values = domain.colors) +
  #ggtitle("CVEs") +
  labs(x = "Atom rate", y="CVEs per year per AST node (log)", color="Domain") +
  guides(colour=FALSE, linetype=FALSE) +
  theme(axis.text.y=element_text(angle=90, hjust=0.5)) +
  theme(legend.position = c(0.85, 0.70))
project.cves.plot

ggsave("img/project_cves.pdf", project.cves.plot, width=(width<-90), height=width*1.10, units = "mm")


binom.test(6,7, alternative = "greater")
binom.test(8,14, alternative = "greater")
binom.test(7,14, alternative = "greater")


# Is there a correlation between atoms and cves within domain
cves.atoms
summary(lm(log(cves.per.year) ~ atom.rate + domain, cves.atoms))
