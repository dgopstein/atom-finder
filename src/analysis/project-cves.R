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

cves.atoms <- merge(project.cves, atom.counts[, .(project=X.project, fair.rate, atom.rate=(X.all.nodes-X.non.atoms)/X.all.nodes)], by='project')


#projects.high.rate <- project.cves[project.cves, on="domain"][project != i.project & cve_rate > 1 & i.cve_rate > 1]

project.cves.plot <- ggplot(cves.atoms, aes(x=atom.rate, y=cves.per.year, group=domain)) +
  theme_classic() +
  geom_path(aes(color = domain), size=1.2) +
  geom_point(size=2.0) + # aes(size=log(cve_rate))) +
  geom_text(aes(label=project)) +
  #geom_text(aes(label=project, hjust=ifelse(atom.rate>0.02, 1.7, 0)), vjust=0.4, nudge_x = 0.0005) +
  #scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_log10(expand = c(0.1, 0.1)) +
  # scale_colour_manual(values = sap.qualitative.palette[c(1,2,3,5)],
  #                     labels=c("Database", "Editor", "OS", "Server")) +
  ggtitle("CVEs") +
  labs(x = "Atom rate", y="CVEs per year\n[log scale]", color="Domain") +
  theme(legend.position = c(0.85, 0.70))
project.cves.plot

ggsave("img/project_cves.pdf", project.cves.plot, width=(width<-138), height=width*0.60, units = "mm")
binom.test(8,14, alternative = "greater")
binom.test(7,14, alternative = "greater")


# Is there a correlation between atoms and cves within domain
cves.atoms
summary(lm(log(cves.per.year) ~ atom.rate + domain, cves.atoms))
