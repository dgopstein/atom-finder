# Do security issues (CVEs) correlate with atoms (or vice-versa)?

library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# https://docs.google.com/spreadsheets/d/1_5E8ICuxDP1hwJO354Fydmuik2SQkyNL_v6VEqdjG6A/edit#gid=1420865776
project.cves <- data.table(read.csv("data/project_cves.csv"))

projects.high.rate <- project.cves[project.cves, on="domain"][project != i.project & cve_rate > 1 & i.cve_rate > 1]

ggplot(projects.high.rate, aes(x=atoms, y=cve_rate, group=domain)) +
  geom_path(aes(color = domain), size=1.5) +
  geom_point(size=3) + # aes(size=log(cve_rate))) +
  geom_text(aes(label=project, hjust=ifelse(atoms>0.02, 1.7, 0)), vjust=0.4, nudge_x = 0.0005) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_log10(expand = c(0.1, 0.1)) +
  labs(x = "Atom rate", y="CVEs per year") +
  ggtitle("Atoms and CVEs", subtitle="Domains with >1 CVE/year")


