# Do number of tracked bugs correlate with atoms (or vice-versa)?

library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

project.bug.atom.counts <- data.table(read.csv("data/atom_counts.csv"))
project.bug.atom.counts$fair.rate <- project.bug.atom.counts[, -c('X.project', 'X.non.atoms')][, lapply(.SD, function(x) (x/X.all.nodes)/sum(x))][, -c('X.all.nodes')][, .(fair.rate=base::sum(.SD)), by=1:14][, fair.rate]
project.bug.atom.counts[, atom.rate:=(X.all.nodes-X.non.atoms)/X.all.nodes]

# https://docs.google.com/spreadsheets/u/1/d/1Ey56XWXNn8i6Xypctb8G52iYQ9449g6NfpCf6tpDlKw/edit#gid=0
project.bug.atom.counts$bug.count <- c(30930,224901,359005,10987,171261,70177,15463,79038,4358,4521,8062,8656,1457,1723)
project.bug.atom.counts$since <- as.Date(c('2002-11-06','1994-09-14','1998-05-07','1999-08-03','2005-06-07','2009-04-08','2003-10-07','2002-09-12','2003-09-21','2008-02-21','2005-04-29','2001-01-08','2011-08-08','2011-05-15'))
project.bug.atom.counts[, bug.rate := bug.count/as.numeric(as.Date("2018-01-01")-since)]
project.bug.atom.counts$domain <- factor(c("os", "os", "browser", "compiler", "browser", "db", "compiler", "db", "vcs", "editor", "vcs", "webserver", "webserver", "editor"),
                      levels=domain.levels,
                      ordered=TRUE)

project.bug.atom.counts$offset.x <- project.bug.atom.counts$offset.y <- 0
project.bug.atom.counts[X.project=="mongo", c("offset.x", "offset.y") := .(-1, 2)]
project.bug.atom.counts[X.project=="mysql-server", c("offset.x", "offset.y") := .(-1, 4)]
project.bug.atom.counts[X.project=="httpd", c("offset.x", "offset.y") := .(0, -2)]
project.bug.atom.counts[X.project=="gecko-dev", c("offset.x", "offset.y") := .(-2, -2)]
project.bug.atom.counts[X.project=="subversion", c("offset.x", "offset.y") := .(0, -.2)]
project.bug.atom.counts[X.project=="freebsd", c("offset.x", "offset.y") := .(0, .3)]
project.bug.atom.counts[X.project=="nginx", c("offset.x", "offset.y") := .(.8, 0)]


project.bugs.plot <- ggplot(project.bug.atom.counts, aes(x=atom.rate, y=365*bug.rate/X.all.nodes, group=domain)) +
  theme_classic() +
  geom_path(aes(color = domain, linetype = domain %in% c('vcs')), size=1.2) +
  geom_point(size=2.0) + # aes(size=log(cve_rate))) +
  geom_text(aes(label=paste(" ", X.project), x=-.0001+atom.rate+.0001*offset.x, y=(365*bug.rate/X.all.nodes)+0.00001*offset.y), hjust=0, vjust=0.4, angle=0) +
  #geom_text(aes(label=project, hjust=ifelse(atom.rate>0.02, 1.7, 0)), vjust=0.4, nudge_x = 0.0005) +
  scale_x_continuous(limits = c(0.006, 0.025), breaks=c(0.001, 0.02)) +
  scale_y_log10(limits = c(1.7*10^-5, 1.1*10^-3)) +
  scale_colour_manual(values = domain.colors) +
  #ggtitle("Bugs") +
  guides(linetype=FALSE) +
  labs(x = "Atom rate", y="Bugs per year per AST node (log)", color="Domain") +
  theme(axis.text.y=element_text(angle=90, hjust=0.5)) +
  theme(legend.position = c(0.88, 0.30), plot.margin = margin(0, r=2, b=0.1, l=0.1, "mm"))
project.bugs.plot

ggsave("img/project_bugs.pdf", project.bugs.plot, width=(width<-90), height=width*1.10, units = "mm")

mod <- summary(lm(365*bug.rate/X.all.nodes ~ atom.rate+domain, project.bug.atom.counts))
mod
sqrt(summary(mod)$r.squared)

# Is there a correlation between atoms and cves within domain
cves.atoms
summary(lm(log(cves.per.year) ~ atom.rate + domain, cves.atoms))
