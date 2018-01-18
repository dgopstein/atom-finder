# Do number of tracked bugs correlate with atoms (or vice-versa)?

library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

atom.counts <- data.table(read.csv("data/atom_counts.csv"))
atom.counts$fair.rate <- atom.counts[, -c('X.project', 'X.non.atoms')][, lapply(.SD, function(x) (x/X.all.nodes)/sum(x))][, -c('X.all.nodes')][, .(fair.rate=base::sum(.SD)), by=1:14][, fair.rate]
atom.counts[, atom.rate:=(X.all.nodes-X.non.atoms)/X.all.nodes]

# https://docs.google.com/spreadsheets/u/1/d/1Ey56XWXNn8i6Xypctb8G52iYQ9449g6NfpCf6tpDlKw/edit#gid=0
atom.counts$bug.count <- c(30930,224901,359005,10987,171261,70177,15463,79038,4358,4521,8062,8656,1457,1723)
atom.counts$since <- as.Date(c('2002-11-06','1994-09-14','1998-05-07','1999-08-03','2005-06-07','2009-04-08','2003-10-07','2002-09-12','2003-09-21','2008-02-21','2005-04-29','2001-01-08','2011-08-08','2011-05-15'))
atom.counts[, bug.rate := bug.count/as.numeric(as.Date("2018-01-01")-since)]
atom.counts$domain <- factor(c("os", "os", "browser", "compiler", "browser", "db", "compiler", "db", "vcs", "editor", "vcs", "webserver", "webserver", "editor"),
                      levels=c("os", "browser", "compiler", "db", "vcs", "editor", "webserver"),
                      ordered=TRUE)

project.bugs.plot <- ggplot(atom.counts, aes(x=fair.rate, y=bug.rate, group=domain)) +
  theme_classic() +
  geom_path(aes(color = domain), size=1.2) +
  geom_point(size=2.0) + # aes(size=log(cve_rate))) +
  geom_text(aes(label=X.project)) +
  #geom_text(aes(label=project, hjust=ifelse(atom.rate>0.02, 1.7, 0)), vjust=0.4, nudge_x = 0.0005) +
  #scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_log10(expand = c(0.1, 0.1)) +
  # scale_colour_manual(values = sap.qualitative.palette[c(1,2,3,5)],
  #                     labels=c("Database", "Editor", "OS", "Server")) +
  ggtitle("Bugs") +
  labs(x = "Atom rate", y="Bugs per day", color="Domain") +
  theme(legend.position = c(0.85, 0.70))
project.bugs.plot

ggsave("img/project_bugs.pdf", project.bugs.plot, width=(width<-138), height=width*0.60, units = "mm")


# Is there a correlation between atoms and cves within domain
cves.atoms
summary(lm(log(cves.per.year) ~ atom.rate + domain, cves.atoms))
