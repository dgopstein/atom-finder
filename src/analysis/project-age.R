# Do newer projects have fewer atoms?

library(data.table)
library(ggplot2)
library(ggrepel)

set.seed(42)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

# https://docs.google.com/spreadsheets/d/1_5E8ICuxDP1hwJO354Fydmuik2SQkyNL_v6VEqdjG6A/edit#gid=1197815198
project.age <- data.table(read.csv("data/project-age.csv"))
project.age[, domain := factor(domain, domain.levels)]
project.age[, date := as.Date(date)]
project.age$newer <- apply(project.age, 1, function (x) x['date'] == project.age[domain==x['domain'], max(date)])

# parallel coordinates
ggplot(project.age, aes(x=newer, y=atoms, group=domain)) +
  geom_path(aes(size = all.nodes, color = domain))

# domain lines
ggplot(project.age, aes(x=date, y=atoms, group=domain)) +
  geom_path(aes(size = all.nodes, color = domain)) +
  geom_point() +
  geom_text(aes(label=project),hjust=0.5, vjust=-1)

# atoms by project age
atoms.by.project.age <-
  ggplot(project.age, aes(x=date, y=atoms)) +
  theme_classic() +
  geom_smooth(method="lm", colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  geom_point(size=3, aes(colour=domain)) +
  geom_text(aes(label=project), size = 3, angle=-20, hjust=0, vjust=0.4, nudge_x=200, nudge_y=-0.00015) +
  #geom_text_repel(aes(label=project), size = 4, angle=-20, force=0.1, direction="x") +
  scale_x_date(limits = as.Date(c("1985-01-01", "2012-01-01"))) +
  scale_y_continuous(limits = c(.005, .024)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.4), axis.ticks.x=element_blank()) +
  scale_colour_manual(values = domain.colors) +
  annotate("text", x=as.Date('1998-01-01'), y=0.019, label="r==-0.32", parse=TRUE, hjust=0.0, size=4.0) +
  labs(x = "Date", y = "Atom Rate")

ggsave("img/atoms_by_project_age.pdf", atoms.by.project.age, width=(width<-138), height=width*0.65, units = "mm")

summary(lm(atoms ~ date, data=project.age))
summary(lm(atoms ~ date + domain, data=project.age))

