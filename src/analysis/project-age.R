# Do newer projects have fewer atoms?

library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# https://docs.google.com/spreadsheets/d/1_5E8ICuxDP1hwJO354Fydmuik2SQkyNL_v6VEqdjG6A/edit#gid=1197815198
project.age <- data.table(read.csv("data/project-age.csv"))
project.age[, date := as.character(date)]
project.age$newer <- apply(project.age, 1, function (x) x['date'] == project.age[type==x['type'], max(date)])

# parallel coordinates
ggplot(project.age, aes(x=newer, y=atoms, group=type)) +
  geom_path(aes(size = all.nodes, color = type))


ggplot(project.age, aes(x=date, y=atoms, group=type)) +
  geom_path(aes(size = all.nodes, color = type)) +
  geom_point() +
  geom_text(aes(label=project),hjust=0.5, vjust=-1)
  
