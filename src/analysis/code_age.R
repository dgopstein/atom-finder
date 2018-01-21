library(data.table)
library(ggplot2)
library(RColorBrewer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

code.age.wide <- data.table(read.csv("data/code-age_all_2018-01-16_all_per-year_combined.csv"))
code.age.wide[, date := as.Date(date)]
code.age.wide <- code.age.wide[date > '1984-01-01' & date < '2018-01-01' & all.nodes > 1000]
code.age.wide <- code.age.wide[order(date)]
code.age.wide[project=='linux-historical']$project <- 'linux'

code.age.wide[project=='emacs', .(date, all.nodes)]

code.age <- melt(code.age.wide, id.vars=c("project", "date","rev.str", 'all.nodes'), variable.name="atom", value.name="count")

code.age.all.atoms <- code.age[!atom%in%c('non.atoms')][, .(count = sum(count), all.nodes = sum(all.nodes)), by=c('project', 'date', 'rev.str')]
code.age.all.atoms[, rate := count / all.nodes]

code.age.all.atoms[, smooth.rate := zoo::rollmedian(rate, 5), by=project]
first.points <- code.age.all.atoms[, .(date=min(date)), by=project]
#code.age.all.atoms[date%in%first.points$date]$smooth.rate <- code.age.all.atoms[date%in%first.points$date]$rate

first.data <- code.age.all.atoms[,.SD[which.min(.SD$date)],by=project]
ggplot(code.age.all.atoms) +
  theme_classic() +
  geom_line(aes(date, rate, group=project, colour=project), size=0.5) +
  geom_point(aes(date, rate, color=project), data=first.data, size=5) +
  geom_text(aes(date, rate, label=paste("  ", project)), data=first.data, hjust=0, angle=0) +
  scale_color_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(14))

# Rolling stddev - https://rviews.rstudio.com/2017/07/18/introduction-to-rolling-volatility/

code.age.all.atoms[, .(date, sd = rollapply(rate, 5, sd)), by=project]

ggplot(code.age.all.atoms[, .(date, sd = rollapply(rate, 3, sd)), by=project]) +
  geom_line(aes(date, sd, group=project))
