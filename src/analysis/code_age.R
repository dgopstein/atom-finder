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

# Remove all emacs commits because of the K&R style code that breaks all pre-2010 analyses
# https://github.com/emacs-mirror/emacs/commit/971de7fb158335fbda39525feb2d7776a26bc030
# code.age.wide[project=='emacs' & date >= '2011-01-01', .(project, date, (all.nodes - non.atoms)/all.nodes)]
# code.age.wide <- code.age.wide[!(project=='emacs' & date < '2011-01-01'),]
# code.age.all.atoms[project=='emacs' & date < '2011-01-01', count := as.integer(2.18*count)]
# code.age.wide <- code.age.wide[project!='emacs']


code.age <- melt(code.age.wide, id.vars=c("project", "date","rev.str", 'all.nodes'), variable.name="atom", value.name="count")

code.age.all.atoms <- code.age[!atom%in%c('non.atoms')][, .(count = sum(count), all.nodes = mean(all.nodes)), by=c('project', 'date', 'rev.str')]

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

# mean atom rate by project start-date
project.age.mean.atoms <- merge(project.age[, .(project, domain, date)],
  code.age.all.atoms[, .(project, count, all.nodes)][
    , lapply(.SD, function(x) base::sum(as.numeric(x))), by=project][
      ,.(project, rate=count/all.nodes)])

project.age.mean.atoms


rlm.model <- MASS::rlm(rate ~ date, data=project.age.mean.atoms)
rlm.lm.model <- summary(lm(rate ~ date, data=project.age.mean.atoms,weights=rlm.model$w) )
pearson.corr <- -sqrt(rlm.lm.model$adj.r.squared)
slope <- coef(rlm.lm.model)
slope['date','Estimate'] * 365

mean.atoms.by.project.age <-
  ggplot(project.age.mean.atoms, aes(x=date, y=rate)) +
  theme_classic() +
  #geom_smooth(method="rlm", colour="black", size=0.5, se=FALSE, fullrange=TRUE) +
  stat_smooth(colour=colors2dark[1], size=0.5, se=FALSE, fullrange=TRUE,
    method=function(f,data=data,weights=weight) MASS::rlm(f,data,weights=weight,method="MM")) +
  geom_point(size=3, color=colors2dark[2]) +
  geom_text(aes(label=paste(" ", project)), size = 3, angle=-18, hjust=0, vjust=0.4) +
  #geom_text_repel(aes(label=project), size = 4, angle=-20, force=0.1, direction="x") +
  scale_x_date(limits = as.Date(c("1985-01-01", "2017-01-01"))) +
  #scale_y_continuous(limits = c(.005, .024)) +
  scale_colour_manual(values = domain.colors) +
  annotate("text", x=as.Date('2002-01-01'), y=0.019, label="r==-0.65", parse=TRUE, hjust=0.0, size=4.0) +
  theme(legend.position = c(0.9, 0.6)) +
  labs(x = "Project Start Date", y = "Average Atom Rate", colour="Domain")


mean.atoms.by.project.age

ggsave("img/mean_atoms_by_project_age.pdf", mean.atoms.by.project.age, width=(width<-145), height=width*0.5, units = "mm")

mean.project.date.atoms <- merge(project.age.mean.atoms[,c('project', 'rate')],
                                 code.age.all.atoms[, .(date = mean(date)), by=project])



mean.project.date.atoms <- merge(project.age.mean.atoms[,c('project', 'rate')],
                                 code.age.all.atoms[, .(date = mean(date)), by=project])


project.age.linear.models <-
  data.table(project = first.points$project,
             model = lapply(first.points$project,
                            function(proj) lm(rate ~ date, code.age.all.atoms[project==proj])),
             date = first.points$date)

# projects estimated by lines
first.points$linear.rate <-
  sapply(first.points$project,
    function(proj)
      predict(lm(rate ~ date, code.age.all.atoms[project==proj]),
              first.points[project==proj, .(date)]))

first.points$angle <-
  sapply(first.points$project,
         function(proj)
           lm(rate ~ date, code.age.all.atoms[project==proj])$coefficients)["date",]

code.age.for.regression <- merge(code.age.all.atoms, first.points[,.(project, linear.rate)], by.x = 'project', by.y='project')

summary(lm(rate ~ date, data=code.age.all.atoms[project=='emacs' & date >= '2011-01-01']))

summary(lm(rate ~ date, data=code.age.all.atoms[project=='emacs' & date >= '2011-01-01']))

ggplot(aes(date, rate), data=code.age.all.atoms[project=='emacs' & date >= '2011-01-01']) +
  geom_point() + stat_smooth(aes(date, rate, group=project, color=project), method="lm", size=0.5, se=FALSE)


first.points$angle.no.intercept <-
  sapply(first.points$project,
         function(proj)
           lm((I(rate - linear.rate) ~ 0+date), code.age.for.regression[project==proj])$coefficients)

project.age.linear.no.intercept <- ggplot(first.points) +
  theme_classic() +
  geom_segment(aes(x = date, y=linear.rate, xend=as.Date("2017-01-01"), yend=(3000*angle.no.intercept)+linear.rate, color=project), size=0.5) +
  geom_point(aes(date, linear.rate, color=project), data=first.points, size=2) +
  geom_text(aes(date, linear.rate, label=paste0("  ", project), angle=(1.3*(10^7)*angle)),
            data=first.points, hjust=0, vjust=-0.4) +
  labs(x = "Date", y = "Linearized Atom Rate") +
  guides(colour=FALSE)
project.age.linear.no.intercept


project.age.linear <- ggplot(code.age.all.atoms[project!='emacs']) +
  theme_classic() +
  stat_smooth(aes(date, rate), method="lm", colour="gray", size=2, se=FALSE, fullrange=TRUE) +
  stat_smooth(aes(date, rate, group=project), color=colors2dark[1], method="lm", size=0.5, se=FALSE) +
  annotate("text", x=as.Date('1991-02-01'), y=0.017, angle=-4.2, label="     All Projects", hjust=0.0, size=4) +
  annotate("point", x=as.Date('1991-02-01'), y=0.01605, colour='#888888', size=4.0) +
  geom_point(aes(date, linear.rate), color=colors2dark[2], data=first.points[project!='emacs'], size=2) +
  geom_text(aes(date, linear.rate, label=paste0("  ", project), angle=(1.3*(10^7)*angle)),
            data=first.points[project!='emacs'], hjust=0, vjust=-0.4, size=3.0) +
  labs(x = "Date", y = "Linearized Atom Rate") +
  guides(colour=FALSE)

project.age.linear

ggsave("img/project_age_linear.pdf", project.age.linear, width=(width<-145), height=width*0.5, units = "mm")

# projects estimated by curves
ggplot(code.age.all.atoms) +
  geom_point(aes(date, rate, color=project), data=first.data, size=5) +
  geom_text(aes(date, rate, label=paste("  ", project)), data=first.data, hjust=0, angle=0) +
  stat_smooth(aes(date, rate, group=project, color=project), size=0.5, se=FALSE) +
  stat_smooth(aes(date, rate), method="lm", colour="red", size=2, se=FALSE, fullrange=TRUE)

Hmisc::binconf(20, 20)
Hmisc::binconf(19, 20)
Hmisc::binconf(12, 20)
