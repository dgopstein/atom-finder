library(data.table)
library(stringr)
library(scales)
library(ggplot2)
library(extrafont)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

atoms.removed <- data.table(read.csv("data/atoms-in-bugs_gcc_2018-09-17_filter-c-files_removed.csv.xz", header=TRUE))
atoms.added   <- data.table(read.csv("data/atoms-in-bugs_gcc_2018-09-17_filter-c-files_added.csv.xz", header=TRUE))

# atoms.removed.old <- data.table(read.csv("data/atoms-in-bugs_gcc_2018-01-11_removed.csv.bz2", header=TRUE))
# atoms.added.old <- data.table(read.csv("data/atoms-in-bugs_gcc_2018-01-11_added.csv.bz2", header=TRUE))

#merge(atoms.removed, atoms.removed.old, by=c('rev.str', 'file'))[comma.operator.x != comma.operator.y, .(rev.str, file, comma.operator.x, comma.operator.y)]

colnames(atoms.removed)

nrow(atoms.removed)
length(unique(atoms.removed$rev.str))

atoms.removed[, bug := n.bugs > 0]
atoms.added[, bug := n.bugs > 0]

atoms.removed[is.na(atoms.removed)] <- 0
atoms.added[is.na(atoms.added)] <- 0

atoms.removed[, all.atoms.removed := n.removed - removed.non.atoms] 
atoms.added[, all.atoms.added := n.added - added.non.atoms] 

atoms.removed[, any.atoms.removed := (all.atoms.removed > 0)]
atoms.added[, any.atoms.added := (all.atoms.added > 0)]

atoms.removed.contingency <- atoms.removed[, .(count = mean(all.atoms.removed / n.removed, na.rm = TRUE)), by=c('bug')]
atoms.added.contingency <- atoms.added[, .(count = mean(all.atoms.added / n.added, na.rm = TRUE)), by=c('bug')]

added.removed.contingency <- merge(atoms.removed.contingency, atoms.added.contingency, by='bug', suffixes = c('.removed', '.added'))
added.removed.contingency[, .(count.removed / count.added), by='bug']

colnames(atoms.added)

added.removed.wide <- merge(atoms.removed, atoms.added, suffixes = c('.removed', '.added'), by=c('author.name', 'author.email', 'file', 'n.bugs', 'added.non.atoms', 'n.added', 'rev.str', 'removed.non.atoms', 'n.removed', 'bug'))

atoms.removed.sums <- atoms.removed[, -c('author.name', 'author.email', 'file', 'rev.str')][, lapply(.SD, sum), by='bug']
atoms.added.sums <- atoms.added[, -c('author.name', 'author.email', 'file', 'rev.str')][, lapply(.SD, sum), by='bug']

#################
# Type conversion is one of the most-removed atoms in non.bug.fix commits. Why?
#################
atoms.removed.sums[, .(bug, type.conversion, n.removed, rate = type.conversion/n.removed)]
atoms.added.sums[, .(bug, type.conversion, n.added, rate = type.conversion/n.removed)]

atoms.removed[type.conversion > 0, .(rev.str, file, bug, n.added, n.removed, all.atoms.removed, type.conversion, rate = type.conversion/n.removed)]
atoms.added[type.conversion > 0, .(rev.str, file, bug, n.added, n.removed, all.atoms.added, type.conversion, rate = type.conversion/n.added)]


#################



### (rem/add)[bug] / (rem/add)[non-bug]

atoms.removed.rate <- atoms.removed.sums[, lapply(.SD, function(x) x / n.removed), by='bug']
atoms.added.rate <- atoms.added.sums[, lapply(.SD, function(x) x / n.added), by='bug']

removed.rate <- data.table(t(mapply(function(rem, add) rem/add, atoms.removed.rate, atoms.added.rate)), keep.rownames = TRUE)
colnames(removed.rate) <- c('atom', 'non.bug', 'bug')
removed.rate[, rate := bug/non.bug]
removed.rate

intercept <- 1
ggplot(removed.rate[!(atom %in% c("bug","n.bugs","any.atoms.removed","n.removed",'added.non.atoms','n.added'))], aes(x = reorder(atom, rate), y = rate)) +
  geom_segment(aes(y = intercept,yend = rate, xend = atom, color=ifelse(atom %in% c("removed.non.atoms",'all.atoms.removed'), "3", rate>intercept)),
               show.legend=F, size=4) +
  geom_text(color="black", size=3, aes(label=round(ifelse(rate >= intercept, rate, 1/rate), digits=2), hjust = ifelse(rate >= intercept, -.3, 1.5))) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  geom_hline(yintercept=intercept) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.4)) +
  scale_y_log10(expand = c(0.1,0.2)) +
  annotate("text", x=16.5, y=0.12, label='bold("Non-bug")', parse=TRUE, hjust=-0.05, size=5.0) +
  annotate("text", x=16.5, y=2, label='bold("Bug")', parse=TRUE, hjust=-0.05, size=5.0) +
  annotate("text", x=13.5, y=0.12, label="over((over(rem,add))[italic(bug)~~~~~~~~.],(over(rem,add))[italic(non-bug)])", parse=TRUE, hjust=-0.05, size=5.0) +
  labs(title="Atoms removed more in...", x="Relative Rate", y="Atom") +
  coord_flip(ylim = c(0.2, 5))

### (rem[bug]/rem[non-bug]) & (add[bug]/add[non-bug])

atoms.removed.bugs.over.non.bugs <- data.table(t(atoms.removed.rate[bug==TRUE] / atoms.removed.rate[bug==FALSE]), removed=TRUE, keep.rownames = TRUE)[rn %in% atom.names.key]
atoms.added.bugs.over.non.bugs <- data.table(t(atoms.added.rate[bug==TRUE] / atoms.added.rate[bug==FALSE]), removed=FALSE, keep.rownames = TRUE)[rn %in% atom.names.key]

atoms.bugs.over.non.bugs <- rbind(atoms.removed.bugs.over.non.bugs, atoms.added.bugs.over.non.bugs)
colnames(atoms.bugs.over.non.bugs) <- c('atom', 'rate', 'removed')

intercept <- 1
ggplot(atoms.bugs.over.non.bugs) +
  geom_bar(aes(x = atom, y = rate, fill=removed), position = "dodge", stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.4)) +
  scale_fill_discrete(name="Added/Removed",
                      breaks=c(FALSE, TRUE),
                      labels=c(expression(over(added[bug],added[non-bug])), expression(over(removed[bug],removed[non-bug])))) +
  theme(legend.key.size = unit(3, 'lines'))

#################################################################
#  A commit where the line was edited, but the atom not removed
#################################################################
common.bugs.lines.atoms <- merge(bugs.lines.csv, atoms.removed, suffixes=c('.lines', '.atoms'), by=c('rev.str', 'file'))
common.bugs.lines.atoms[repurposed.variable.lines > 0 & repurposed.variable.atoms <= 0]
common.bugs.lines.atoms[operator.precedence.lines > 0 & operator.precedence.atoms <= 0]

atoms.added[rev.str=='006e76552254b00892858364b87a22790fa5dfc3']


tail(bugs.lines.csv[, rev.str])
atoms.removed[1:3, rev.str]

bugs.lines.csv[rev.str=="2201c33012d4c6dc522ddbfa97f5aa95a209e24d"]
atoms.removed[ rev.str=="711789cc2a21e70e4ea0e2d9d5863924d85687d6"]

#################################################################
#  Are atoms removed at a higher rate in bug commits vs non-bug commits
#################################################################

atoms.removed.rate.by.bug.dt <- rbind(
    data.table(t(atoms.removed.rate[bug==FALSE]), bug=FALSE, keep.rownames = TRUE),
    data.table(t(atoms.removed.rate[bug==TRUE]), bug=TRUE, keep.rownames = TRUE))

colnames(atoms.removed.rate.by.bug.dt) <- c('atom', 'rate', 'bug')

atoms.removed.rate.dt <-
  data.table(atom = colnames(atoms.removed.rate),
             bug = t(atoms.removed.rate[bug==TRUE])[,1],
             no.bug = t(atoms.removed.rate[bug==FALSE])[,1],
             bug.count = t(atoms.removed.sums[bug==FALSE])[,1])

atoms.removed.rate.dt[, rate := bug / no.bug]
atoms.removed.rate.dt[, display.atom := convert.atom.names(atom)]

only.atoms.removed.rate.dt <- atoms.removed.rate.dt[!(atom %in% c("bug","n.bugs","any.atoms.removed","n.removed",'added.non.atoms','n.added', 'removed.non.atoms', 'all.atoms.removed'))]
only.atoms.removed.rate.dt[, display.atom := reorder(display.atom, rate)]

atoms.removed.rate[bug==TRUE, all.atoms.removed] / atoms.removed.rate[bug==FALSE, all.atoms.removed]
chisq.test(c(atoms.removed.sums[bug==TRUE, all.atoms.removed], atoms.removed.sums[bug==FALSE, all.atoms.removed],
           atoms.removed.sums[bug==TRUE, n.removed], atoms.removed.sums[bug==FALSE, n.removed]))

atoms.removed.chis <-
  data.table(t(atoms.removed.sums[, lapply(.SD, function(x)
    with(chisq.test(matrix(c(x[bug==FALSE], x[bug==TRUE], n.removed[bug==FALSE], n.removed[bug==TRUE]), nrow=2)), .(p.value, statistic, n = sum(observed)))), ]),
    keep.rownames = TRUE)

colnames(atoms.removed.chis) <- c('atom', 'p.value', 'X2', 'n')

only.atoms.removed.rate.dt <- merge(only.atoms.removed.rate.dt, atoms.removed.chis, on='atom')

par(font.axis = 2)
par(font.lab = 2)

#font_import()
#View(fonttable())
Sys.setenv(R_GSCMD = '/usr/local/bin/gs')

intercept <- 1
atom.removed.rate.plot <-
ggplot(only.atoms.removed.rate.dt[!is.nan(rate)],
       aes(x = display.atom, y = rate)) +
  theme_minimal() +
#  theme_classic() +
  geom_segment(aes(y = intercept, yend = rate, xend = display.atom, size = bug.count,
                   color=rate<intercept),
               show.legend=F) +
  geom_hline(yintercept=intercept) +
  scale_size(range = c(0.3, 7.2)) +
  geom_segment(aes(xend=display.atom, y=rate*ifelse(rate >= intercept, 1.05, 1/1.05), yend=rate*ifelse(rate >= intercept, 1.45, 1/1.45)),
               size=2, color="white") +
  geom_text(aes(label=ifelse(rate >= intercept, paste0(sprintf("                 %0.2f ", rate), signif.stars(p.value)),
                                                paste0(signif.stars(p.value), sprintf(" %0.2f                 ", 1/rate)))),
                #,y=ifelse(rate > 0.001, rate, 0.35)),
            color="black", size=3, vjust=0.4) +
  annotate('rect', xmin = .9, xmax = 1.1, ymin = 0.25, ymax = 0.36, fill="white", alpha=0.5) +
  annotate('text', x=1, y=0.3, label="** Inf", size=3) +
  annotate('label', x=2.5, y=1.01, size=2.8, hjust=0, label.size=NA,
           family="DroidSansMono", label=" p<0.1    *\n p<0.01   **\n p<0.001  ***\n p<0.0001 ****") +
  scale_color_manual(values=c(colors2, 'red')) +
  scale_y_log10(position="right", labels=c("Non-bugs", "Bugs"), breaks=c(.47, 1.7)) +
  labs(x="Atom", y="Atoms removed more often in...") +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(vjust=0.4),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_flip(ylim = c(0.23, 2.8))
atom.removed.rate.plot

ggsave("img/atom_removed_rate.pdf", atom.removed.rate.plot, width=(width<-110), height=width*0.7, units = "mm", device=cairo_pdf)


atom.removed.rate.significant.plot <-
  ggplot(only.atoms.removed.rate.dt[p.value < 0.001],
         aes(x = display.atom, y = rate)) +
  theme_minimal() +
  geom_segment(aes(y = intercept, yend = rate, xend = display.atom, size = bug.count,
                   color=rate<intercept),
               show.legend=F) +
  geom_hline(yintercept=intercept) +
  scale_size(range = c(0.3, 7.2)) +
  geom_segment(aes(xend=display.atom, y=rate*ifelse(rate >= intercept, 1.05, 1/1.05), yend=rate*ifelse(rate >= intercept, 1.45, 1/1.45)),
               size=2, color="white") +
  geom_text(aes(label=ifelse(rate >= intercept, paste0(sprintf("                 %0.2f ", rate), signif.stars(p.value)),
                             paste0(signif.stars(p.value), sprintf(" %0.2f                 ", 1/rate)))),
            color="black", size=3, vjust=0.4) +
  scale_color_manual(values=c(colors2, 'red')) +
  scale_y_log10(position="right", labels=c("Non-bugs", "Bugs"), breaks=c(.67, 1.5)) +
  labs(x="Atom", y="Atoms removed more often in...") +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(vjust=0.4),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_flip(ylim = c(0.51, 2.2))
atom.removed.rate.significant.plot

ggsave("img/atom_removed_rate_significant.pdf", atom.removed.rate.significant.plot, width=(width<-110), height=width*0.5, units = "mm", device=cairo_pdf)



bug.effect <- merge(only.atoms.removed.rate.dt, atom.effect.sizes, by='atom')

ggplot(bug.effect) +
  geom_point(aes(effect.size, rate))# +
  geom_text(aes(effect.size, rate, label=atom), hjust=0)

###### Atoms added

atoms.added.rate.by.bug.dt <- rbind(
  data.table(t(atoms.added.rate[bug==FALSE]), bug=FALSE, keep.rownames = TRUE),
  data.table(t(atoms.added.rate[bug==TRUE]), bug=TRUE, keep.rownames = TRUE))

colnames(atoms.added.rate.by.bug.dt) <- c('atom', 'rate', 'bug')

atoms.added.rate.dt <-
  data.table(atom = colnames(atoms.added.rate), bug = unlist(t(atoms.added.rate[bug==TRUE])[,1]),
             no.bug = t(atoms.added.rate[bug==FALSE])[,1],
             bug.count = t(atoms.added.sums[bug==FALSE])[,1])

atoms.added.rate.dt[, rate := pmax(bug / no.bug, 0.00001)]

intercept <- 1
ggplot(atoms.added.rate.dt[!(atom %in% c("bug","n.bugs","any.atoms.added","n.added",'added.non.atoms','n.added', 'added.non.atoms', 'all.atoms.added', 'n.removed', 'removed.non.atoms'))],
       aes(x = reorder(atom, rate), y = rate)) +
  geom_segment(aes(y = intercept,yend = rate, xend = atom, size = bug.count,
                   color=ifelse(atom %in% c("added.non.atoms",'all.atoms.added'), "3", rate>intercept)),
               show.legend=F) +
  scale_size(range = c(0.3, 11)) +
  geom_text(color="black", size=3, aes(label=round(ifelse(rate >= intercept, rate, 1/rate), digits=2), hjust = ifelse(rate >= intercept, -.3, 1.5))) +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_hline(yintercept=intercept) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.4)) +
  scale_y_log10(expand = c(0.1,0.2)) +
  annotate("text", x=15, y=0.12, label='bold("Non-bug")', parse=TRUE, hjust=-0.05, size=5.0) +
  annotate("text", x=15, y=2, label='bold("Bug")', parse=TRUE, hjust=-0.05, size=5.0) +
  annotate("text", x=11.5, y=0.12, label="over((over(atoms[added],all-nodes[added]))[italic(bug)~~~~~~~~.],(over(atoms[added],all-nodes[added]))[italic(non-bug)])", parse=TRUE, hjust=-0.05, size=5.0) +
  labs(title="Atoms added more in...", x="Relative Rate", y="Atom") +
  coord_flip(ylim = c(0.2, 5))

