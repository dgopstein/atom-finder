library(data.table)
library(stringr)
library(scales)
library(ggplot2)
library(extrafont)
library(tidyr)
library(dplyr)
library(MASS)
library(viridis)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

atoms.removed.cve <- data.table(read.csv("data/cve-patch-atoms_2018-08-29_fixed-comma-operator_removed.csv", header=TRUE))
atoms.added.cve   <- data.table(read.csv("data/cve-patch-atoms_2018-08-29_fixed-comma-operator_added.csv", header=TRUE))

nrow(atoms.removed.cve)
length(unique(atoms.removed.cve$rev.str))

atoms.removed.cve[is.na(atoms.removed.cve)] <- 0
atoms.added.cve[is.na(atoms.added.cve)] <- 0

atom.dot.names <- unique(atom.names.key[!grepl('-', atom.names.key)])
only.atoms.removed.cve <- subset(atoms.removed.cve, select=atom.dot.names)
only.atoms.added.cve <- subset(atoms.added.cve, select=atom.dot.names)

atoms.removed.cve$n.atoms.removed.cve <- rowSums(only.atoms.removed.cve)
atoms.added.cve$n.atoms.added.cve <- rowSums(only.atoms.added.cve)

atoms.removed.cve.sums <- atoms.removed.cve[, -c('author.name', 'author.email', 'file', 'rev.str', 'git.repo.hash', 'git.repo.url', 'cve.ids')][, lapply(.SD, sum)]
atoms.added.cve.sums <- atoms.added.cve[, -c('author.name', 'author.email', 'file', 'rev.str', 'git.repo.hash', 'git.repo.url', 'cve.ids')][, lapply(.SD, sum)]

#atoms.removed.cve[, n.atoms.removed.cve := n.removed - removed.non.atoms] 
#atoms.added.cve[, n.atoms.added.cve := n.added - added.non.atoms] 

atoms.removed.cve[, any.atoms.removed.cve := (n.atoms.removed.cve > 0)]
atoms.added.cve[, any.atoms.added.cve := (n.atoms.added.cve > 0)]

###### Summary Statistics #####
#atoms.added.cve.sums$n.added - atoms.added.cve.sums$added.non.atoms
atoms.added.cve.sums$n.atoms.added.cve
#atoms.removed.cve.sums$n.removed - atoms.removed.cve.sums$removed.non.atoms
atoms.removed.cve.sums$n.atoms.removed.cve

atoms.added.rate.in.cves <- mean(atoms.added.cve$n.atoms.added.cve / atoms.added.cve$n.added, na.rm=TRUE)
atoms.removed.rate.in.cves <- mean(atoms.removed.cve$n.atoms.removed.cve / atoms.removed.cve$n.removed, na.rm=TRUE)
cve.removal.rate.paper <- atoms.removed.rate.in.cves / atoms.added.rate.in.cves
cve.removal.rate.paper

chisq.test(matrix(c(atoms.removed.cve.sums$n.atoms.removed.cve, atoms.removed.cve.sums$n.removed,
                    atoms.added.cve.sums$n.atoms.added.cve,     atoms.added.cve.sums$n.added), nrow=2))

#######################################################################

atoms.removed.cve.rate <- atoms.removed.cve.sums[, lapply(.SD, function(x) x / n.removed)]
atoms.added.cve.rate <- atoms.added.cve.sums[, lapply(.SD, function(x) x / n.added)]

atoms.added.removed.cve.rate.dt <- rbind(
  data.table(t(atoms.added.cve.rate), removed=FALSE, keep.rownames = TRUE),
  data.table(t(atoms.removed.cve.rate), removed=TRUE, keep.rownames = TRUE))

colnames(atoms.added.removed.cve.rate.dt) <- c('atom', 'rate', 'removed')

atoms.relative.cve.rate <- data.table(colnames(atoms.added.cve.rate),
                                      convert.atom.names(colnames(atoms.added.cve.rate)),
                                      t(atoms.added.cve.sums), t(atoms.removed.cve.sums),
                                      t(atoms.added.cve.rate), t(atoms.removed.cve.rate))
colnames(atoms.relative.cve.rate) <- c('atom', 'display.atom', 'added.sum', 'removed.sum', 'added.rate', 'removed.rate')

atoms.relative.cve.rate[, relative.rate := removed.rate / added.rate]
atoms.relative.cve.rate <- atoms.relative.cve.rate[!display.atom %in% c("Non-Atom") & !is.nan(relative.rate), ,]# & (added.sum + removed.sum > 1), ,]

atoms.relative.cve.rate$p.value <- mapply(function(a,b,c,d) chisq.test(matrix(c(a,b,c,d), nrow=2))$p.value,
       atoms.added.cve.sums$n.added, atoms.added.cve.sums$n.removed, atoms.relative.cve.rate$added.sum, atoms.relative.cve.rate$removed.sum)

intercept <- 1
atoms.relative.cve.rate.plot <-
  ggplot(atoms.relative.cve.rate, aes(x = reorder(display.atom, relative.rate), y = relative.rate)) +
  theme_minimal() +
  geom_segment(aes(y = intercept, yend = relative.rate, xend = display.atom, size = added.sum+removed.sum,
                   color=relative.rate<intercept),
               show.legend=F) +
  geom_hline(yintercept=intercept) +
  scale_size(range = c(0.3, 7.2)) +
  geom_segment(aes(xend=display.atom, y=relative.rate*ifelse(relative.rate >= intercept, 1.05, 1/1.05),
                   yend=relative.rate*ifelse(relative.rate >= intercept, 1.45, 1/1.45)),
               size=2, color="white") +
  geom_text(aes(label=ifelse(relative.rate >= intercept,
                             paste0(sprintf("                  %0.2fx ", relative.rate),  signif.stars(p.value)),
                             paste0(stringi::stri_reverse(signif.stars(p.value)),  sprintf(" %0.2fx                  ", 1/relative.rate)))),
           color="black", size=3, vjust=0.4) +
  #annotate('rect', xmin = 0.9, xmax = 1.1, ymin = 0.18, ymax = 0.22, fill="white", alpha=0.5) + annotate('text', x=1, y=0.2, label="Inf", size=3) +
  #annotate('rect', xmin = 1.9, xmax = 2.1, ymin = 0.18, ymax = 0.22, fill="white", alpha=0.5) + annotate('text', x=2, y=0.2, label="Inf", size=3) +
  annotate('label', x=2.8, y=2.1, size=3.5, hjust=0, label.size=NA,
           family="DroidSansMono", label=" p<0.1    *\n p<0.01   **\n p<0.001  ***\n p<0.0001 ****") +
  scale_color_manual(values=c(colors2, 'red')) +
  scale_y_log10(position="right", labels=c("Added", "Removed"), breaks=c(.47, 2.3)) +
  labs(x="Atom", y="Atoms added/removed more often in security patches") +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(vjust=0.4),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_flip(ylim = c(0.25, 6.5))
atoms.relative.cve.rate.plot

ggsave("img/atom_relative_cve_rate.pdf", atoms.relative.cve.rate.plot, width=(width<-130), height=width*0.5, units = "mm", device=cairo_pdf)


##############################
#        CWE heatmap
##############################

cwe.cvss.csv <- data.table(read.csv("data/cve_details.csv.xz", header=TRUE))[, -c('Description')]
cwes <- data.table(read.csv("data/all_cwes.csv", header=TRUE))
cwes <- cwes[order(CWE_ID)]

colnames(cwe.cvss.csv)[1] <- "cwe.cvss.csv.idx"
colnames(cwe.cvss.csv)[4] <- "n.exploits"

atoms.removed.unnested <- data.table(atoms.removed.cve %>% 
  mutate(cve.id = strsplit(as.character(cve.ids), ";")) %>% 
  unnest(cve.id))

# Number of unique CVEs, commits, projects
NROW(unique(atoms.removed.unnested$cve.id))
NROW(unique(atoms.removed.unnested$rev.str))
NROW(unique(atoms.removed.unnested$git.repo.url))



atoms.cwe.cvss <- merge(cwe.cvss.csv, atoms.removed.unnested, by.x = "CVE.ID", by.y="cve.id")

atom.cwe.cvss.long <- unique(melt(atoms.cwe.cvss[, c(1,3,8, 27:41)], id.vars=c('CVE.ID', 'CWE.ID', "Score"), variable.name = "atom"))
atom.cwe.cvss.long$cwe.name <- cwes[match(atom.cwe.cvss.long$CWE.ID, as.character(cwes$CWE_ID))][, strtrim(paste0(CWE_TYPE, "-",CWE_ID,": ",CWE_NAME), 80)]


atom.cwe.sums <- atom.cwe.cvss.long[, .(sum=sum(value)), by=c("CWE.ID", "atom")]
atom.cwe.sums.wide <- spread(atom.cwe.sums, key = "CWE.ID", value = "sum")
atom.cwe.sums.wide <- atom.cwe.sums.wide[rowSums(atom.cwe.sums.wide[, -1])>0, ]
atom.cwe.sums.wide <- atom.cwe.sums.wide[, apply(atom.cwe.sums.wide,2,function(x) !all(x==0)), with=FALSE] 
atom.cwe.sums.mat <- as.matrix(atom.cwe.sums.wide[,-c("atom"),])
rownames(atom.cwe.sums.mat) <- atom.cwe.sums.wide$atom
colnames(atom.cwe.sums.mat) <- c(cwes[which(as.character(cwes$CWE_ID) %in% colnames(atom.cwe.sums.mat))][, paste0(CWE_TYPE, "-",CWE_ID,": ",CWE_NAME)], "NA")

atom.cwe.cvss.long <- atom.cwe.cvss.long[!is.na(value)]

atom.cwe.cvss.long[, value.atom.norm := range01(value, na.rm=TRUE), by = atom]
atom.cwe.cvss.long[, value.norm := range01(value.atom.norm, na.rm=TRUE), by = CWE.ID]

atom.cwe.cvss.sum.long <- atom.cwe.cvss.long[, .(count = sum(value)), by = .(atom, cwe.name)]
atom.cwe.cvss.sum.long[, count.atom.norm := range01(count, na.rm=TRUE), by = atom]
atom.cwe.cvss.sum.long[, count.norm := range01(count.atom.norm, na.rm=TRUE), by = cwe.name]

atom.cwe.cvss.sum.wide.normed <- spread(atom.cwe.cvss.sum.long[,-c('count', 'count.atom.norm')], cwe.name, count.norm)
atom.cwe.cvss.mat <- as.matrix(atom.cwe.cvss.sum.wide.normed[, -"atom"], atom.cwe.cvss.sum.wide.normed[, atom])
heatmap(atom.cwe.cvss.mat)


atom.cwe.spot.plot <- ggplot(atom.cwe.cvss.sum.long, aes(cwe.name, atom)) + spot.theme +
  geom_point(colour = "black",     aes(size = 1)) +
  geom_point(colour = "white",     aes(size = 0.8)) +
  geom_point(aes(size = 0.81*count.norm, colour=count)) +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(size = 15)) +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_text(size = 10)) +
  theme(text = element_text(size = 12)) +
  scale_fill_viridis()
atom.cwe.spot.plot

ggsave("img/atom_cwe_spot_plot.pdf", atom.cwe.spot.plot, width=(width<-570), height=width*0.62, units = "mm", device=cairo_pdf)


atom.cwe.cvss.sum.long[is.nan(count.norm), count.norm := 0]

# remove the boring rows/cols

atom.cwe.cvss.sum.long.trimmed <- atom.cwe.cvss.sum.long[cwe.name != "NA-NA: NA", ][,if(sum(count.norm, na.rm=TRUE)>=1 && sum(count) > 20) .SD, by=cwe.name][
  , if(sum(count.norm, na.rm=TRUE)>=1) .SD, by=atom][, atom := droplevels(atom)]

atom.cwe.cvss.sum.long.trimmed.clustered <- cluster.long(atom.cwe.cvss.sum.long.trimmed, 'atom', 'cwe.name', 'count.norm')

atom.cwe.cvss.sum.long.trimmed[, sum(count), by=cwe.name][order(V1)]
atom.cwe.cvss.sum.long.trimmed[, cwe.group.count := sum(.SD$count), by='cwe.name']

atom.cwe.cvss.sum.long.trimmed[, display.atom := convert.atom.names(atom)]

atom.cwe.spot.w.bar.plot <- ggplot(atom.cwe.cvss.sum.long.trimmed, aes(cwe.name, display.atom)) + spot.theme +
  geom_point(colour = "black",     aes(size = 1)) +
  geom_point(colour = "white",     aes(size = 0.8)) +
  geom_point(aes(size = 0.81*count.norm, colour=-count.norm^2)) +
  geom_segment(aes(x=cwe.name, xend = cwe.name, y = 11.7, yend=11.7+cwe.group.count/200), size=10) +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(size = 18), axis.title.y=element_text(size=20, vjust=8)) +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_text(size = 14, vjust=2), axis.title.x=element_text(size=20, vjust=8)) +
  theme(text = element_text(size = 12)) +
  scale_size_continuous(range = c(-1,15)) + # minimum point size
  scale_x_discrete(limits = atom.cwe.cvss.sum.long.trimmed.clustered$colName, position="top") +
  scale_y_discrete(limits = c(convert.atom.names(atom.cwe.cvss.sum.long.trimmed.clustered$rowName), 'Total Size')) +
  labs(x="Weakness (CWE)", y="Atom")+
  scale_fill_viridis() 
atom.cwe.spot.w.bar.plot

ggsave("img/atom_cwe_spot_bar_plot.pdf", atom.cwe.spot.w.bar.plot, width=(width<-210), height=width*1.50, units = "mm", device=cairo_pdf)

atom.cwe.cvss.sum.long.trimmed[grepl("CWE-362", cwe.name), cwe.name := 'CWE-362: Race Condition']
atom.cwe.cvss.sum.long.trimmed[grepl("CWE-119", cwe.name), cwe.name := 'CWE-119: Improper Restriction of Operations\n                  within the Bounds of a Memory Buffer']

atom.cwe.cvss.sum.long.trimmed.clustered <- cluster.long(atom.cwe.cvss.sum.long.trimmed, 'atom', 'cwe.name', 'count.norm')

flipped.cwe.spot <- list(spot.theme,
  geom_point(colour = "black",     aes(size = 1)),
  geom_point(colour = "white",     aes(size = 0.8)),
  geom_point(aes(size = 0.81*count.norm, colour=-count.norm^2)),
  geom_segment(aes(x=cwe.name, xend = cwe.name, y = 11.7, yend=11.7+cwe.group.count/200), size=10),
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(size = 14, hjust=0), axis.title.y=element_text(size=20, vjust=8)),
  theme(axis.ticks.x=element_blank(), axis.text.x=element_text(size = 18), axis.title.x=element_text(size=20, vjust=9999)), #axis.text.x.top = element_text(vjust = 0.5)
  theme(text = element_text(size = 12)),
  scale_size_continuous(range = c(-1,15)), # minimum point size
  scale_x_discrete(limits = atom.cwe.cvss.sum.long.trimmed.clustered$colName, position="bottom"),
  scale_y_discrete(limits = c(convert.atom.names(atom.cwe.cvss.sum.long.trimmed.clustered$rowName), 'Total Size                       '), position="right"), # axis title height
  labs(x="Weakness (CWE)", y="Atom"),
  coord_flip(),
  scale_fill_viridis())

atom.cwe.spot.w.bar.flipped.plot <- ggplot(atom.cwe.cvss.sum.long.trimmed, aes(cwe.name, display.atom)) + flipped.cwe.spot
atom.cwe.spot.w.bar.flipped.plot

ggsave("img/atom_cwe_spot_bar_flipped_plot.pdf", atom.cwe.spot.w.bar.flipped.plot, width=(width<-315), height=width*0.7, units = "mm", device=cairo_pdf)


atom.cwe.cats <- atom.cwe.cvss.sum.long.trimmed[grepl("Category-", cwe.name)][,if(sum(count)>=1).SD,by=atom][, atom:=droplevels(atom)]
atom.cwe.non.cats <- atom.cwe.cvss.sum.long.trimmed[!grepl("Category-", cwe.name)][,if(sum(count)>=1).SD,by=atom][, atom:=droplevels(atom)]

atom.cwe.cats.clustered <- cluster.long(atom.cwe.cats, 'atom', 'cwe.name', 'count.norm')
atom.cwe.non.cats.clustered <- cluster.long(atom.cwe.non.cats, 'atom', 'cwe.name', 'count.norm')

atom.cwe.non.cats.plot <- ggplot(atom.cwe.non.cats, aes(cwe.name, display.atom)) + flipped.cwe.spot +
  scale_x_discrete(limits = atom.cwe.non.cats.clustered$colName) +
  scale_y_discrete(limits = c(convert.atom.names(atom.cwe.non.cats.clustered$rowName), "", "Total"), position="right")
atom.cwe.non.cats.plot

atom.cwe.cats.plot <- ggplot(atom.cwe.cats, aes(cwe.name, display.atom)) + flipped.cwe.spot +
  scale_x_discrete(limits = atom.cwe.cats.clustered$colName) +
  scale_y_discrete(limits = c(convert.atom.names(atom.cwe.cats.clustered$rowName), "Total"), position="right")
atom.cwe.cats.plot


##############################
#        CVSS scores
##############################

atom.cwe.cvss.long[, rounded.score := round(Score)]

mean.atom.cvss <- atom.cwe.cvss.long[value != 0, .(score = mean(Score)), by=atom]
atoms.relative.cve.rate <- merge(atoms.relative.cve.rate, mean.atom.cvss, by='atom')

mean.cvss <- mean(cwe.cvss.csv$Score)
mean.combined.atom.cvss <- atom.cwe.cvss.long[, mean(Score)]

hist(cwe.cvss.csv$Score, breaks=0:10, freq=FALSE, ylim=c(0,.5))
hist(atom.cwe.cvss.long$Score, breaks=0:10, freq=FALSE, ylim=c(0,.5))
atom.cwe.cvss.long[Score == 5 & value != 0][, .(.N), by=atom]
atoms.per.score <- atom.cwe.cvss.long[, .(count.by.score = sum(value)),by=rounded.score]
counts.per.atom <- atom.cwe.cvss.long[, .(count.by.atom = sum(value)),by=atom]


normalized.atom.scores <- atom.cwe.cvss.long[, .(count = sum(value)),by=c('atom', 'rounded.score')][
  atoms.per.score, on='rounded.score'][
    ,normalized.count.by.score := count/count.by.score][
  counts.per.atom, on='atom'][
    ,normalized.count.by.atom  := count/count.by.atom][
    ,normalize.count.by.atom.score := normalized.count.by.atom/count.by.score]

ggplot(normalized.atom.scores, aes(rounded.score, normalize.count.by.atom.score)) +
  geom_line(aes(color=atom))

ggplot(mean.atom.cvss, aes(reorder(atom, -score), score)) +
  geom_bar(stat="identity") +
  theme(axis.ticks.x=element_blank(), axis.text.x=element_text(size = 19, angle = 90, hjust = 0)) +
  geom_hline(yintercept=mean.cvss, color="red", size=2)

with(atoms.relative.cve.rate, rlm(formula=score~log(relative.rate), method="MM"))

ggplot(atoms.relative.cve.rate, aes(relative.rate, score)) +
  geom_point(size=4) +
  geom_text(aes(label=atom), vjust=-1) +
  scale_x_log10()

with(atoms.relative.cve.rate, cor(log(relative.rate), score))

ggplot(atoms.relative.cve.rate[atom.effect.sizes, on="atom"], aes(effect.size, score)) +
  geom_point(size=4) +
  geom_text(aes(label=atom), vjust=-1) +
  scale_x_log10()


################################################
#     Looking for Individual Examples in Code
################################################
out.of.bounds.post <- data.table(merge(atom.cwe.cvss.long[(CWE.ID == 125 | CWE.ID == 787) & (value != 0)], atoms.removed.cve[, .(git.repo.url, rev.str, file, cve.ids)], by.x='CVE.ID', by.y='cve.ids'))
atoms.removed.cve[grepl('CVE-2016-2064', as.character(cve.ids))]

#github.url <- function(git.repo.url, rev.str) str_replace(str_extract(git.repo.url, '(https://github.com/.*).git'), '\\.git', paste0("/commit/", rev.str), )
github.url <- function(git.repo.url, rev.str) str_replace(git.repo.url, '\\.git', paste0("/commit/", rev.str))
out.of.bounds.post[, github.url := github.url(git.repo.url, rev.str)][, .(file, github.url)]


### Examples where very few nodes were removed
inaccessible.repos <- c('git://anongit.freedesktop.org/poppler/poppler')
atoms.removed.cve.denoised <- atoms.removed.cve[!(git.repo.url %in% inaccessible.repos)][any.atoms.removed.cve == TRUE & n.removed < 10 & n.added < 10 &
                                                                                           (n.removed - removed.non.atoms) > (n.added - added.non.atoms)]
atoms.removed.cve.denoised[, github.url := github.url(git.repo.url, rev.str)]
#View(atoms.removed.cve.denoised) # the results are very wide so it's easier to read them as a spreadsheet

atoms.removed.cve.denoised[]
atoms.removed.cve[conditional > 0, .(git.repo.url, rev.str, file, cve.ids, n.removed, n.added, removed.non.atoms, added.non.atoms)]

################################################
#   Why is conditional operator added a lot
################################################
atoms.added.cve[n.added > 100 & conditional > 0]
atoms.added.cve[conditional > 0]
unique(atoms.added.cve$git.repo.url)
