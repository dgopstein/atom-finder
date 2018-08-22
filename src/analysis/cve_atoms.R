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

atoms.removed.cve <- data.table(read.csv("data/cve-patch-atoms_2018-08-08_removed.csv", header=TRUE))
atoms.added.cve   <- data.table(read.csv("data/cve-patch-atoms_2018-08-08_added.csv", header=TRUE))

nrow(atoms.removed.cve)
length(unique(atoms.removed.cve$rev.str))

atoms.removed.cve[is.na(atoms.removed.cve)] <- 0
atoms.added.cve[is.na(atoms.added.cve)] <- 0

atoms.removed.cve.sums <- atoms.removed.cve[, -c('author.name', 'author.email', 'file', 'rev.str', 'git.repo.hash', 'git.repo.url', 'cve.ids')][, lapply(.SD, sum)]
atoms.added.cve.sums <- atoms.added.cve[, -c('author.name', 'author.email', 'file', 'rev.str', 'git.repo.hash', 'git.repo.url', 'cve.ids')][, lapply(.SD, sum)]

atoms.removed.cve[, all.atoms.removed.cve := n.removed - removed.non.atoms] 
atoms.added.cve[, all.atoms.added.cve := n.added - added.non.atoms] 

atoms.removed.cve[, any.atoms.removed.cve := (all.atoms.removed.cve > 0)]
atoms.added.cve[, any.atoms.added.cve := (all.atoms.added.cve > 0)]

###### Summary Statistics #####
atoms.added.cve.sums$n.added - atoms.added.cve.sums$added.non.atoms
atoms.removed.cve.sums$n.removed - atoms.removed.cve.sums$removed.non.atoms

mean(atoms.added.cve$all.atoms.added.cve / atoms.added.cve$n.added, na.rm=TRUE)
mean(atoms.removed.cve$all.atoms.removed.cve / atoms.removed.cve$n.removed, na.rm=TRUE)

chisq.test(matrix(c(atoms.removed.cve.sums$n.removed - atoms.removed.cve.sums$removed.non.atoms, atoms.removed.cve.sums$n.removed,
           atoms.added.cve.sums$n.added - atoms.added.cve.sums$added.non.atoms, atoms.added.cve.sums$n.added), nrow=2))

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
atoms.relative.cve.rate <- atoms.relative.cve.rate[!display.atom %in% c("Non-Atom") & !is.nan(relative.rate) & (added.sum + removed.sum > 1), ,]

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
                             paste0(signif.stars(p.value),  sprintf(" %0.2fx                  ", 1/relative.rate)))),
           color="black", size=3, vjust=0.4) +
  #annotate('rect', xmin = 0.9, xmax = 1.1, ymin = 0.18, ymax = 0.22, fill="white", alpha=0.5) + annotate('text', x=1, y=0.2, label="Inf", size=3) +
  #annotate('rect', xmin = 1.9, xmax = 2.1, ymin = 0.18, ymax = 0.22, fill="white", alpha=0.5) + annotate('text', x=2, y=0.2, label="Inf", size=3) +
  annotate('label', x=2.1, y=2.1, size=3.5, hjust=0, label.size=NA,
           family="DroidSansMono", label=" p<0.05   *\n p<0.01   **\n p<0.001  ***\n p<0.0001 ****") +
  scale_color_manual(values=c(colors2, 'red')) +
  scale_y_log10(position="right", labels=c("Added", "Removed"), breaks=c(.47, 2.3)) +
  labs(x="Atom", y="Atoms added/removed more often") +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(vjust=0.4),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_flip(ylim = c(0.4, 6))
atoms.relative.cve.rate.plot

ggsave("img/atom_relative_cve_rate.pdf", atoms.relative.cve.rate.plot, width=(width<-150), height=width*0.7, units = "mm", device=cairo_pdf)


##############################
#        CWE heatmap
##############################

cwe.cvss.csv <- data.table(read.csv("data/cve_details_complete_clean.csv", header=TRUE))[, -c('Description')]
cwes <- data.table(read.csv("data/all_cwes.csv", header=TRUE))
cwes <- cwes[order(CWE_ID)]

colnames(cwe.cvss.csv)[1] <- "cwe.cvss.csv.idx"
colnames(cwe.cvss.csv)[4] <- "n.exploits"

atoms.removed.unnested <- data.table(atoms.removed.cve %>% 
  mutate(cve.id = strsplit(as.character(cve.ids), ";")) %>% 
  unnest(cve.id))

atoms.cwe.cvss <- merge(cwe.cvss.csv, atoms.removed.unnested, by.x = "CVE.ID", by.y="cve.id")

atom.cwe.cvss.long <- melt(atoms.cwe.cvss[, c(1,3,8, 27:41)], id.vars=c('CVE.ID', 'CWE.ID', "Score"), variable.name = "atom")
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

  
##############################
#        CVSS scores
##############################

mean.atom.cvss <- atom.cwe.cvss.long[value != 0, .(score = mean(Score)), by=atom]
atoms.relative.cve.rate <- merge(atoms.relative.cve.rate, mean.atom.cvss, by='atom')

mean.cvss <- mean(cwe.cvss.csv$Score)

hist(cwe.cvss.csv$Score, breaks=0:10, freq=FALSE, ylim=c(0,.5))
hist(atom.cwe.cvss.long$Score, breaks=0:10, freq=FALSE, ylim=c(0,.5))

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
