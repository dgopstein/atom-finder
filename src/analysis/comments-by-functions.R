library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

chi.test <- function(a, b, c, d) {
  cnt.tbl.out <- matrix(c(a, b, c, d), nrow=2)
  n.out <- sum(cnt.tbl.out)

  X2.out <- chisq.test(cnt.tbl.out)

  list(p.value = X2.out$p.value, es = sqrt(X2.out$statistic / n.out), ratio=(a/b)/(c/d))
}

individual.atom.comments <- data.table(read.csv("data/comment-summary_2017-12-31.csv", header=TRUE))
individual.atom.comments[, any.atom := atom != '', ]
individual.atom.comments[, comment := as.logical(ifelse(as.character(comment) == 'true', TRUE, ifelse(as.character(comment)=='false', FALSE, comment))), ]

all.atom.comments <- individual.atom.comments[, .(count = sum(count)), by=c("comment", "any.atom")]

all.atom.comments.widths <- all.atom.comments[, .(width=sum(count)), by=comment][, .(comment, width = width/sum(width))]
all.atom.comments <- merge(all.atom.comments, all.atom.comments.widths, by="comment")


(all.atom.comments[comment==TRUE & any.atom==TRUE, count] / all.atom.comments[any.atom==TRUE, sum(count)]) /
(all.atom.comments[comment==TRUE & any.atom==FALSE, count] / all.atom.comments[any.atom==FALSE, sum(count)])

all.atom.comments[comment==TRUE, sum(count)]
all.atom.comments[comment==FALSE, sum(count)]


# inside/outside function together
chi.test(all.atom.comments[comment==TRUE  & any.atom == TRUE,  count],
         all.atom.comments[comment==FALSE & any.atom == TRUE,  count],
         all.atom.comments[comment==TRUE  & any.atom == FALSE, count],
         all.atom.comments[comment==FALSE & any.atom == FALSE, count])

fisher.test(matrix(c(all.atom.comments[comment==TRUE  & any.atom == TRUE,  count],
         all.atom.comments[comment==FALSE & any.atom == TRUE,  count],
         all.atom.comments[comment==TRUE  & any.atom == FALSE, count],
         all.atom.comments[comment==FALSE & any.atom == FALSE, count]), nrow=2))

# chi-square plot
p <- ggplot(all.atom.comments, aes(x=comment,y=count, width=1.93*width)) +
  theme_classic() +
  geom_bar(aes(fill=any.atom), position = "fill", stat = "identity", colour="white", lwd = 1.5) +
  coord_cartesian(xlim = c(0.5, 1.7)) +
  scale_fill_manual(values = rev(colors2)) +
  labs(x="Comment") +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = c(1.16, 0.7), plot.margin = unit(c(5,40,1,1), "mm")) +
  annotate("text", x=2.3, y=0.35, label='bold("p-value      < 1e-177")', parse=TRUE, hjust=-0.05, size=4.0) +
  annotate("text", x=2.3, y=0.25, label=paste0('bold("Effect Size φ: ', round(1.97, 2), '")'), parse=TRUE, hjust=-0.05, size=4.0) +
  guides(fill=guide_legend(title="Near Atom", hjust=0))

no.clip(p)

# fisher plot
comment.rates <- data.table(atom = c(TRUE, FALSE),
   rate = c((all.atom.comments[comment==TRUE & any.atom==TRUE, count] / all.atom.comments[any.atom==TRUE, sum(count)]),
            (all.atom.comments[comment==TRUE & any.atom==FALSE, count] / all.atom.comments[any.atom==FALSE, sum(count)])))

ggplot(comment.rates, aes(atom, rate)) +
  theme_classic() +
  geom_col(aes(fill=(atom==atom))) +
  scale_fill_manual(values = colors2) +
  guides(fill=FALSE)
  
########################################
#        Comments by Project
########################################

individual.atom.comments.proj <- data.table(read.csv("data/comment-summary-projects_2018-01-02.csv", header=TRUE))
individual.atom.comments.proj[, any.atom := atom != '', ]
individual.atom.comments.proj[, comment := as.logical(ifelse(as.character(comment) == 'true', TRUE, ifelse(as.character(comment)=='false', FALSE, comment))), ]

# All atoms, by project
all.atom.comments.proj <- individual.atom.comments.proj[, .(count = sum(count)), by=c("comment", "any.atom", "project")]

all.atom.comments.proj.wide <- data.frame(t(tidyr::spread(all.atom.comments.proj, key = project, value = count)))
all.atom.comments.proj.wide <- setDT(all.atom.comments.proj.wide, keep.rownames = TRUE)
colnames(all.atom.comments.proj.wide) <- c('project', 'nc.na', 'nc.a', 'c.na', 'c.a')
all.atom.comments.proj.wide <- all.atom.comments.proj.wide[!project %in% c('comment', 'any.atom'), ]

all.atom.comments.proj.wide[, a := nc.a + c.a][, na := nc.na + c.na]
all.atom.comments.proj.wide[, comment.rate.a := c.a / a][, comment.rate.na := c.na / na]
all.atom.comments.proj.wide[, .(project, comment.rate.a - comment.rate.na )]

# All projects, by atom
all.proj.comments <- individual.atom.comments.proj[, .(count = sum(count)), by=c("comment", "atom")]

comment.count     <- all.proj.comments[atom=='' & comment==TRUE, count]
non.comment.count <- all.proj.comments[atom=='' & comment==FALSE, count]

all.proj.comments <- all.proj.comments[atom != '']
all.proj.comments[, rate := ifelse(comment, count/comment.count, count/non.comment.count)]

ggplot(all.proj.comments, aes(atom, rate)) +   
  geom_bar(aes(fill = comment), position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
