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


all.atom.comments[comment==TRUE & any.atom==TRUE, count] / all.atom.comments[any.atom==TRUE, sum(count)]
all.atom.comments[comment==TRUE & any.atom==FALSE, count] / all.atom.comments[any.atom==FALSE, sum(count)]

all.atom.comments[comment==TRUE, sum(count)]
all.atom.comments[comment==FALSE, sum(count)]


# inside/outside function together
chi.test(all.atom.comments[comment==TRUE  & any.atom == TRUE,  count],
         all.atom.comments[comment==FALSE & any.atom == TRUE,  count],
         all.atom.comments[comment==TRUE  & any.atom == FALSE, count],
         all.atom.comments[comment==FALSE & any.atom == FALSE, count])

p <- ggplot(all.atom.comments, aes(x=comment,y=count, width=1.93*width)) +
  theme_classic() +
  geom_bar(aes(fill=any.atom), position = "fill", stat = "identity", colour="white", lwd = 1.5) +
  coord_cartesian(xlim = c(0.7, 1.7)) +
  scale_fill_manual(values = rev(colors2)) +
  labs(x="Comment") +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = c(1.16, 0.7), plot.margin = unit(c(5,40,1,1), "mm")) +
  annotate("text", x=2.3, y=0.35, label='bold("p-value      < 1e-177")', parse=TRUE, hjust=-0.05, size=4.0) +
  annotate("text", x=2.3, y=0.25, label=paste0('bold("Effect Size φ: ', round(1.97, 2), '")'), parse=TRUE, hjust=-0.05, size=4.0) +
  guides(fill=guide_legend(title="Near Atom", hjust=0))

no.clip(p)

