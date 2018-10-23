library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")


phi.es <- function(X2, n) sqrt(X2/n)
odd.rat <- function(observed) (observed[1]/observed[2])/(observed[3]/observed[4])
#phi.es <- function(X2) sqrt(X2$statistic/sum(X2$observed))
#odd.rat <- function(X2) with(X2.out, (observed[1]/observed[2])/(observed[3]/observed[4]))

chi.test <- function(a, b, c, d) {
  cnt.tbl.out <- matrix(c(a, b, c, d), nrow=2)
  n.out <- sum(cnt.tbl.out)

  X2.out <- chisq.test(cnt.tbl.out)

  list(X2 = X2.out$statistic, p.value = X2.out$p.value, es = phi.es(X2.out$statistic,n.out), ratio=(a/b)/(c/d))
}

to.bool <- function(x) as.logical(ifelse(as.character(x) == 'true', TRUE, ifelse(as.character(x)=='false', FALSE, x)))

individual.atom.comments <- data.table(read.csv("data/comment-counts_2018-10-11_01_group-results.csv", header=TRUE))
#individual.atom.comments <- data.table(read.csv("data/comment-counts_2018-01-27_01_proximity-1-line.csv", header=TRUE))
individual.atom.comments[, any.atom := atom != '', ]
individual.atom.comments[, comment := to.bool(comment), ]
individual.atom.comments[, in.function := to.bool(in.function), ]


all.atom.comments <- individual.atom.comments[, .(count = sum(count)), by=c("comment", "in.function", "any.atom")]

all.atom.comments.widths <- all.atom.comments[, .(width=sum(count)), by=comment][, .(comment, width = width/sum(width))]
all.atom.comments <- merge(all.atom.comments, all.atom.comments.widths, by="comment")

atom.comment.rate.in.function <-
(all.atom.comments[in.function == TRUE & comment==TRUE & any.atom==TRUE, count] / all.atom.comments[in.function == TRUE & any.atom==TRUE, sum(count)]) /
(all.atom.comments[in.function == TRUE & comment==TRUE & any.atom==FALSE, count] / all.atom.comments[in.function == TRUE & any.atom==FALSE, sum(count)])

(all.atom.comments[in.function == FALSE & comment==TRUE & any.atom==TRUE, count] / all.atom.comments[in.function == FALSE & any.atom==TRUE, sum(count)]) /
(all.atom.comments[in.function == FALSE & comment==TRUE & any.atom==FALSE, count] / all.atom.comments[in.function == FALSE & any.atom==FALSE, sum(count)])


all.atom.comments[comment==TRUE, sum(count)]
all.atom.comments[comment==FALSE, sum(count)]


# inside/outside function together
chi.test(all.atom.comments[in.function == TRUE & comment==TRUE  & any.atom == TRUE,  count],
         all.atom.comments[in.function == TRUE & comment==FALSE & any.atom == TRUE,  count],
         all.atom.comments[in.function == TRUE & comment==TRUE  & any.atom == FALSE, count],
         all.atom.comments[in.function == TRUE & comment==FALSE & any.atom == FALSE, count])

# fisher.test(matrix(c(all.atom.comments[in.function == TRUE & comment==TRUE  & any.atom == TRUE,  count],
#          all.atom.comments[in.function == TRUE & comment==FALSE & any.atom == TRUE,  count],
#          all.atom.comments[in.function == TRUE & comment==TRUE  & any.atom == FALSE, count],
#          all.atom.comments[in.function == TRUE & comment==FALSE & any.atom == FALSE, count]), nrow=2))

atom.comments.no.fun <- all.atom.comments[, .(count=sum(count)), by=c("comment", "any.atom", "width")]

# chi-square plot
p <- ggplot(atom.comments.no.fun, aes(x=comment,y=count, width=1.93*width)) +
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
   rate = c((atom.comments.no.fun[comment==TRUE & any.atom==TRUE, count] / atom.comments.no.fun[any.atom==TRUE, sum(count)]),
            (atom.comments.no.fun[comment==TRUE & any.atom==FALSE, count] / atom.comments.no.fun[any.atom==FALSE, sum(count)])))

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

########################################
#        Comments by Atom
########################################

atoms <- c("post.increment", "preprocessor.in.statement")
atoms <- c("post.increment", "preprocessor.in.statement", "operator.precedence", 
           "literal.encoding", "omitted.curly.braces", "pre.increment", 
           "logic.as.control.flow", "reversed.subscript", "comma.operator", 
           "type.conversion", "repurposed.variable", "macro.operator.precedence", 
           "implicit.predicate", "conditional", "assignment.as.value", "all.atoms")



individual.comment.counts.all.proj <- individual.atom.comments[, .(count = sum(count)), by=c('in.function', 'comment', 'atom', 'any.atom')]

individual.comment.rates.all.proj <-
  merge(individual.comment.counts.all.proj[comment==TRUE], individual.comment.counts.all.proj[comment==FALSE],
        by=c('in.function', 'atom', 'any.atom'), suffixes = c(".comment", ".non.comment"))[, -c("comment.comment", "comment.non.comment")]

individual.comment.rates.all.proj[, any.atom := atom!='']
individual.comment.rates.all.proj[, atom := convert.atom.names(atom)]
individual.comment.rates.all.proj[, count := count.comment + count.non.comment]
individual.comment.rates.all.proj[, comment.rate := count.comment / count]
individual.comment.rates.all.proj[, lower.wilson := Hmisc::binconf(count.comment, count, alpha=10^-6)[,2]]

individual.comment.rates.all.proj.in.fun <- individual.comment.rates.all.proj[in.function==TRUE]
individual.comment.rates.all.proj.in.fun$atom <- with(individual.comment.rates.all.proj.in.fun, reorder(atom,comment.rate))
non.atom.comment.counts <- unlist(individual.comment.rates.all.proj.in.fun[as.character(atom)=="Non-Atom", .(count.comment, count.non.comment)])
individual.comment.rates.all.proj.in.fun[, c("X2", "p.value", "es", "odds") :=
                                         with(chisq.test(matrix(c(count.comment, count.non.comment,non.atom.comment.counts), nrow=2)),
                                              list(X2 = statistic, p.value, es = phi.es(statistic, sum(observed)), odds = odd.rat(observed))),
                                         by=1:nrow(individual.comment.rates.all.proj.in.fun)]
individual.comment.rates.all.proj.in.fun[atom=="Non-Atom", odds := NA]


individual.comment.rates.all.proj.in.fun[, median(odds, na.rm=TRUE)]

individual.comment.rates.all.proj.in.fun.plot <-
  ggplot(individual.comment.rates.all.proj.in.fun, aes(atom, comment.rate)) +
  theme_minimal() +
  #geom_bar(aes(fill = any.atom), stat="identity") +
  geom_bar(aes(fill = any.atom, width=0.099*log(0.001*(ifelse(count < 10^8, count, 10^8) +1000))), stat="identity") +
  geom_text(aes(label=ifelse(is.na(odds), '', sprintf('%0.2f', round(odds, 2)))), angle = 90, vjust=0.5, hjust=-0.3, size=3) +
  coord_cartesian(ylim = c(0, 0.18)) +
  scale_fill_manual(values = rev(colors2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) +
  #labs(title = "Comment Rates Inside Functions") +
  guides(fill=FALSE) +
  labs(x = "Atom", y="Comment Rate")

individual.comment.rates.all.proj.in.fun.plot

ggsave("img/comment_rates_in_fun.pdf", individual.comment.rates.all.proj.in.fun.plot, width=(width<-115), height=width*0.65, units = "mm")


individual.comment.rates.all.proj.out.fun <- individual.comment.rates.all.proj[in.function==FALSE]
individual.comment.rates.all.proj.out.fun$atom <- with(individual.comment.rates.all.proj.out.fun,reorder(atom,comment.rate))

ggplot(individual.comment.rates.all.proj.out.fun, aes(atom, comment.rate)) +
  geom_bar(aes(fill = any.atom, width=0.08*log(0.5*count)), position = "dodge", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Comment Rates Outside Functions")

