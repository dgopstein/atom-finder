# Determine which file extensions parse well/poorly

library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

parse.problems <- data.table(read.csv('../../tmp/parse_problem_rate_full.csv.bz2', header=TRUE))

plot(density(parse.problems[total.nodes > 10]$problem.rate))
hist(parse.problems[total.nodes > 10]$problem.rate)

parse.problems.by.ext <- parse.problems[, .(mean.nodes = mean(total.nodes), sum.nodes = sum(total.nodes), problem.rate = mean(problem.rate)), by=file.ext][order(problem.rate)]

print(parse.problems.by.ext[mean.nodes > 1][order(-sum.nodes)], nrow=3000)


ggplot(parse.problems.by.ext[mean.nodes > 1 & sum.nodes > 100000], aes(mean.nodes, problem.rate)) +
  geom_point(aes(size=sum.nodes), color="green") +
  geom_text(aes(label=file.ext)) +
  scale_x_log10()


hist(parse.problems[total.nodes < 1000][file.ext=='c', log(total.nodes)])
2.71^7
parse.problems[total.nodes < 2][file.ext=='c',]

parse.problems.by.ext[sum.nodes > 10000 & problem.rate < .2][order(problem.rate)]

