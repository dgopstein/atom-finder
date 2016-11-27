library(data.table)

# project, node, count
write.csv(read.csv('../data/github-top-c.csv'))
github.top.c <- data.table(read.csv('../data/github-top-c.csv'))

project.size <- github.top.c[, .(count = sum(count)), by=project][order(-count)]

github.top.c <- merge(github.top.c, project.size, by='project', suffixes = c('.type', '.total'))

github.top.c[, rate:=(count.type/count.total)]

node.rates <- github.top.c[, .(rate = mean(rate)), by=node][order(-rate)]

write.csv(node.rates)
