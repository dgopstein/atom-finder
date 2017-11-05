library(data.table)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

file.counts.full <- data.table(read.csv("data/atom-counts_2017-10-25_1.csv", stringsAsFactors = FALSE));
#file.counts <- file.counts.full[1:5]
file.counts <- file.counts.full

file.counts[, atoms := all.nodes - non.atoms]
file.counts[, rate := atoms / all.nodes]
file.counts[, dirs := strsplit(file, "/")]
file.counts$proj <-   sapply(file.counts$dirs, function (x) x[1])
file.counts$module <- sapply(file.counts$dirs, function (x) paste(x[1:2], collapse="/"))


module.rates <- file.counts[, .(rate=(sum(all.nodes) - sum(non.atoms))/sum(all.nodes), count=.N, proj=max(proj)), by=module][count > 1]
module.rates <- transform(module.rates, module=reorder(module, -rate) ) 

ggplot(module.rates[proj=="clang"], aes(module, rate, width=0.5*log(count, base=200), fill=module)) + geom_col() +
  theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1, vjust=0.3))

View(file.counts[module=="clang/INPUTS"])
