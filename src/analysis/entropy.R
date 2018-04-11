library(data.table)
library(ggplot2)
library(viridis)
library(ggridges)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

line.entropies <- data.table(read.csv("../../tmp/atom-finder-token-probabilities_gecko-dev.csv", sep="|"))

line.entropies[, probability := sapply(strsplit(gsub("]|\\[","",probability), ", "), function(x) mean(as.numeric(x)))]

plot(density(line.entropies, na.rm=TRUE), xlim=c(0, 12))

line.nodes <- data.table(read.csv("../../tmp/gecko-node-lines.csv"))

line.node.entropy <- merge(line.nodes, line.entropies, by=c("file", "line"), all=TRUE)
line.node.entropy <- line.node.entropy[file!="anonymously-parsed-code.c"]
line.node.entropy[, any.atom := atom!="non-atoms"]


ast.probability <- line.node.entropy[, .(prob = mean(probability, na.rm=TRUE)), by="ast"]

ggplot(ast.probability) +
  geom_bar(aes(reorder(ast, prob), prob), stat="identity") +
  coord_flip()

atom.probability <- line.node.entropy[, .(prob = mean(probability, na.rm=TRUE)), by="atom"]

ggplot(atom.probability) +
  geom_bar(aes(reorder(atom, prob), prob), stat="identity") +
  coord_flip()

file.node.entropy <- line.node.entropy[, .(atom=atom[1], probability = min(probability)), by=c("file","line")][, .(atom = mean(atom!="non-atoms", na.rm=TRUE), probability = mean(probability, na.rm=TRUE)), by=file]

ggplot(file.node.entropy, aes(atom, probability)) +
  geom_density_2d() +
  stat_density_2d(geom = "raster", aes(fill = ..density..), n = 500, contour = FALSE) +
  scale_fill_viridis(guide=FALSE) +
  xlim(0,0.1)

ggplot(line.node.entropy[!is.na(atom) & !is.na(probability)], aes(x = probability, y = atom, group = atom, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Probability", option = "C", guide=FALSE) +
  scale_x_continuous(limits = c(-1, 12)) +
  labs(title = 'Predictability of Files') +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)

ggplot(line.node.entropy[!is.na(any.atom) & !is.na(probability)], aes(x=probability, fill=any.atom)) +
  geom_density(alpha=0.25, bw=.5) +
  scale_x_continuous(limits=c(-1,12))



atom.rates.probability <- all.atom.rates
setDT(atom.rates.probability, keep.rownames = TRUE)[]
colnames(atom.rates.probability) <- c("atom", "display.name", "rate")
atom.rates.probability[, atom := gsub("\\.", "-", atom)]
atom.rates.probability <- merge(atom.rates.probability, atom.probability, by="atom")
atom.rates.probability <- merge(atom.rates.probability, atom.effect.sizes[, .(atom = gsub("\\.", "-", atom), effect.size = effect.size)], by = "atom")

ggplot(atom.rates.probability, aes(log(rate), prob)) +  geom_point()
ggplot(atom.rates.probability, aes(effect.size, prob)) +  geom_point()

  