library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

atom.node.type <- data.table(read.csv("data/atom-context_2018-10-12_parent-type.csv.xz"))
all.node.type <- data.table(read.csv("data/all-node-counts_2018-08-31_for-emse.csv"))

atom.node.type.by.offset <- atom.node.type[, .(count = .N, atoms = toString(.SD$atom)), by=c('file', 'line', 'offset')]

cooccurring.atoms <- atom.node.type.by.offset[, .(count=.N), by=atoms]

atom.node.type.sums <- atom.node.type[, .(sum=.N), by=atom]
atom.node.type.counts <- atom.node.type[, .(count=.N), by=c('atom', 'node.type')]
atom.node.type.rates <- atom.node.type.counts[atom.node.type.sums, , on='atom'][, rate := count / sum]
atom.node.type.group.size <- atom.node.type.rates[, .(n.contexts = .N), by=atom][order(-count)]

atom.parent.type.counts <- atom.node.type[, .(count=.N), by=c('atom', 'parent.type')]
atom.parent.type.rates <- atom.parent.type.counts[atom.node.type.sums, , on='atom'][, rate := count / sum]

atom.node.type.rates[, .SD[order(-rate)][1:3], by="atom"]
atom.parent.type.rates[atom %in% atoms.needing.parent.types$atom][, .SD[order(-rate)][1:3], by="atom"]
atoms.needing.parent.types <- atom.node.type.rates[rate > .999 | atom == 'logic-as-control-flow']

# How is there a preprocessor-in-statment inside a field reference?
# e.g. in clang/lib/Basic/OpenMPKinds.cpp:24 the code uses an x-macro pattern
# with defines inside a field reference
atom.node.type.rates[atom=='preprocessor-in-statement']
atom.node.type[atom=='preprocessor-in-statement' & node.type=='field-reference']

################################################################
#      What nodes are omitted curly braces omitted from
################################################################
n.if.stmts <- all.node.type[node.type=='<IfStatement>', count]
omitted.curly.braces.nodes <- atom.node.type.rates[atom=='omitted-curly-braces'][, .(node.type, atom, count)]
omitted.curly.braces.nodes[node.type=='<ForStatement>']$count <- omitted.curly.braces.nodes[node.type=='<ForStatement>']$count + omitted.curly.braces.nodes[node.type=='<RangeBasedForStatement>']$count
omitted.curly.braces.nodes <- omitted.curly.braces.nodes[node.type!='<RangeBasedForStatement>']

omitted.context <- merge(omitted.curly.braces.nodes, all.node.type, by='node.type', suffixes = c('.atom', '.node'))
omitted.context[, rate := count.atom/count.node]
omitted.context[, total.node := count.node]
omitted.context[, total.rate := rate]

omitted.context.totals <- omitted.context[, .(atom = sum(count.atom), node = sum(count.node))][, .(atom, node, rate = atom/node)]
omitted.context.totals

omitted.context.long <- data.table(tidyr::gather(omitted.context, count.type, count.value, count.atom:count.node, factor_key=TRUE))
omitted.context.long[count.type=='count.node', rate := 1 - rate]
omitted.context.long[, display.node.type := str_replace_all(node.type, '[><]|Statement', '')]

omitted.context.plot <- ggplot(omitted.context.long, aes(reorder(display.node.type, total.rate), rate)) +
  theme_minimal() +
  geom_bar(aes(fill = reorder(count.type, -count.value), width=0.199*log(0.0001*total.node)), stat="identity") +
  #geom_text(aes(y = 0.8, label= total.node), color="white", size=3, vjust=0.4) +
  scale_fill_manual(values=colors2, labels=c("Included", "Omitted")) +
  labs(x="Statement Type", y="Curly Brace Omission Rate") +
  guides(fill=guide_legend(title="Curly Braces")) +
  coord_flip()
omitted.context.plot

ggsave("img/omitted_context_plot.pdf", omitted.context.plot, width=(width<-150), height=width*0.35, units = "mm", device=cairo_pdf)
