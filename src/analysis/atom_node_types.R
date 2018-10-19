library(data.table)
library(ggplot2)
library(RColorBrewer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

atom.node.type <- data.table(read.csv("data/atom-context_2018-10-12_parent-type.csv.xz"))
all.node.type <- data.table(read.csv("data/all-node-counts_2018-08-31_for-esem.csv"))

atom.node.type.by.offset <- atom.node.type[, .(count = .N, atoms = toString(.SD$atom)), by=c('file', 'line', 'offset')]
#atom.node.type.by.offset[, atoms.str := toString(atoms)]
atom.node.type.by.offset[1:100]

atom.node.type.by.offset$atoms

cooccurring.atoms <- atom.node.type.by.offset[, .(count=.N), by=atoms]

n.lines.with.atoms

atom.node.type.sums <- atom.node.type[, .(sum=.N), by=atom]
atom.node.type.counts <- atom.node.type[, .(count=.N), by=c('atom', 'node.type')]
atom.node.type.rates <- atom.node.type.counts[atom.node.type.sums, , on='atom'][, rate := count / sum]


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

# What nodes are omitted curly braces omitted from
atom.node.type.rates[atom=='omitted-curly-braces']
