library(data.table)
library(ggplot2)
library(RColorBrewer)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("util.R")

all.parens <- data.table(read.csv("data/all-parens_2018-10-30_03_with-timeout-60.csv"))

parens <- all.parens[selection == 'parens']
superfluous.parens <- all.parens[selection == 'superfluous-parens']
operator.precedence <- all.parens[selection == 'operator-precedence']
multi.op.expr <- all.parens[selection == 'multi-op-expr']

parens[order(-count)][1:20]
superfluous.parens[order(-count)][1:20]
superfluous.parens[!( parent.type == 'sizeof' # sizeof doesn't need parens at all
                    | (parent.type == 'greaterThan' & child.type == '<IdExpression>') # mis-parse of template
                    | (parent.type == 'function-call' & child.type == '<IdExpression>') # sometimes this is fine, sometimes its a mis-parse of type-casting
                     )][order(-count)][1:20]


operator.precedence.node.child <- operator.precedence[, .(count=sum(count)),by=c('parent.type', 'node.type')]
head(operator.precedence.node.child[order(-count)])


print(all.node.type, nrow=200) # from atom_node_types.R
all.expr.types <- all.node.type[!grepl("^<.*>$", node.type)]
total.n.exprs <- all.expr.types[, sum(count)]
n.multi.op.expr <- multi.op.expr[, sum(count)]

parens[, sum(count)] / n.multi.op.expr # rate of parens
superfluous.parens[, sum(count)] / n.multi.op.expr # rate of superfluous parens
op.prec.multi.op.rate <- operator.precedence[, sum(count)] / n.multi.op.expr # rate of operator precedence
op.prec.multi.op.rate

all.node.type[node.type=='bracketedPrimary']
print(all.node.type, nrows = 150)

all.parens.summary <- all.parens[, .(count = sum(count), is.paren = selection == 'parens' || selection == 'superfluous-parens'), by=selection][
                                 selection != 'multi-op-expr', ]
all.parens.summary$selection <- factor(all.parens.summary$selection,
                                       levels = c('multi-op-expr', 'parens', 'superfluous-parens', 'operator-precedence'))

paren.vs.operator.precedence.plot <-
  ggplot(all.parens.summary, aes(reorder(is.paren, count), 100 * count / n.multi.op.expr)) +
  theme_classic() +
  geom_bar(stat="identity", position="identity", alpha=1, aes(fill=selection)) +
  scale_x_discrete(labels=c('Parens', 'Operator\nPrecedence\nAtoms')) +
  scale_fill_manual(values=set2[c(6,1,3)],
                    name="",
                    labels=c("All Parens", "Superfluous Parens", "Operator\nPrecedence\nAtoms")) +
  ylab(label="Percent of\nMulti-Operator Expressions") +
  theme(axis.title.x = element_blank())
ggsave("img/paren_vs_operator_precedence_plot.pdf", paren.vs.operator.precedence.plot, width=(width<-90), height=width*.9, units = "mm", device=cairo_pdf)



superfluous.parens[, rate := count/sum(count)]
superfluous.parens[, both.ops := paste(parent.type, '-', child.type)]

asymmetric.superfluous.parens.flat <- superfluous.parens[, .(position=c('parent', 'child'), node.type=c(rbind(as.character(parent.type), as.character(child.type))), count = rep(count, each=2))]
asymmetric.superfluous.parens.flat.by.position <- asymmetric.superfluous.parens.flat[, .(count = sum(count)), by=c('position', 'node.type')]
asymmetric.superfluous.parens.flat.by.position[, rate := count/sum(count)]

asymmetric.superfluous.parens.flat.summary <- asymmetric.superfluous.parens.flat.by.position[, .(count = sum(count)), by=c('node.type')]

operators <-      c("sizeof", "binaryAnd", "shiftLeft", "binaryOr", "logicalAnd", "<IdExpression>", "logicalOr", "function-call", "star",    "equals", "conditional", "field-reference", "shiftRight",    "plus2",      "minus2",     "assign", "multiply",   "notequals", "cast", "lessThan", "divide",     "binaryOrAssign", "greaterThan", "modulo",     "pmarrow", "binaryXor",   "lessEqual", "pmdot",   "plus1",      "minus1")
operator.types <- c("Unique", "Bitwise",   "Bitwise",   "Bitwise",  "Logic",      "Unique",         "Logic",     "function-call", "Pointer", "Logic",  "Unique",      "Pointer",         "Bitwise",       "Arithmetic", "Arithmetic", "Unique", "Arithmetic", "Logic",     "Unique", "Logic",  "Arithmetic", "Bitwise",        "Logic",       "Arithmetic", "Pointer", "Bitwise",     "Logic",     "Pointer", "Arithmetic", "Arithmetic")
operator.type.conversion <- data.table(cbind(operators, operator.types))

asymmetric.superfluous.parens.flat.summary <- merge(asymmetric.superfluous.parens.flat.summary, operator.type.conversion, by.x='node.type', by.y='operators', all.x=TRUE)

multi.op.expr.flat.summary.by.position <- multi.op.expr[, .(position=c('parent', 'child'), node.type=c(rbind(as.character(node.type), as.character(child.type))), count = rep(count, each=2))]
multi.op.expr.flat.summary <- multi.op.expr.flat.summary.by.position[, .(multi.op.count=sum(count)), by=(node.type)]

asymmetric.superfluous.parens.flat.summary <- merge(asymmetric.superfluous.parens.flat.summary, multi.op.expr.flat.summary)

asymmetric.superfluous.parens.flat.summary[, normalized.count := count / multi.op.count]

asymmetric.superfluous.parens.flat.summary[, rate := normalized.count/sum(normalized.count)]

asymmetric.superfluous.parens.flat.summary[rate > 0.02][order(-rate)][, .(multi.op.count, log(multi.op.count+2000)-7)]
asymmetric.superfluous.parens.flat.summary[rate > 0.02][order(-multi.op.count)][, .((log(multi.op.count+20000)-9.5)/3)]

ggplot(asymmetric.superfluous.parens.flat.summary[rate > 0.02][order(-rate)], aes(reorder(node.type, rate), 100*rate)) +
  theme_minimal() +
  geom_bar(stat="identity", aes(fill=operator.types, width=(log(multi.op.count+20000)-9.5)/3)) +
  theme(axis.text.x = element_text(angle=90)) +
  scale_fill_manual(values=c(set2[3:6], "#aaaaaa")) +
  coord_flip()

superfluous.parens.by.operator.type <- asymmetric.superfluous.parens.flat.summary[rate > 0.02][order(-rate)][, .(rate=sum(rate), multi.op.count=sum(multi.op.count)), by=operator.types]

superfluous.parens.by.operator.type.plot <-
ggplot(superfluous.parens.by.operator.type, aes(reorder(operator.types, rate), rate)) +
  theme_minimal() +
  geom_bar(stat="identity", aes(fill=reorder(operator.types, -rate), width=(log(multi.op.count-45000)-9)/4)) +
  theme(axis.text.x = element_text(angle=0, hjust=1, vjust=.4)) +
  scale_fill_manual(values=set2[c(3,6,8,5,4)]) +
  ylab(label="\nRate relative to\nMulti-Operator Expressions") +
  xlab(label="Operator Type\n") +
  guides(fill=FALSE) +
  coord_flip()

superfluous.parens.by.operator.type.plot
ggsave("img/superfluous_parens_by_operator_type_plot.pdf", superfluous.parens.by.operator.type.plot, width=(width<-110), height=width*.6, units = "mm", device=cairo_pdf)
