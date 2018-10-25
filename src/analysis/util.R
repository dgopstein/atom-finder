atom.names.dot     <- c("assignment.as.value", "comma.operator", "conditional",
                        "implicit.predicate", "literal.encoding", "logic.as.control.flow",
                        "macro.operator.precedence", "omitted.curly.braces", "operator.precedence",
                        "post.increment", "pre.increment", "preprocessor.in.statement",
                        "repurposed.variable", "reversed.subscript", "type.conversion")

atom.names.key     <- c(atom.names.dot,
                        #--------------------------------------------------------------
                        "assignment-as-value", "comma-operator",
                        "implicit-predicate", "literal-encoding", "logic-as-control-flow",
                        "macro-operator-precedence", "omitted-curly-braces", "operator-precedence",
                        "post-increment", "pre-increment", "preprocessor-in-statement",
                        "repurposed-variable", "reversed-subscript", "type-conversion")

atom.names.display <- c("Assignment as Value", "Comma Operator", "Conditional Operator",
                        "Implicit Predicate", "Literal Encoding", "Logic as Control Flow",
                        "Macro Operator Precedence", "Omitted Curly Brace", "Operator Precedence",
                        "Post-Increment", "Pre-Increment", "Preprocessor in Statement",
                        "Repurposed Variable", "Reversed Subscript", "Type Conversion",
                        #--------------------------------------------------------------
                        "Assignment as Value", "Comma Operator",
                        "Implicit Predicate", "Literal Encoding", "Logic as Control Flow",
                        "Macro Operator Precedence", "Omitted Curly Brace", "Operator Precedence",
                        "Post-Increment", "Pre-Increment", "Preprocessor in Statement",
                        "Repurposed Variable", "Reversed Subscript", "Type Conversion")

atom.name.conversion <- as.list(atom.names.display)
names(atom.name.conversion) <- atom.names.key

convert.atom.names <- function(names) unlist(sapply(names, function(name) {
  new.name <- unlist(atom.name.conversion[as.character(name)])
  ifelse(is.null(new.name), "Non-Atom", new.name)
  }))

domain.levels <- c("os", "browser", "compiler", "db", "vcs", "editor", "webserver")
domain.abbrev <- c("os ", "br", "cm", "db", "vc", "ed", "ws")

domain.lookup <- data.table(name = domain.levels, abbrev = domain.abbrev)

convert.domain.abbrev <- function(domain) domain.lookup[name==domain]$abbrev

# https://experience.sap.com/fiori-design-web/values-and-names/
sap.qualitative.palette <- c('#5cbae6', '#b6d957', '#fac364', '#8cd3ff', '#d998cb', '#f2d249', '#93b9c6')

#colors2 <- RColorBrewer::brewer.pal(3, "Set3")[c(2,3)]
set3 <- RColorBrewer::brewer.pal(12, "Set3")

set2 <- RColorBrewer::brewer.pal(8, "Set2")
set2.7 <- RColorBrewer::brewer.pal(7, "Set2")
domain.colors <- RColorBrewer::brewer.pal(8, "Set2")[-7]
colors2 <- set2[c(3,6)]
colors2dark <- set2[c(3,4)]

# https://gist.github.com/Jfortin1/72ef064469d1703c6b30
lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- pmin(col*factor, 255)
  col <- rgb(t(col), maxColorValue=255)
  col
}

no.clip <- function(p) {
  print(p)
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  grid::grid.draw(gt)
  gt
}

effect.size <- c(0.52, 0.30, 0.36, 0.24, 0.63, 0.48, 0.53, 0.22, 0.33, 0.45, 0.28, 0.54, 0.22, 0.40, 0.42)

atom.effect.sizes <- as.data.table(cbind.data.frame(atom = c("assignment.as.value", "comma.operator", "conditional", "implicit.predicate", "literal.encoding", "logic.as.control.flow",
                                               "macro.operator.precedence", "omitted.curly.braces", "operator.precedence", "post.increment", "pre.increment", "preprocessor.in.statement",
                                               "repurposed.variable", "reversed.subscript", "type.conversion"),
                                      effect.size))

signif.stars <- function(p.value) {
  ifelse(p.value < 0.0001, "****",
  ifelse(p.value < 0.001, "*** ",
  ifelse(p.value < 0.01, "**  ",
  ifelse(p.value < 0.1, "*   ", "    "))))
}

spot.theme <- list(
  theme_classic(),
  theme(axis.ticks.x=element_blank(), axis.text.x=element_text(size = 19, angle = 90, hjust = 0)),
  theme(axis.ticks.y=element_blank(), axis.text.y=element_text(size = 19)),
  theme(axis.line=element_blank()),
  theme(text = element_text(size = 22)),
  theme(legend.position = "none"),
  theme(plot.margin = unit(c(10,10,10,10), "mm")),
  scale_size_continuous(range = c(-0.3, 15)),
  scale_x_discrete(position = "top"))

range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}


# cluster the rows/columns of a long data.table by making a dendogrammed-heatmap out of it
cluster.long <- function(dt, row, col, value) {
  dt.sparse <- do.call(tidytext::cast_sparse, list(dt, row, col, value))
  dt.mat <- as.matrix(dt.sparse)
  
  row.ind <- cluster::agnes(dt.mat)
  col.ind <- cluster::agnes(t(dt.mat))
  
  list(rowInd = row.ind$order, colInd = col.ind$order, rowName = row.ind$order.lab, colName = col.ind$order.lab)
}
