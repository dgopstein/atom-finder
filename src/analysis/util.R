atom.names.key     <- c("assignment.as.value", "comma.operator", "conditional",
                        "implicit.predicate", "literal.encoding", "logic.as.control.flow",
                        "macro.operator.precedence", "omitted.curly.braces", "operator.precedence",
                        "post.increment", "pre.increment", "preprocessor.in.statement",
                        "repurposed.variable", "reversed.subscript", "type.conversion",
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

