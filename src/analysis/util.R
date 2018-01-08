atom.names.key     <- c("assignment.as.value", "comma.operator", "conditional",
                        "implicit.predicate", "literal.encoding", "logic.as.control.flow",
                        "macro.operator.precedence", "omitted.curly.braces", "operator.precedence",
                        "post.increment", "pre.increment", "preprocessor.in.statement",
                        "repurposed.variable", "reversed.subscript", "type.conversion")

atom.names.display <- c("Assignment as Value", "Comma Operator", "Conditional Operator",
                        "Implicit Predicate", "Literal Encoding", "Logic as Control Flow",
                        "Macro Operator Precedence", "Omitted Curly Braces", "Operator Precedence",
                        "Post-Increment", "Pre-Increment", "Preprocessor in Statement",
                        "Repurposed Variables", "Reversed Subscripts", "Type Conversion")

atom.name.conversion <- as.list(atom.names.display)
names(atom.name.conversion) <- atom.names.key

convert.atom.names <- function(names) unlist(atom.name.conversion[as.character(names)])

# https://experience.sap.com/fiori-design-web/values-and-names/
sap.qualitative.palette <- c('#5cbae6', '#b6d957', '#fac364', '#8cd3ff', '#d998cb', '#f2d249', '#93b9c6')

colors2 <- sap.qualitative.palette[c(3,4)]

no.clip <- function(p) {
  print(p)
  gt <- ggplot_gtable(ggplot_build(p))
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  grid::grid.draw(gt)
  gt
}
