atom.names.key     <- c("assignment.as.value", "comma.operator", "conditional",
                        "implicit.predicate", "literal.encoding", "logic.as.control.flow",
                        "macro.operator.precedence", "omitted.curly.braces", "operator.precedence",
                        "post.increment", "pre.increment", "preprocessor.in.statement",
                        "repurposed.variable", "reversed.subscript", "type.conversion")

atom.names.display <- c("Assignment as Value", "Comma Operator", "Conditional Operator",
                        "Implicit Predicate", "Change of Literal Encoding", "Logic as Control Flow",
                        "Macro Operator Precedence", "Omitted Curly Braces", "Operator Precedence",
                        "Post-Increment", "Pre-Increment", "Preprocessor in Statement",
                        "Repurposed Variables", "Reversed Subscripts", "Type Conversion")

atom.name.conversion <- as.list(atom.names.display)
names(atom.name.conversion) <- atom.names.key

