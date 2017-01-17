;(write-ast (parse-expr "int c = 1 + 3;"))
;(map write-ast (children (parse-expr "int c = 1 + 3;")))
;(print-tree(first(children(tu (resource-path "filename")))) returns a string of path to the file
;leaf?
;(map print-tree(logic-as-control-flow-atoms (first (children (tu (resource-path "logic-as-control-flow.c"))))))
;(map print-tree(decrement-increment-atoms(tu (resource-path "decrement-increment.c"))))
