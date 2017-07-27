chi.test <- function(a, b, c, d) {
  cnt.tbl.out <- matrix(c(a, b, c, d), nrow=2)
  n.out <- sum(cnt.tbl.out)
  
  X2.out <- chisq.test(cnt.tbl.out)
  
  list(p.value = X2.out$p.value, es = sqrt(X2.out$statistic / n.out), ratio=(a/b)/(c/d))
}

atom.in.fn.with.cmnt    <-   81994
atom.in.fn.no.cmnt      <-  422374
nonatom.in.fn.with.cmnt <-  2189331
nonatom.in.fn.no.cmnt   <- 25156946
chi.test(atom.in.fn.with.cmnt, atom.in.fn.no.cmnt, nonatom.in.fn.with.cmnt, nonatom.in.fn.no.cmnt)

atom.out.fn.with.cmnt    <-    8796
atom.out.fn.no.cmnt      <-   35368
nonatom.out.fn.with.cmnt <-  780808
nonatom.out.fn.no.cmnt   <- 4404317
chi.test(atom.out.fn.with.cmnt, atom.out.fn.no.cmnt, nonatom.out.fn.with.cmnt, nonatom.out.fn.no.cmnt)

# lines edited by atom/non-atom
n.atom.lines <- 346133
n.changed.atom.lines <- 2712
n.non.atom.lines <- 12201031
n.changed.non.atom.lines <- 164474
chi.test(n.atom.lines, n.non.atom.lines, n.changed.atom.lines, n.changed.non.atom.lines)
