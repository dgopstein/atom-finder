library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

gcc.bugs.csv <- data.table(read.csv("../../gcc-bugs.csv"))

names(gcc.bugs.csv)

nrow(gcc.bugs.csv)

atoms <- gcc.bugs.csv[, .(commits = .N, bug.commits = sum(n.bugs > 1),
                          count.before = sum(count.before), count.after = sum(count.after),
                          decreased = sum(count.after < count.before), increased = sum(count.after > count.before)) , by=atom]

tail(gcc.bugs.csv)

atoms

atoms.bugs <- gcc.bugs.csv[, .(commits = .N, bug.commits = sum(n.bugs > 1),
                          count.before = sum(count.before), count.after = sum(count.after),
                          decreased = sum(count.after < count.before), increased = sum(count.after > count.before)),
                          by=.(atom, is.bug = n.bugs > 0)]

cnts <- atoms.bugs[, .(TT = sum(is.bug*decreased), TF = sum(is.bug*increased), FT = sum((1-is.bug)*decreased), FF = sum((1-is.bug)*increased)), by=.(atom)]

# http://stackoverflow.com/questions/24299171/function-to-split-a-matrix-into-sub-matrices-in-r
matsplitter<-function(M, r, c) {
  rg <- (row(M)-1)%/%r+1
  cg <- (col(M)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(M))/r/c
  cv <- unlist(lapply(1:N, function(x) M[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
} 

#is.bug/atom.removal
cnt.mats <- mapply(function(TT,TF,FT,FF)
  matrix(c(TT, TF, FT, FF), nrow=2, dimnames=list(c("bugT","bugF"), c("atmRmv","atomAdd")), byrow=TRUE),
  cnts$TT, cnts$TF, cnts$FT, cnts$FF, SIMPLIFY = FALSE)

names(cnt.mats) <- cnts$atom

fishers <- sapply(cnt.mats, function(m) fisher.test(m), simplify=FALSE)
unlist(lapply(fishers, function(x) x$p.value))

#######
## NORMALIZE by lines in commit
############