library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#gcc.bugs.csv <- data.table(read.csv("../../gcc-bugs.csv"))
gcc.bugs.csv <- data.table(read.csv("../../gcc-bugs_2017-03-28_200.csv"))
gcc.bugs.csv[, source.change := source.chars.after - source.chars.before]

names(gcc.bugs.csv)

nrow(gcc.bugs.csv)

atoms <- gcc.bugs.csv[, .(commits = .N, bug.commits = sum(n.bugs > 1), source.change = sum(source.change),
                          source.chars.before = sum(source.chars.before), source.chars.after = sum(source.chars.after),
                          count.before = sum(count.before), count.after = sum(count.after),
                          decreased = sum(count.after < count.before), increased = sum(count.after > count.before)), by=atom]
atoms

tail(gcc.bugs.csv)

atoms.bugs <- gcc.bugs.csv[, .(commits = .N, bug.commits = sum(n.bugs > 1), source.change = sum(source.change),
                          source.chars.before = sum(source.chars.before), source.chars.after = sum(source.chars.after),
                          count.before = sum(count.before), count.after = sum(count.after),
                          decreased = sum(count.after < count.before), increased = sum(count.after > count.before)),
                          by=.(atom, is.bug = n.bugs > 0)]
atoms.bugs

#cnts <- atoms.bugs[, .(TT = sum(is.bug*decreased), TF = sum(is.bug*increased), FT = sum((1-is.bug)*decreased), FF = sum((1-is.bug)*increased)), by=.(atom)]
cnts.norm <- atoms.bugs[, .(TT = sum(is.bug*decreased/source.change), TF = sum(is.bug*increased/source.change), FT = sum((1-is.bug)*decreased/source.change), FF = sum((1-is.bug)*increased/source.change)), by=.(atom)]


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
cnt.norm.mats <- mapply(function(TT,TF,FT,FF)
  matrix(as.integer(1000000*c(TT, TF, FT, FF)), nrow=2, dimnames=list(c("bugT","bugF"), c("atmRmv","atomAdd")), byrow=TRUE),
  cnts.norm$TT, cnts.norm$TF, cnts.norm$FT, cnts.norm$FF, SIMPLIFY = FALSE)
cnt.norm.mats

names(cnt.norm.mats) <- cnts.norm$atom


#fishers <- sapply(cnt.norm.mats, function(m) fisher.test(m), simplify=FALSE)
fishers <- sapply(cnt.norm.mats, function(m) chisq.test(m), simplify=FALSE)
unlist(lapply(fishers, function(x) x$p.value))

#######
## NORMALIZE by lines in commit
############