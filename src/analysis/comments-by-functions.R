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

# inside/outside function together
chi.test(atom.in.fn.with.cmnt + atom.out.fn.with.cmnt,
         atom.in.fn.no.cmnt + atom.out.fn.no.cmnt,
         nonatom.in.fn.with.cmnt + nonatom.out.fn.with.cmnt,
         nonatom.in.fn.no.cmnt + nonatom.out.fn.no.cmnt)

comment.atoms <- data.table(count = c(atom.in.fn.with.cmnt + atom.out.fn.with.cmnt,
                                      atom.in.fn.no.cmnt + atom.out.fn.no.cmnt,
                                      nonatom.in.fn.with.cmnt + nonatom.out.fn.with.cmnt,
                                      nonatom.in.fn.no.cmnt + nonatom.out.fn.no.cmnt),
                            atom = c(T, T, F, F), comment = c(T,F,T,F))
comment.atoms[, width := sum(count), by=comment][, width := 2*width/sum(width)]

p <- ggplot(comment.atoms, aes(x=comment,y=count, width=1.93*width)) +
  theme_classic() +
  geom_bar(aes(fill=atom), position = "fill", stat = "identity", colour="white", lwd = 1.5) +
  coord_cartesian(xlim = c(0.7, 1.7)) +
  scale_fill_manual(values = rev(colors2)) +
  labs(x="Comment") +
  theme(axis.title.y=element_blank()) +
  theme(legend.position = c(1.16, 0.7), plot.margin = unit(c(5,40,1,1), "mm")) +
  annotate("text", x=2.3, y=0.35, label='bold("p-value      < 1e-177")', parse=TRUE, hjust=-0.05, size=4.0) +
  annotate("text", x=2.3, y=0.25, label=paste0('bold("Effect Size φ: ', round(1.97, 2), '")'), parse=TRUE, hjust=-0.05, size=4.0) +
  guides(fill=guide_legend(title="Near Atom", hjust=0))

no.clip(p)

