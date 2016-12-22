conditionals <- matrix(c(63, 19, 2384, 1262), nrow=2)
conditionals <- matrix(c(13350, 355, 7201, 100), nrow=2)
conditionals <- matrix(c(179, 1048, 13640, 32979), nrow=2)
conditionals <- matrix(c(636, 3191, 41604, 163485), nrow=2)
conditionals <- matrix(c(827,	2463, 14430,	35472), nrow=2)
conditionals <- matrix(c(1216, 3623, 21050, 51866), nrow=2)

chisq <- chisq.test(conditionals)
chisq
sqrt(chisq$statistic / sum(conditionals))

conditionals[,1]/conditionals[,2]
