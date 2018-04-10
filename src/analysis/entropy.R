library(data.table)
library(ggplot2)
library(viridis)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stream.file <- function(filename, n.lines, init, fun) {
  f = file(filename, "r")
  
  accumulator <- init
  stop = FALSE
  iter = 0
  while(!stop) {
    print(paste("processed lines: ", n.lines * iter));
    iter <- iter + 1;
    
    next.lines = readLines(f, n = n.lines)

    accumulator <- fun(accumulator, next.lines)

    if(length(next.lines) < n.lines) {
      stop = TRUE
      close(f)
    }
  }
  
  accumulator
}

# cat atom-finder-token-probabilities_gecko-dev.csv | cut -d\| -f3 | grep -o '[^][]\+' > atom-finder-token-probabilities_gecko-dev_only-probs.txt
line.entropies <-
  stream.file("../../tmp/atom-finder-token-probabilities_gecko-dev_only-probs.txt", n.lines = 100000, init = c(),
            function(line.entropies, next.lines) {
              line.entropy <- unlist(lapply(strsplit(next.lines, ", "), function(x) mean(as.numeric(x))))
              c(line.entropies, line.entropy)
            })

plot(density(line.entropies, na.rm=TRUE), xlim=c(0, 12))


