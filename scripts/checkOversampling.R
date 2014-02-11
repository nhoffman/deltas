checkSampling <- function(x, y, samples){
  ## x,y - names of columns in samples to compare
  pairs <- gettextf('%i-%i', samples[[x]], samples[[y]])
  table(table(pairs))/nrow(samples)
}

source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=2)
sources <- args$sources
targets <- args$targets

## source
dataFile <- sources[1]
sampleFile <- sources[2]

## target
outfile <- targets[1]
figfile <- targets[2]

samples <- loadf(sampleFile)$samples

## determine the degree of oversampling
iTab <- sort(table(samples$i), decreasing=TRUE)
jTab <- sort(table(samples$j), decreasing=TRUE)
kTab <- sort(table(samples$k), decreasing=TRUE)
ijPairs <- checkSampling('i','j', samples)
ikPairs <- checkSampling('i','k', samples)

## dat <- do.call(rbind,lapply(seq(100, nrow(samples), 100), function(N){
##   c(N=N, checkSampling('i','j', samples[1:N,]))
## }))

dat <- do.call(rbind, lapply(seq(100, nrow(samples), 100), function(N){
  data.frame(N=N, checkSampling('i', 'j', samples[1:N,]))
}))

library(ggplot2)
library(grid)

source('scripts/ggplot2theme.R')

colnames(dat) <- c('N', 'Group', 'Freq')

datSub <- subset(dat, Group==1)

save(datSub, file=outfile)

jpeg(figfile)

q <- ggplot(datSub, aes(x=N, y=Freq))
qq <- q + geom_line()+
  theme_QC()+
 facet_wrap(~Group, scales='free')

print(qq)

invisible(dev.off())
