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
str(samples)
## determine the degree of oversampling



print('i')
iTab <- sort(table(samples$i), decreasing=TRUE)
print(table(iTab))

print('j')
jTab <- sort(table(samples$j), decreasing=TRUE)
print(table(jTab))

print('k')
kTab <- sort(table(samples$k), decreasing=TRUE)
print(table(kTab))

ijPairs <- checkSampling('i','j', samples)
print(ijPairs)
ikPairs <- checkSampling('i','k', samples)
print(ikPairs)

## dat <- do.call(rbind,lapply(seq(100, nrow(samples), 100), function(N){
##   c(N=N, checkSampling('i','j', samples[1:N,]))
## }))

dat <- do.call(rbind, lapply(seq(100, nrow(samples), 100), function(N){
  data.frame(N=N, checkSampling('i', 'j', samples[1:N,]))
}))

library(ggplot2)
themeObj <- function(base_size=15){

  none <- theme_blank()
  
  structure(list(
                 panel.grid.major=none,
                 panel.grid.minor=none,
                 panel.background=theme_rect(fill='white'),
                 panel.border=theme_rect(colour='black'),
                 strip.background=theme_rect(colour='white'),
                 legend.key=theme_rect(colour='white'),
                 axis.text.x=theme_text(colour='black', hjust=1, angle=45),
                 axis.text.y=theme_text(colour='black', hjust=1)
                 ),
            class='options'
            )
}
colnames(dat) <- c('N', 'Group', 'Freq')

datSub <- subset(dat, Group==1)

save(datSub, file=outfile)

q <- ggplot(datSub, aes(x=N, y=Freq))
qq <- q + geom_line()+
  themeObj()
  #facet_wrap(~Group, scales='free')

pdf(figfile)
print(qq)
invisible(dev.off())
