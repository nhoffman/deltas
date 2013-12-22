library(ROCR)
library(lattice)

source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

## sources
deltaROC <- loadf(sources[1])[[1]]
experiment <- loadf(sources[2])[[1]]
cutoffs <- loadf(sources[3])[[1]]

## target
pdfFile <- targets[1]

aucVals <- sort(sapply(deltaROC, '[[', 'auc'),
                decreasing=TRUE, na.last=TRUE)

plotdata <- experiment$transposedDeltas

## order panels by decreasing value of AUC
plotdata$test <- ordered(plotdata$test, names(aucVals))

## make sure that rows of cutoffs correspond to levels of plotdata$test
cutoffs <- cutoffs[levels(plotdata$test),]

pdf(pdfFile)

## see the link below for a description of the hack used to define
## panel-dependent ylim using counter
## http://r.789695.n4.nabble.com/R-panel-dependent-distribution-in-qqmath-td809794.html
lattice.options(counter = 1)
currentCutoffs <- ifelse(is.na(cutoffs$current), 0, cutoffs$current)
ff <- bwplot(delta~mislabel|test,
             horizontal=FALSE,
             data=plotdata,
             prepanel = function(x, y, ...){
               counter <- lattice.getOption("counter")
               boxstats <- do.call(
                                   cbind,
                                   lapply(levels(x),
                                          function(L){
                                            boxplot.stats(subset(y,x==L))$stats
                                          })
                                   )

               ymax <- max(max(boxstats[5,], currentCutoffs[counter]))
               ylim <- c(min(boxstats[1,]), ymax)

               lattice.options(counter = counter + 1)
               list(ylim=ylim)
             },
             panel=function(...){
               ## shade the area between 20% and 80% sensitivity cutoffs
               panel.abline(
                            h=seq(cutoffs[panel.number(),3],
                              cutoffs[panel.number(),2],
                              cutoffs[panel.number(),3]/6),
                            col='grey50',
                            lwd=1,
                            lty=3
                            )
               ## margins of the shaded area
               panel.abline(
                            h = cutoffs[panel.number(),c(2,3)],
                            col = 'grey50',
                            lwd = 1,
                            lty = 3
                            )

               ## the actual boxplots
               panel.bwplot(..., fill='white')

               ## clinical cutoff and PRBE
               panel.abline(
                            h = cutoffs[panel.number(),c(1,4)],
                            col = c('red', 'black'),
                            lwd = 2,
                            lty = c(2,1)
                            )

             },
             auto.key=TRUE,
             scales=list(
                 x=list(cex=0.5),
                 y=list(relation='free', cex=0.75)
                 ),
             between=list(y=1),
             as.table=TRUE,
             do.out=FALSE,
             notch=TRUE
             )
plot(ff)

dev.off()
