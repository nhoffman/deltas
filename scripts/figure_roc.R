library(ROCR)
library(ggplot2)
library(grid)

source('scripts/common.R')
source('scripts/ggplot2theme.R')

args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

deltaROC <- loadf(sources[1])[[1]]
perfData <- loadf(sources[2])[[1]]
perfSub <- perfData[[1]]

## setup dataframe of perf for plotting
perf <- with(perfSub, lapply(perfSub, function(x){
  x$analyte <- rownames(x)
  x
}))


aucVals <- sort(sapply(deltaROC, '[[', 'auc'),
                decreasing=TRUE, na.last=TRUE)

## lines <- lapply(names(deltaROC),
##                function(analyte){
##                  data.frame(sens=deltaROC[[analyte]]$sens,
##                             spec=deltaROC[[analyte]]$spec,
##                             analyte=analyte
##                             )}
##                )

## lines <- do.call(rbind, lines)

## data preparation for ggplot
rocData <- lapply(names(deltaROC),
                  function(analyte){
                    data.frame(
                               specificity=unlist(deltaROC[[analyte]]$roc@x.values),
                               sensitivity=unlist(deltaROC[[analyte]]$roc@y.values),
                               analyte=rep(analyte, length(unlist(deltaROC[[analyte]]$roc@x.values))),
                               auc=rep(aucVals[[analyte]], length(unlist(deltaROC[[analyte]]$roc@x.values)))
                               )
                  })

rocPlot <- do.call(rbind, rocData)

rocPlot <- rocPlot[order(rocPlot$auc, decreasing=TRUE),]

rocPlot$analyte <- factor(rocPlot$analyte, levels=names(aucVals), ordered=TRUE)

## reorder the perfSub data frame to match the roc data
perfOrd <- with(perf, lapply(perf, function(x){
  x <- x[names(aucVals),]
  x$analyte <- factor(x$analyte, levels=names(aucVals), ordered=TRUE)
  rownames(x) <- NULL
  x
}))


## separate for graphing into lines and dots
sens20 <- perfOrd$sens20
sens80 <- perfOrd$sens80
curr <- perfOrd$current
prbe <- perfOrd$prbe

pdfFile <- targets[1]

## figure generation

pdf(pdfFile, width=7, height=6)
## mfrow <- c(4,3)
## par(mfrow=mfrow,
##     mar=c(bottom=4, left=0, top=.85, right=.25),
##     oma=c(5,5,0,0),
##     pin=c(.95,.95))

## for(analyte in names(aucVals)[!is.na(aucVals)]){
##   rdata <- deltaROC[[analyte]]
  
##   plot(rdata$roc,
##        ## print.cutoffs.at=cutoffs[anames],
##        #xaxt="n",
##        #yaxt="n",
##        xlab='',
##        ylab='',
##        colorize=FALSE,
##        lwd=3,
##        main=gettextf('%s    AUC = %.2f', analyte, rdata$auc), cex.main=1)
##   title(xlab='1 - specificity', ylab='sensitivity', outer=T, cex.lab=2)
##   abline(h=rdata$sens,v=1-rdata$spec)
## }

## p <- ggplot(data=rocPlot, aes(x=specificity, y=sensitivity))

## p <- p +  geom_line()+
##   facet_wrap(~analyte)+
##   opts(panel.grid.major=theme_blank())+
##   opts(aspect.ratio = 1)+
##   opts(panel.grid.minor=theme_blank())+
##   opts(panel.background=theme_rect(fill='white'))+
##   opts(panel.border=theme_rect(colour='black'))+
##   opts(axis.text.x=theme_text(colour='black', angle=45, hjust=1))+
##   opts(axis.text.y=theme_text(colour='black', hjust=1))+
##   opts(strip.background=theme_rect(fill='bisque'))+
##   geom_hline(data=lines, aes(yintercept=sens))+
##   geom_vline(data=lines, aes(xintercept=1-spec))

p <- ggplot(data=rocPlot, aes(specificity, sensitivity))+
  theme_QC()+
  geom_line(data=rocPlot)+
  facet_wrap(~analyte)+
  geom_point(data=curr, aes(1-spec, sens), shape=1, size=3)+
  geom_point(data=prbe, aes(1-spec, sens), shape=20, size=3)+
  geom_point(data=sens20, aes(1-spec, sens), shape=24, size=3)+
  geom_point(data=sens80, aes(1-spec, sens), shape=25, size=3)

print(p)

invisible(dev.off())
