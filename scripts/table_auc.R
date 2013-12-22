library(xtable)

source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

## sources
deltaROC <- loadf(sources[1])[[1]]
cutoffs <- loadf(sources[2])[[1]]
aucDist <- loadf(sources[3])[[1]]

## calculate statistics aucDist

tab <- lapply(names(aucDist[1:11]),
              function(x){
                mh <- with(aucDist, mean(subset(aucDist[x], hosp=='H')))
                mu <- with(aucDist, mean(subset(aucDist[x], hosp=='U')))
                sdh <- with(aucDist, sd(subset(aucDist[x], hosp=='H')))
                sdu <- with(aucDist, sd(subset(aucDist[x], hosp=='U')))
                upperh <- mh + 1.96 * sdh
                lowerh <- mh - 1.96 * sdh
                upperu <- mu + 1.96 * sdu
                loweru <- mu - 1.96 * sdu
                
                dats <- data.frame(HMCmean = mh,
                                  HMClower=lowerh,
                                  HMCupper=upperh,
                                  UWMCmean = mu,
                                  UWMClower=loweru,
                                  UWMCupper=upperu
                                  )
                dats
              })

tabt <- do.call(rbind, tab)
tabt$analyte <- rownames(tab)

## target
texfile <- targets[1]

aucVals <- sort(sapply(deltaROC, '[[', 'auc'),
                decreasing=TRUE, na.last=TRUE)

tabt <- tabt[names(aucVals),]

dat <- data.frame(Cutoffs = cutoffs[names(aucVals), 'current'],
                  HMCx = tabt$HMCmean,
                  Hci = paste(signif(tabt$HMClower, digits=3), ', ',signif(tabt$HMCupper, digits=3), sep=''),
                  UWMCx = tabt$UWMCmean,
                  Uci = paste(signif(tabt$UWMClower, digits=3),', ', signif(tabt$UWMCupper, digits=3), sep=''),
                  row.names = names(aucVals)
                  )

asLatex <- function(x){
  print(xtable(dat, digits=c(1,1,3,1,3,1)),
        only.contents=TRUE,
        include.colnames=FALSE,
        file= '/dev/null')

}

page <- '
\\begin{tabular}{cc|cc|cc}
\\hline
\\multicolumn{2}{c|}{} & \\multicolumn{2}{c}{Medical Center 1} & \\multicolumn{2}{c}{Medical Center 2}\\\\
& Cutoffs & AUC $\\bar{x}$ & 95\\%% CI & AUC $\\bar{x}$ & 95\\%% CI\\\\
%s
\\end{tabular}'
cat(
    gettextf(page,
             asLatex(dat)
             ),
    file = texfile
    )

