library(ggplot2)
library(grid)
library(reshape)

## custom ggplot theme
source('scripts/ggplot2theme.R')

source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

## sources
aucs <- loadf(sources[1])[[1]]
aucSim <- loadf(sources[2])[[1]]

## Sort and extract the AUCs
aucVals <- sort(sapply(aucSim, '[[', 'auc'),
                decreasing=TRUE, na.last=TRUE)

aucVals <- as.data.frame(aucVals)
aucVals$Analyte <- rownames(aucVals)
colnames(aucVals) <- c('AUC', 'Analyte')

## reshape the data to long format for faceting
aucsL <- melt(aucs, idvar=hosp)
aucsL$hosp <- ifelse(aucsL$hosp == 'H', '1', '2')

## order by median AUC at hosp 1
levels <- names(sort(sapply(with(subset(aucsL, hosp == '1'), split(value, variable)), median), decreasing=TRUE))
aucsL$variable <- factor(aucsL$variable, levels=levels, ordered=TRUE)

## trick ggplot2 into placing the mean AUC in the middle of HMC
aucVals2 <- aucVals
aucVals2$AUC <- rep(NA, nrow(aucVals2))

aucValsc <- rbind(aucVals, aucVals2)

pdfFile <- targets[1]

## generate the figure
pdf(pdfFile, width=6, height=2.5)

p <- ggplot(mapping = aes(variable, value))+
  theme_QC()+
  geom_boxplot(data=aucsL, aes(fill=hosp), outlier.size=1)+
  geom_point(data=aucValsc, aes(Analyte, AUC), colour='red', shape=8, position=position_dodge(width=.85))+
  scale_fill_manual(values = c('white', 'grey'), name='Medical\nCenter')+
  labs(x='Analyte', y='AUC')

print(p)

invisible(dev.off())
