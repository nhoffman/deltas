deltaCutoffs <- c(ALB=1, IGAP=10, Ca=1, K=1.5, Na=8, BUN=20, CRE=0.5, MCV=4)
locTypeLabels <- c(ACUTE='Acute',
                   IP='Inpatient',
                   OP='Outpatient')
hospLabels <- c(H='HMC',U='UWMC')
refranges <- list(
                  ALB=c(3.5,5.2),
                  ALK=c(NA,NA),
                  ALT=c(NA,NA),
                  AST=c(15,40),
                  BIL=c(0.2,1.3),
                  BUN=c(8,21),
                  CO2=c(22,32),
                  CRE=c(0.2,1.1),
                  Ca=c(8.9,10.2),
                  Cl=c(98,108),
                  GLU=c(62,125),
                  IGAP=c(NA,NA),
                  K=c(3.7,5.2),
                  Na=c(136,145),
                  TP=c(6.0,8.2),
                  HCT=c(NA,NA),
                  MCV=c(NA,NA)
                  )
explabels <- c('Same','Different')

minSamplesPerPatNum <- 2

## analytes <- c("HCT", "MCV", "Na", "K", "Cl", "CO2", "IGAP", "GLU",
## "BUN", "CRE", "Ca")
