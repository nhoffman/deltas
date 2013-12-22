## Invoke from SConstruct using
## output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])
library(multicore)

resample <- function(cmp, analytes, iterations, nsamples, hosp, nproc){
  ## Calculate a distribution of AUC values for a given hospital
  ## * iterations - number of sampling experiments
  ## * nsamples - number of samples to select in each experiment

  aucList <- multicore::mclapply(seq(iterations), mc.cores=nproc, FUN=function(iter){
    samples <- functions$getSamples(x = cmp,
                                    limit = cmp$hospital %in% hosp & cmp$LocType %in% 'IP',
                                    iterations = nsamples,
                                    verbose = FALSE)

    ## calculate AUC for each vector of deltas stored as columns in
    ## deltas
    deltas <- with(samples, abs(cmp[rep(i,2),analytes] - cmp[c(j,k),analytes]))
    aucs <- apply(deltas, 2, function(analyte){
      pred <- ROCR::prediction(analyte, rep(c(1,2), each=nrow(samples)))
      unlist(ROCR::performance(pred,'auc')@y.values)
    })
  })

  tab <- as.data.frame(do.call(rbind, aucList))
  tab$hosp <- hosp
  tab
}

source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)

if(Sys.getenv('NPROC')==''){
  nproc <- 2
}else{
  nproc <- as.integer(Sys.getenv('NPROC'))
}

sources <- args$sources
targets <- args$targets

## sources
constants <- sourcef(sources[1])
functions <- sourcef(sources[2])
data <- loadf(sources[3])

## targets
outfile <- targets[1]

## TODO: note that set.seed doesn't work with multicore - each process
## sets its own seed
set.seed(1727)

iterations <- 1000
nsamples <- 1000

cmp <- data$cmp
analytes <- data$analytes

htab <- resample(cmp, analytes, iterations, nsamples, hosp='H', nproc=nproc)
utab <- resample(cmp, analytes, iterations, nsamples, hosp='U', nproc=nproc)

aucTab <- rbind(htab, utab)
save(aucTab, file=outfile)
