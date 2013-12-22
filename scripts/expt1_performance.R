## Invoke from SConstruct using
## output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])
source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

## sources
functions <- sourcef(sources[1])
cutoffs <- loadf(sources[2])[[1]]
roc <- loadf(sources[3])[[1]]
samples <- loadf(sources[4])[[1]]
deltas <- loadf(sources[5])

## targets
outfile <- targets[1]

## determine the number of deltas that are not NA
correction <- apply(deltas$cmp[,deltas$analytes], 2, function(col){
  sum(!is.na(col[samples$j])) / length(samples$j)
})

## performance over a range of prevalences
performance <- lapply(c(0.002, 0.001, 0.0002), function(prevalence){
  functions$assemblePerf(roc, cutoffs, prevalence, correction)
})

save(performance, file=outfile)
