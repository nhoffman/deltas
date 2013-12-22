## Invoke from SConstruct using
## output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])
source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

## sources
constants <- sourcef(sources[1])
functions <- sourcef(sources[2])
experiment <- loadf(sources[3])[[1]]

## targets
outfile <- targets[1]

deltas <- experiment$deltas
truth <- experiment$labels$mislabel

## restrict analytes here - could define in constants.R
analytes <- colnames(deltas)

deltaROC <- lapply(analytes,
                   function(a){
                     functions$getROC(deltas[[a]], truth, constants$deltaCutoffs[a])
                   })
names(deltaROC) <- analytes

save(deltaROC, file=outfile)
