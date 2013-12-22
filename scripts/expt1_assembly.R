## Invoke from SConstruct using
## output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])
source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

constants <- sourcef(sources[1])
functions <- sourcef(sources[2])
data <- loadf(sources[3])
samples <- loadf(sources[4])[[1]]

outfile <- targets[1]

experiment <- functions$assemble(x=data$cmp,
                       samples=samples,
                       fields=data$analytes,
                       explabels=constants$explabels)

save(experiment, file=outfile)
