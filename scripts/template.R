## Template for R scripts. Invoke from SConstruct using
##   output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])

source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

sourcefile1 <- sources[1]
sourcefile2 <- sources[2]

outfile <- targets[1]
