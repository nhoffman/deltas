## Invoke from SConstruct using
## output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])
source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

functions <- sourcef(sources[1])
datafile <- sources[2]
outfile <- targets[1]

load(datafile)
set.seed(1727)

hosp <- 'H'
samples <- functions$getSamples(cmp,
                      limit=cmp$hospital %in% hosp & cmp$LocType %in% 'IP',
                      iterations = 2500,
                      verbose = FALSE
                      )

## HMC only?
hospitals <- with(samples, setdiff(unique(cmp$hospital[c(i,j,k)]),NA))
stopifnot(identical(hospitals, hosp))

## make sure we have only inpatient locations
allLocations <- with(samples, setdiff(unique(cmp$LocType[c(i,j,k)]),NA))
print(allLocations)
stopifnot(identical(allLocations, 'IP'))

save(samples, file=outfile)
