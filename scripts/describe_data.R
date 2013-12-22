library(ROCR)

## Invoke from SConstruct using
## output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])
source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

data <- loadf(sources[1])
constants <- sourcef(sources[2])
outfile <- targets[1]

cmp <- data$cmp
analytes <- data$analytes

desc <- list()

cat('number of rows in cmp\n')
desc$nrows <- nrow(cmp)
print(desc$nrows)

cat('number of unique patients\n')
desc$uniquePatNum <- length(unique(cmp$PatNum))
print(desc$uniquePatNum)

cat('number of unique accession numbers\n')
desc$uniqueAccNum <- length(unique(cmp$AccNum))
print(desc$uniqueAccNum)

cat('approximate number of tests per day for each analyte\n')
## assumes 6 months of data
ndays <- 365/2
desc$perday <- list()
desc$perday$total <- round(sapply(analytes,
                                  function(a) sum(!is.na(cmp[[a]])))/ndays)
desc$perday$byHospital <- round(sapply(analytes,
                                       function(a) table(is.na(cmp[[a]]), cmp$hospital)[1,])/ndays)
desc$perday$byLocType <- round(sapply(analytes,
                                function(a) table(is.na(cmp[[a]]), cmp$LocType)[1,])/ndays)

print(desc$perday)
## description matching the simulated data
simdat <- subset(cmp, hospital %in% 'U' & LocType %in% 'IP')
desc$simdata$tally <- nrow(simdat)

desc$simdata$simUniquePatNum <- with(simdat, length(unique(PatNum))) 

cat('proposed simulation data total samples\n')
print(desc$simdata$tally)

cat('proposed simulation data unique patient numbers\n')
print(desc$simdata$simUniquePatNum)

save(desc, file=outfile)
