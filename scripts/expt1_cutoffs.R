## Invoke from SConstruct using
## output = env.RScript(target=[targets], source=['scripts/scriptname.R', ...])
source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

constants <- sourcef(sources[1])
functions <- sourcef(sources[2])
experiment <- loadf(sources[3])[[1]]

sensCutoff <- functions$sensCutoff
optCutoff <- functions$optCutoff

##find indices corresponding to HMC renal unit
#renal <- grep("H.RENAL", experiment$labels$loc)

#print('The following is printed from expt1_cutoffs.R')
#print('The deltas from HMC renal units are as follows:', quote=FALSE)
#print(renal)

outfile <- targets[1]

cutoffs <- with(experiment, {

  analytes <- colnames(deltas) ## could limit this in constants
  data <- deltas[,analytes]
  ##remove deltas from renal unit
  #dataRes <- deltas[-renal,analytes]
  truth <- labels$mislabel
  #truthRes <- subset(labels, loc!="H.RENAL", mislabel)

  current <- constants$deltaCutoffs[analytes]
  names(current) <- analytes

  data.frame(
       current=current,
       sens20=apply(data, 2, function(vals) sensCutoff(vals, truth, 0.2)),
       sens50=apply(data, 2, function(vals) sensCutoff(vals, truth, 0.5)),
      sens80=apply(data, 2, function(vals) sensCutoff(vals, truth, 0.8)),
       prbe=apply(data, 2, function(vals) optCutoff(vals, truth, 'prbe'))
       #sens20Res=apply(dataRes, 2, function(vals) sensCutoff(vals, truthRes, 0.2)),
       #sens80Res=apply(dataRes, 2, function(vals) sensCutoff(vals, truthRes, 0.8)),
       #prbeRes=apply(dataRes, 2, function(vals) optCutoff(vals, truthRes, 'prbe'))
       )
})

save(cutoffs, file=outfile)
