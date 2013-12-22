library(ROCR)

getSamples <- function(x, limit=rep(TRUE, nrow(x)),
                       iterations=NA,
                       sameLocation=FALSE,
                       verbose=FALSE){

  cat(gettextf('getSamples.R: performing simulation with %s samples\n', iterations))

  ## print(iterations)
  ## Perform selection of paired properly- and mis-labeled
  ## specimens. Returns a data.frame with columns
  ## * $i - index of initial sample
  ## * $j - index of properly labeled sample
  ## * $k - index of mislabeled sample

  ## * x - the input data TODO: decsribe
  ## * iterations -
  ## * limit - boolean vector corresponding to rows of x
  ##   limiting rows for simulation to those with TRUE values

  ## > head(cmp)
  ##   i   PatNum AccNum Location LocType hospital            dateTime  Na   K  Cl
  ## 1 1 U2203053 W17730    U.PSC      OP        U 2009-07-01 11:20:00 136 3.6 102
  ## 2 2 N1836100 T13439   U.NKDM      OP        U 2009-06-09 10:05:00 141 4.3 107
  ## 3 3 U2207457 F69167    U.PSC      OP        U 2009-04-24 12:04:00 144 5.0 109
  ## 4 4 U2207469 H55146  U.NESUR      OP        U 2009-02-19 13:40:00 141 3.7 103
  ## 5 5 U2207469 W40619  U.NESUR      OP        U 2009-04-29 15:15:00 142 4.1 105
  ## 6 6 U2207469 H70117  U.NEURO      OP        U 2009-06-11 12:11:00 140 3.7 105
  ##   CO2 IGAP GLU BUN CRE  TP ALB BIL  Ca AST ALK ALT
  ## 1  28    6 107  15 0.7 6.9 3.7 0.6 9.3  19 106  17
  ## 2  27    7  82   8 0.6 7.6 3.8 0.4 9.6  18  80  15
  ## 3  25   10 106  61 1.7 7.8 3.5 0.6 9.5  37 147  37
  ## 4  31    7  77   9 0.8 7.3 3.9 0.6 9.4  18 107  20
  ## 5  30    7  84   9 0.8 7.7 4.1 0.4 9.5  20 125  27
  ## 6  28    7 101   7 0.9 8.5 4.5 0.7 9.9  18 115  16

  ## note that we need to make sure that x$i corresponds to rows in x

  ## groups of indices in a list keyed by PatNum. limit is a logical
  ## vector corresponding to rows to include in the analysis
  mrnSplit <- with(subset(x, limit), split(i,factor(PatNum)))
  ## groups of indices in a list keyed by Location
  locSplit <- with(subset(x, limit), split(i,factor(Location)))

  chooseFrom <- which(limit)
  samples <- lapply(sample(chooseFrom, iterations, replace = TRUE),
                    function(i){
                      pick(x=x, i=i,
                           mspl=mrnSplit,
                           lspl=locSplit,
                           chooseFrom=chooseFrom,
                           verbose=verbose)
                    })

  ## samples is a data.frame based on return value of pick
  samples <- do.call(rbind, samples)

  ## print(samples)
  ## print(cbind(x$PatNum[samples$i],
  ## x$PatNum[samples$j],
  ## x$PatNum[samples$k]))

  ## confirm that comparisons labeled as within or between patients are actually so
  stopifnot(with(subset(samples, !is.na(j)), all(x$PatNum[i] == x$PatNum[j])))
  stopifnot(with(subset(samples, !is.na(j)), all(x$PatNum[i] != x$PatNum[k])))
  ## confirm that time calculation is being done properly

  ## confirm that samples at i and k are within the same location only
  ## if same=TRUE
  if(sameLocation){
    stopifnot(all(x$Location[samples$i] == x$Location[samples$k]))
  }

  ## we should only have selected indices corresponding to rows
  ## identified by 'limit'
  stopifnot(all(as.matrix(samples) %in% c(which(limit), NA)))
  
  return(samples)
}

pick <- function(x, i, mspl, lspl, chooseFrom, sameLocation=FALSE, dt=72, verbose=FALSE){

  ## * x - data.frame containing all specimens
  ## * i - row index of a specimen
  ## * mspl - list of row indices named by PatNum
  ## * lspl - list of row indices named by Location
  ## * chooseFrom - candidate values for j and k
  ## * sameLocation - if TRUE, k is drawn from a patient in the same
  ##   location as sample i
  ## * dt - maximum time difference in hours

  ## return a numeric vector with named elements
  ## * i - value of argument i
  ## * j - row in x containing sample from same patient
  ## * k - row in x containing sample from different patient

  if(verbose){
    cat(gettextf('i=%s\n',i))
  }

  ## start with a sample at index i
  SiPx <- x[i,]
  mrn <- SiPx$PatNum
  loc <- SiPx$Location

  ## get other samples with same PatNum drawn earlier than
  ## SiPx.  Note that these samples may or may not be from the
  ## same location as the sample at i
  Px <- x[setdiff(mspl[[mrn]],i),]

  ## limit to specimens preceding the specimen at i, but by no more
  ## than dt hours

  ## a subquery on Px is faster then doing second query on full
  ## dataset
  Px <- subset(
               Px,
               dateTime < SiPx$dateTime & difftime(SiPx$dateTime, dateTime, units='hours') <= dt
               )

  if(nrow(Px) > 0){
    j <- Px$i[which.max(Px$dateTime)]
  }else{
    ## no earlier samples for this patient
    if(verbose){cat('no earlier samples for this patient\n')}
    j <- NA
  }

  ## now pick sample from another patient in the *same* location or
  ## with no restriction on location based upon value of
  ## "sameLocation" passed to getSamples
  if(sameLocation){
    kk <- setdiff(lspl[[loc]], mspl[[mrn]])
  }else{
    kk <- setdiff(chooseFrom, mspl[[mrn]])
  }

  if(length(kk) == 0){
    if(verbose) cat('no other patients to choose from\n')
    k <- NA
  }else if(length(kk) == 1){
    k <- kk ## unexpected results for sample(numeric-length-one)
  }else{
    k <- sample(kk,1)
  }

  ## locations <- x$Location
  data.frame(i=i,j=j,k=k)
}

assemble <- function(x, samples, fields, explabels=c('s','d')){
  
  s1=rep(samples$i,2)
  s2=c(samples$j,samples$k)

  labels <- with(x, 
                 data.frame(
                            s1=s1,
                            s2=s2,
                            loc=Location[s1],
                            dt=dateTime[s1]-dateTime[s2],
                            mislabel=factor(
                                rep(explabels,each=nrow(samples)),
                                ordered=TRUE,levels=explabels
                                )
                            )
                 )  
  deltas <- abs(x[s1,fields] - x[s2,fields])
  rownames(deltas) <- NULL  
  percents <- 100*deltas/x[s1,fields]
  
  list(
       samples=samples,
       labels=labels,
       deltas=deltas,
       transposedDeltas=transposeExperiment(labels, deltas),
       percents=percents,
       transposedPercents=transposeExperiment(labels, percents)
       )

}

transposeExperiment <- function(labels, results){
  ## Transform experiment such that a single column named "delta"
  ## contains the contents of columns in results; columns in labels
  ## will be repeated accordingly. Useful for preparing data for
  ## visualization using plotting tools in lattice package.
  
  deltas <- as.matrix(results)
  ## converts to a vector, taken column-wise
  dim(deltas) <- NULL
  
  edf <- data.frame(
                    test=rep(colnames(results), each=nrow(results)),
                    delta=deltas             
                    )
  
  ot <- lapply(
               colnames(labels),
               function(col){rep(labels[[col]],ncol(results))}
               )
  names(ot) <- colnames(labels)
  
  return(cbind(ot,edf))
}

optCutoff <- function(vals, truth, test){
  ## calculate the optimal cutoff for the given test
  
  if(sum(!is.na(vals)) > 2){ 
    pred <- ROCR::prediction(vals, truth)
    perf <- ROCR::performance(pred,test)    
    unlist(perf@x.values)[which.max(unlist(perf@y.values))]
  }else{
    NA
  }
}

sensCutoff <- function(vals, truth, sens){
  ## calculate the cutoff associated with a sensitivity as close as
  ## possible to sens

  pred <- ROCR::prediction(vals, truth)
  tp <- unlist(pred@tp)
  fn <- unlist(pred@fn)
  ## minimize the difference between calculated sensitivity and sens
  cutoff <- unlist(pred@cutoffs)[which.min(abs((tp/(tp+fn))-sens))]
  ifelse(length(cutoff) > 0, cutoff, NA)
}

getROC <- function(vals, truth, cutoff=NA){
  ## Describe test performance given vectors vals and truth at cutoff
  ## quick reference: http://en.wikipedia.org/wiki/Sensitivity_and_specificity

  if(sum(!is.na(vals)) < 2){
    return(list(auc=NA,sens=NA,spec=NA))
  }
  
  pred <- ROCR::prediction(vals, truth)

  ## TODO: remove calculation of nSamples if not needed in
  ## expt1_performance.deltaPerformance
  
  x <- list(
            nSamples=length(vals),
            pred=pred,
            roc=ROCR::performance(pred, "tpr", "fpr"),
            auc=unlist(ROCR::performance(pred,'auc')@y.values),
            sens=NA,
            spec=NA,
            deltaCheck=NA,
            tab=NA,
            cells=NA            
            )

  if(!is.na(cutoff)){
    x$sens <- valAtCutoff(performance(pred,'sens'), cutoff)
    x$spec <- valAtCutoff(performance(pred,'spec'), cutoff)

    x$deltaCheck <- ordered(ifelse(vals < cutoff, 'pass','fail'),
                          levels=c('pass','fail'))

    x$cells <- x$tab <- table(x$deltaCheck, truth)[2:1,2:1] ## puts TP in upper left
    dim(x$cells) <- NULL
    names(x$cells) <- c('TP','FN','FP','TN')
  }
  return(x)
}

totalpositives <- function(prev, sens, spec, per=1000){
  ## Calculate total positives observed per 'per' specimens, using an
  ## arbitrarily large starting population (n)

  n <- 100000
  
  TPFN <- n * prev
  TP <- TPFN * sens
  FN <- TPFN - TP
  FPTN <- n - TPFN
  TN <- FPTN * spec
  FP <- FPTN - TN
  TTP <- (FP + TP)/per
  TTP
 
}

deltaPerformance <- function(cutoff, pred, prevalence, correct){
  ## Calculate ppv, npv, TTP 
  
  if(is.na(cutoff)){
    return(data.frame(cutoff=NA, ppv=NA, npv=NA, PPT=NA, sens=NA, spec=NA))
  }
  
  ## N <- nSamples
  P <- prevalence
  
  sens <- valAtCutoff(ROCR::performance(pred,'sens'), cutoff)
  spec <- valAtCutoff(ROCR::performance(pred,'spec'), cutoff)
  
  ppv <- (P * sens)/((sens * P) + (1-spec)*(1-P)) * 100
  npv <- ((1-P) * spec)/((spec * (1-P)) + (1-sens)*(P)) * 100
  TTP <- totalpositives(prevalence, sens, spec)
  
  ## Multiply TTP by correction factor reflecting the fraction of
  ## results that actually experience a delta check
  PPT <- TTP * correct
  
 # data.frame(cutoff=cutoff, ppv=ppv, npv=npv, PPT=PPT)
  data.frame(cutoff=cutoff, ppv=ppv, npv=npv, PPT=PPT, sens=sens, spec=spec)

}


valAtCutoff <- function(perfObj, cutoff){
  ind <- which.min(abs(unlist(perfObj@x.values) - cutoff))
  unlist(perfObj@y.values)[ind]
}

assemblePerf <- function(roc, cutoffs, prevalence, correction){
  ## returns a list of dataframes each of which contains performance
  ## measures for all analytes for a given cutoff
  outer <- lapply(colnames(cutoffs),
                         function(cutcol){
                           ## inner loop returns a data.frame with
                           ## cols cutoff, ppv, npv, TTP for each
                           ## analyte
                           inner <- lapply(names(roc), ## analytes
                                           function(analyte){
                                             cutoff <- cutoffs[analyte, cutcol]
                                             pred <- roc[[analyte]]$pred
                                             correct <- correction[[analyte]]
                                             deltaPerformance(cutoff, pred, prevalence, correct)
                                           })
                           names(inner) <- names(roc)
                           do.call(rbind, inner)
                         }
                  )
  names(outer) <- colnames(cutoffs)
  return(outer)
}
