getPairedSamples <- function(x, iterations, minSamplesPerPatNum,
                             limit=rep(TRUE, nrow(x)), same=FALSE){

  ## Perform selection of paired properly- and mis-labeled
  ## specimens. Returns a data.frame with columns
  ## * $i - index of initial sample
  ## * $j - index of properly labeled sample
  ## * $k - index of mislabeled sample

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
  
  ## number of specimens associated with each PatNum
  samplesPerPatNum <- sapply(mrnSplit, length)
  
  ## limit selection of initial sample to patients with at least 
  ## minSamples samples
  chooseFrom <- unlist(mrnSplit[samplesPerPatNum >= minSamplesPerPatNum],
                       use.names=FALSE)

  ## initialize matrix to contain results (more memory-efficient than
  ## appending to an object); be sure to maintain correspondence of
  ## column names with output of pick!
  samples <- matrix(nrow=iterations, ncol=3)

  tries <- 0
  iter <- 1
  while(iter <= iterations){
    tries <- tries + 1
    i <- sample(chooseFrom,1)
    picked <- pick(x=x, i=i,
                   mspl=mrnSplit,
                   lspl=locSplit,
                   chooseFrom=chooseFrom,
                   same=same, verbose=FALSE)
    if(!is.null(picked)){
      samples[iter,] <- picked
      iter <- iter + 1
    }
  }

  cat(gettextf('Calculated %s deltas in %s tries\n', iterations, tries))
  
  colnames(samples) <- c('i','j','k')
  samples <- as.data.frame(samples)

  ## print(samples)
  ## print(cbind(x$PatNum[samples$i],
  ## x$PatNum[samples$j],
  ## x$PatNum[samples$k]))
  

  
  ## confirm that comparisons labeled as within or between patients are actually so
  stopifnot(all(x$PatNum[samples$i] == x$PatNum[samples$j]))
  stopifnot(all(x$PatNum[samples$i] != x$PatNum[samples$k]))

  ## confirm that time calculation is being done properly
  
  ## confirm that samples at i and k are within the same location only
  ## if same=TRUE, else make sure they are not from the same location
  if(same){
    stopifnot(all(x$Location[samples$i] == x$Location[samples$k]))
  }
  
  return(samples)
}

pick <- function(x, i, mspl, lspl, chooseFrom, same, dt=72, verbose=FALSE){

  ## x - data.frame containing all specimens
  ## i - row index of a specimen
  ## mspl - list of row indices named by PatNum
  ## lspl - list of row indices named by Location
  ## dt - maximum time difference in hours 

  ## return a numeric vector with named elements
  ## * i - same as argument i
  ## * j - row in x containing sample from same patient
  ## * k - row in x containing sample from same patient in same location
  

  ## start with a sample at index i
  SiPx <- x[i,]
  mrn <- SiPx$PatNum
  loc <- SiPx$Location

  ## get other samples with same PatNum drawn earlier than SiPx.  Note
  ## that these samples may or may not be from the same location as
  ## the sample at i
  Px <- x[setdiff(mspl[[mrn]],i),]

  ## limit to specimens preceding the specimen at i, but
  ## by no more than dt hours
  ## I'm assuming that a subquery on Px will be faster then 
  ## doing second query on full dataset
  Px <- subset(
               Px,
               dateTime < SiPx$dateTime & difftime(SiPx$dateTime, dateTime, units='hours') <= dt
               )

  if(nrow(Px) == 0){
    if(verbose) cat('no earlier samples for this patient\n')
    return(NULL)
  } ## no earlier samples for this patient

  ## print(with(Px, dateTime - SiPx$dateTime))
  
  j <- Px$i[which.max(Px$dateTime)]
      
  ## now pick sample from another patient in the *same* location or
  ## with no restriction on location based upon value of "same" passed
  ## to getSamples
  if (same){
    kk <- setdiff(lspl[[loc]], mspl[[mrn]])
  }else{
    kk <- setdiff(chooseFrom, mspl[[mrn]])
  }
  
  ## could have argument sameLocation
  ## find all indices belonging to other patients and choose

  if(length(kk) == 0){
    if(verbose) cat('no other patients in this location\n')
    return(NULL) ## no other patients in this location
  }else if(length(kk) == 1){
    k <- kk ## unexpected results for sample(numeric-length-one)
  }else{
    k <- sample(kk,1)
  }
  
  ## locations <- x$Location
  c(i=i,j=j,k=k)
}       
