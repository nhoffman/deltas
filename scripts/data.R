## provides cmp, analytes, loc

## filter data by location
## location data from Fred
## note that locations can be obtained using LIS function 
## MRP, option 12

readData <- function(csvfile, loc){

  ## Input
  ## =====
  ##
  ## * csvfile - '|' delimited data
  ## * loc - output of filterLocations

  cmp <- read.table(file=csvfile, sep='\t', header=TRUE, fill=TRUE,
                    stringsAsFactors=FALSE,
                    ## ,nrows=10000
                    )
  
  ## adjust some column names  
  colnames(cmp)[colnames(cmp)=='NA.'] <- 'Na'
  colnames(cmp)[colnames(cmp)=='CL'] <- 'Cl'
  colnames(cmp)[colnames(cmp)=='MG'] <- 'Mg'
  colnames(cmp)[colnames(cmp)=='CA'] <- 'Ca'
  colnames(cmp)[colnames(cmp)=='PostDate_Time'] <- 'dateTime'
  colnames(cmp)[colnames(cmp)=='PatLoc'] <- 'Location'
  
  ## filter by location; add column cmp$LocType, cmp$hospital, alter cmp$Location

  ## which locations are N numbers?
  ## h <- substr(cmp$PatNum,1,1)
  ## nLocs <- names(table(cmp$Location[h == 'N']))
  ## loc[loc$code %in% nLocs,]
  ## appear to be U locations, so convert all 
  ## N's to U's
  
  ## mrn1 is the first character of each MRN
  mrn1 <- substring(cmp$PatNum,1,1)
  nlocs <- unique(cmp$Location[mrn1 == 'N'])

  ## these are the ambiguous codes: might be either H or U
  ulocs <- unique(loc[,c('code','hospital')])
  ulocs <- ulocs[order(ulocs$code),]
  ambiguous <- with(rle(ulocs$code), values[lengths > 1])

  ## N numbers not in ambiguous locations can be set to U
  mrn1[mrn1 == 'N' & !cmp$Location %in% ambiguous] <- 'U'

  ## otherwise, location is unknown - remove these rows later
  mrn1[mrn1 == 'N' & cmp$Location %in% ambiguous] <- 'X'
  
  cmp$Location <- paste(mrn1, cmp$Location, sep='.')

  ## tabulate excluded locations
  ## print(with(cmp,table(Location[!Location %in% rownames(loc)])))
  
  ## now cmp$Location should correspond to rownames of loc
  cmp$LocType <- loc[cmp$Location,'LocType']
  cmp$hospital <- loc[cmp$Location,'hospital']
  
  ## describe location types
  ## locSpl <- split(cmp$Location, cmp$LocType)
  ## for(locType in unique(cmp$LocType)){
  ##   print(locType)
  ##   locs <- unique(locSpl[[locType]])
  ##   print(loc[locs,])
  ## }
  
  ## remove rows in cmp with undefined LocType
  ## print(cmp[is.na(cmp$LocType),])
  cmp <- cmp[!is.na(cmp$LocType),]

  notAnalytes <- c('i','PatNum','AccNum','Location','LocType',
  'hospital','dateTime','Order')
  analytes <- colnames(cmp)[!colnames(cmp) %in% notAnalytes]
  
  ## remove rows that are all NA or are missing dateTime
  allNA <- apply(cmp[,analytes], 1, function(row) all(is.na(row)))
  cmp <- cmp[!allNA & !is.na(cmp$dateTime),]

  rownames(cmp) <- NULL  
  cmp$dateTime <- as.POSIXct(cmp$dateTime)
  
  ## add a column with index numbers
  cmp$i <- seq(nrow(cmp))
  
  ## reorder columns of cmp to place analytes at end
  cmp <- cmp[,c(notAnalytes,analytes)]

  list(
       cmp=cmp,
       analytes=analytes
       )
  
}


filterLocations <- function(locations){

  ## filter data by location
  ## location data from Fred
  ## note that locations can be obtained using LIS function 
  ## MRP, option 12

  ## Input
  ## =====
  ##
  ## > head(locations)
  ##   Institution    Code                Description Type
  ## 1           U  YELLOW           SCCA YELLOW TEAM  OPX
  ## 2           U  WHCRNR              U WHCC ARNP-R   OP
  ## 3           U WHCGYNR          U WHCC GYNECOLOGY   OP
  ## 4           U  WHCGUR         U WHCC GYN UROLOGY   OP
  ## 5           U WHCGIMR         U WHCC GEN INT MED   OP
  ## 6           U   WHCCR WOMEN'S HEALTH CARE CNTR-R   OP
  ##
  ## Output
  ## ======
  ##
  ## > head(loc)
  ##                code hospital type                       text LocType
  ## U.YELLOW   YELLOW        U  OPX           SCCA YELLOW TEAM      OP
  ## U.WHCRNR   WHCRNR        U   OP              U WHCC ARNP-R      OP
  ## U.WHCGYNR WHCGYNR        U   OP          U WHCC GYNECOLOGY      OP
  ## U.WHCGUR   WHCGUR        U   OP         U WHCC GYN UROLOGY      OP
  ## U.WHCGIMR WHCGIMR        U   OP         U WHCC GEN INT MED      OP
  ## U.WHCCR     WHCCR        U   OP WOMEN'S HEALTH CARE CNTR-R      OP
 
  loc <- locations[,c(2,1,4,3)]
  colnames(loc) <- c('code','hospital','type','text')

  for(i in seq(ncol(loc))){
    loc[,i] <- as.character(loc[,i])
  }

  ## NOTE: some codes appear more than once
  ## tt <- table(loc$code)
  ## dups <- as.character(loc$code[loc$code %in% names(tt[tt>1])])

  ## hospital.code should be unique
  uloc <- with(loc, paste(hospital, code, sep='.'))
  stopifnot(max(table(uloc))==1)
  rownames(loc) <- uloc

  ## determine locations to keep
  ## OPA - UWPN
  ## OPM - Mayo
  ## OPS,OPX - SCCA

  oktype <- c('ER','IP','OP','OPA','OPS','OPX')
  okloc <- rownames(loc)[loc$type %in% oktype]

  loc <- loc[okloc,]

  ## collapse OP locations into a single type
  loc$LocType <- ifelse(loc$type %in% c('OPA','OPS','OPX','ER'), 
                        'OP', loc$type)

  ## determine acute locations; note that grep returns indices
  acute <- unique(
                  c(
                    grep('ICU', loc$text),
                    grep('intensive',loc$text,ignore.case=TRUE)
                    )
                  )
  loc$LocType[acute] <- 'ACUTE'
  return(loc)
}

############################################
source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=2)
sources <- args$sources
targets <- args$targets

csvfile <- sources[1]
locationfile <- sources[2]

datafile <- targets[1]
locfile <- targets[2]

rawlocs <- loadf(locationfile)[[1]]
locations <- filterLocations(rawlocs)  

data <- readData(csvfile, locations)
cmp <- data$cmp
analytes <- data$analytes

## limit analytes to those with values in at least 25% of rows
cat('fraction of rows with a value for each analyte\n')
counts <- sapply(analytes, function(a) sum(!is.na(cmp[[a]])))/nrow(cmp)
print(counts)
for(colname in analytes[counts < 0.25]){
  cat(gettextf('removing %s\n', colname))
  cmp[[colname]] <- NULL
}
analytes <- analytes[counts >= 0.25]

save(cmp, analytes, file=datafile)
save(locations, file=locfile)
