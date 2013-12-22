loadf <- function(fname){
  ## return a named list containing objects loaded from an R data file
  env <- new.env()
  loaded <- load(fname, envir=env)
  ## cat(gettextf('loaded objects [%s] from %s\n',
  ##              paste(loaded, collapse=', '), fname))
  as.list(env)
}

sourcef <- function(fname){
  ## return named list of objects defined in fname
  local({source(fname,local=TRUE); as.list(environment())})
}

getargs <- function(argv, ntargets=1){

  ## * argv - command line arguments - probably commandArgs(trailingOnly=TRUE)
  ## * ntargets - specify how many of the first N=ntargets elements in
  ##   args that should be included in the "targets" element
  
  targets <- argv[1:ntargets]
  cat('targets:\n'); print(targets)

  ## sources
  if(length(argv) > length(targets)){
    sources <- argv[(length(targets)+1):length(argv)]
  }else{
    sources <- NA
  }
  cat('sources:\n')
  if(length(sources) < 10){
    print(sources)
  }else{
    cat(sources[1:10])
    cat(gettextf('...plus %s more\n', length(sources)-10))
  }

  return(list(sources=sources, targets=targets))
}

template <- function(fstr, argv, default=''){
  rstr <- '(?<=%)\\(\\w+\\)'    

  ## support for vectorized operation
  if(length(fstr) > 1){
    return(sapply(fstr, function(x){
      template(x, argv, default)}, USE.NAMES=FALSE))
  }
  
  if(is.data.frame(argv)){
    return(apply(argv, MARGIN=1, function(x){
      template(fstr, as.list(x), default)}))
  }
  
  starts <- gregexpr(rstr, fstr, perl=TRUE)[[1]]    

  if (starts[1] != -1){        
    words <- substring(fstr, starts+1, starts+attr(starts,'match.length')-2)    

    ## restrict to keys in fstr; ensure proper order; also replace
    ## zero-length elements in repl (eg, NULL, character(0))    
    repl <- ifelse(sapply(argv[words],length) > 0, argv[words], default)
    
    do.call(
            gettextf,
            c(list(fmt=gsub(rstr, '', fstr, perl=TRUE)), repl)
            )
  }
  else{
    ## no replacement directives were found
    fstr
  }
}

matcharg <- function(args, pattern){
  ## return the argument matching 'pattern' 
  matches <- grep(pattern, args)
  if(length(matches) == 0){stop(gettextf('no arguments matching %s', pattern))}
  if(length(matches) > 1){stop(gettextf('more then one argument matching %s', pattern))}
  args[matches]
}
