library(xtable)

asLatex <- function(x){
  tab <- do.call(cbind, x)
  colnames(tab) <- rep(c('cutoff','PPV','NPV','PPT'), 4)
  print(
      xtable(tab, digits = c(0,rep(c(1,2,1,2),4))),
      ## xtable(tab),
      only.contents=TRUE,
      file = '/dev/null')
}

source('scripts/common.R')
args <- getargs(argv=commandArgs(trailingOnly=TRUE), ntargets=1)
sources <- args$sources
targets <- args$targets

## sources
perfData <- loadf(sources[1])[[1]]

## target
texfile <- targets[1]

## remember to escape backslashes '/hline' --> '//hline'
##             ... and percent signs '20%' --> '20%%'

page <- '
\\begin{tabular}{c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c}
\\multicolumn{17}{c}{Mislabel rate of 1 in 500}\\\\
\\hline
\\multicolumn{1}{c|}{} & \\multicolumn{4}{|c|}{Current} & \\multicolumn{4}{|c|}{20\\%% Sensitivity} & \\multicolumn{4}{|c|}{80\\%% Sensitivity} & \\multicolumn{4}{|c}{PRBE} \\\\
%s
\\end{tabular}

\\vspace{1em}

\\begin{tabular}{c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c}
\\multicolumn{17}{c}{Mislabel rate of 1 in 1000}\\\\
\\hline
\\multicolumn{1}{c|}{} & \\multicolumn{4}{|c|}{Current} & \\multicolumn{4}{|c|}{20\\%% Sensitivity} & \\multicolumn{4}{|c|}{80\\%% Sensitivity} & \\multicolumn{4}{|c}{PRBE} \\\\
%s
\\end{tabular}

\\vspace{1em}

\\begin{tabular}{c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c}
\\multicolumn{17}{c}{Mislabel rate of 1 in 5000}\\\\
\\hline
\\multicolumn{1}{c|}{} & \\multicolumn{4}{|c|}{Current} & \\multicolumn{4}{|c|}{20\\%% Sensitivity} & \\multicolumn{4}{|c|}{80\\%% Sensitivity} & \\multicolumn{4}{|c}{PRBE} \\\\
%s
\\end{tabular}
'

## trim the columns to cutoff, ppv, npv and PPT
cols <- c('cutoff', 'ppv', 'npv', 'PPT')

perfData <- with(perfData, lapply(perfData, function(x){
  x$current <- x$current[,cols]
  x$sens20 <- x$sens20[,cols]
  x$sens80 <- x$sens80[,cols]
  x$prbe <- x$prbe[,cols]
  x
}))


cat(
    gettextf(page,
           asLatex(perfData[[1]]), asLatex(perfData[[2]]), asLatex(perfData[[3]])
           ),
    file = texfile
    )
