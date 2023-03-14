' @title Convert .Rmd file to a ready to run .R script
#'  
#' @description Converts markdown text to comments (following roxygen protocol) and optionally
#' comments out code chunks that produce plots.
#' 
#' @details This function is inspired by code from Kevin Keena 
#' (http://rstudio-pubs-static.s3.amazonaws.com/12734_0a38887f19a34d92b7311a2c9cb15022.html).
#' However, \code{rmd2r} is a complete re-write and adds the functionallity to specify a file 
#' in which to write the outpu and also the option to comment out plotting chunks (useful, 
#' e.g., if you'd like to run the script in the background)
#' 
#' @param x the Rmd file
#' @param file the file to write to, defaults to same name as \code{x} but appended with '.R'
#' @param noPlots logical, should plotting chunks be commented out
#' @param rmTags logical, should code chunk tags be removed
#' @param stripMD logical, should markdown text be removed
#' 
# @examples
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
# @seealso
#' @export

rmd2r <- function(x, file, noPlots = TRUE, rmTags = TRUE, stripMD = FALSE) {
  ## deal with possible file input
  if(missing(file)) file <- gsub('\\.rmd', '.R', x, ignore.case = TRUE)
  if(!grepl('\\.R', file, ignore.case = TRUE)) file <- paste0(file, '.R')
  
  ## load the Rmd file
  x <- readLines(x)
  
  ## find R code chunk starts
  rStrt <- grep('```\\{r', x)
  
  ## find all '```' tags
  codeTag <- grep('```', x)
  
  ## match start and end
  actuallyStrt <- which(codeTag %in% rStrt)
  rEnd <- codeTag[actuallyStrt + 1]
  
  ## add roxygen tag to everything
  x <- paste0("#' ", x)
  
  ## remove from R code chunks
  for(i in 1:length(rStrt)) {
    x[(rStrt[i] + 1):(rEnd[i] - 1)] <- gsub("#' ", '', x[(rStrt[i] + 1):(rEnd[i] - 1)])
    
    if(rmTags) x[c(rStrt[i], rEnd[i])] <- ''
  }
  
  if(noPlots) {
    ## find plotting function indeces
    plotInd <- .findPlots(x)
    
    ## find which chunks to comment out
    comThese <- sapply(1:length(rStrt), function(i) any(rStrt[i]:rEnd[i] %in% plotInd))
    pStrt <- rStrt[comThese]
    pEnd <- rEnd[comThese]
    
    ## comment them out
    for(i in 1:length(pStrt)) {
      x[(pStrt[i] + 1):(pEnd[i] - 1)] <- paste0("# ", x[(pStrt[i] + 1):(pEnd[i] - 1)])
    }
  }
  
  ## strip MD text if desired
  if(stripMD) {
    x[rStrt] <- ''
    x <- x[!grepl("#' ", x)]
  }
  
  writeLines(x, con = file)
}


## helper function to find plotting functions
.findPlots <- function(x) {
  codeText <- getParseData(parse(text = x, keep.source=TRUE))
  funs <- codeText[codeText$token == 'SYMBOL_FUNCTION_CALL', ]
  
  ## run needed code to make things work
  src2run <- apply(funs[funs$text %in% c('setwd', 'source', 'library', 'require'), ], 1, 
                   function(src) x[src[1]:src[3]])
  eval(parse(text = paste0(src2run, collapse = '\n')))
  
  plotFuns <- plotHere <- sapply(unique(funs$text), function(f) {
    fun <- try(get(f), silent = TRUE)
    if(class(fun) == 'function') {
      return(any(grepl('plot|points', format(get(f)))))
    } else {
      return(FALSE)
    }
  })
  
  codeText$line1[codeText$text %in% names(plotFuns[plotFuns])]
}
