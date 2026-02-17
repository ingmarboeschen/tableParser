#' matrix2text
#'
#' Converts character matrix content to a screen reader-like readable character string. The parsing is performed row-wise in standard mode. 
#' @param x A character matrix or list of character matrices.
#' @param legend A list with table legend codes extracted from table caption and/or footnote with legendCodings(). 
#' @param unifyMatrix Logical. If TRUE, matrix cells are unified for better post-processing.
#' @param correctComma Logical. If TRUE and 'unifyMatrix=TRUE', decimal sign commas are converted to dots. 
#' @param na.rm Logical. If TRUE, NA cells are set to empty cells.
#' @param forceClass character. Set matrix-specific handling to one of c("tabled result", "correlation", "matrix", "text").
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label detected in table caption/footnote with legendCodings().
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param dfHandling Logical. If TRUE, detected sample size N in the caption/footnote is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param decodeP Logical. If TRUE, imputes the converts the detected p-value codings to text with seperator ';;' (e.g., '1.23*' -> '1.23;; p<.01')
#' @param standardPcoding Logical. If TRUE, and no other detection of p-value coding is detected, standard coding of p-values is assumed to be: * p<.05, ** p<.01, and *** p<.001.
#' @param noSign2p Logical. If TRUE, imputes 'p>maximum of coded p-values' to cells that are not coded to be significant.
#' @param bracketHandling Logical. If TRUE and if possible, decodes numbers in brackets.
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param unlist Logical. If TRUE, output is returned as a vector with parsed text from all listed matrices; else, a list with parsed text from each matrix is returned as a list. 
#' @param addTableName Logical. If TRUE and unlist=TRUE, the table number is added in front of unlisted text lines.
#' @param split Logical. If TRUE, multi-model tables are split before being processed.
#' @return Character vector with a parsed and human-readable form of the input table. The result vector can be further processed with standardStats() to extract and structure the statistical standard test results only.
#' @examples 
#' # some random data
#' x<-rnorm(100)
#' y<-x+rnorm(100)
#' 
#' # a model result table...
#' mod<-round(summary(lm(y~x))$coefficients,3)
#' rnames<-c("",rownames(mod))
#' cnames<-colnames(mod)
#' mod<-rbind(cnames,mod)
#' mod<-cbind(rnames,mod)
#' 
#' # ...as character result matrix
#' x<-unname(mod)
#' x
#' 
#' ## parse matrix to text vector
#' # - as is
#' matrix2text(x,unifyMatrix=FALSE)
#' # - with unified content
#' matrix2text(x,unifyMatrix=TRUE)
#' 
#' ## processing of a matrix with two header lines
#' x<-rbind(c("","A","A","B","B"),x)
#' x
#' matrix2text(x,unifyMatrix=FALSE)
#' 
#' ## processing of a matrix with two header lines and grouping column [,1]
#' x<-cbind(c("","","C","D"),x)
#' x
#' matrix2text(x,unifyMatrix=FALSE)

#' @export

matrix2text<-function(x,legend=NULL,
                      unifyMatrix=TRUE,correctComma=FALSE,
                      na.rm=TRUE,forceClass=NULL,
                      expandAbbreviations=TRUE,
                      superscript2bracket=TRUE,
                      decodeP=FALSE,
                      standardPcoding=FALSE,
                      noSign2p=FALSE,
                      bracketHandling=FALSE,
                      dfHandling=TRUE,
                      rotate=FALSE,unlist=FALSE,addTableName=TRUE,
                      split=FALSE
){

  # escapes
  if(length(x)==0) return(NULL)
  if(length(unlist(x))==0) return(NULL)
  
  if(isFALSE(decodeP)) noSign2p<-FALSE

  # single matrix to list
  if(is.matrix(x)) x<-list(x)
  # escape
  if(!is.list(x) | !is.matrix(x[[1]])) stop("x must be a character matrix or a list of character matrices.")
  # check forceClass
  if(!is.null(forceClass))
    if(sum(is.element(c("tabled result","correlation","matrix","text"),forceClass))!=1)
      stop("Argument 'forceClass' can only be set to one of 'tabled result','correlation', 'matrix', or 'text'.")
  
  # remove html but convert sub and sup, bold and italic numbers
  x<-lapply(x,function(x) gsub("<sub>","_",gsub("<sup>","^",x)))
  
  x<-lapply(x,function(x) gsub("([0-9])</bold>","\\1^bold",gsub("([0-9])</italic>","\\1^italic",x)))
  x<-lapply(x,function(x) gsub("([0-9])</b>","\\1^bold",gsub("([0-9])</i>","\\1^italic",x)))
  
  x<-lapply(x,function(x) gsub("</*[A-z][^>]*>","",x))
  
  # escape if no characters left
  if(sum(nchar(unlist(x)))==0) return(x)
  # collapse header rows
  x<-lapply(x,headerHandling)
  
  # split at lines that are text between numeric lines
  if(isTRUE(split)){
    x<-lapply(x,multiTextRowSplit)
   x<-flatten(x)
  }
  
  if(is.matrix(x)) x<-list(x)
  
  # prepare
  x<-lapply(x,prepareMatrix,forceClass=forceClass,na.rm=na.rm,legend=legend)
  
  fun<-function(x,rotate=FALSE,unifyMatrix=TRUE,correctComma=FALSE,na.rm=TRUE){
  # escapes
  if(length(x)==0) return(NULL)
  # set NA to ""
  x[is.na(x)]<-""
  # unify matrix content
  if(isTRUE(unifyMatrix)) x<-unifyMatrixContent(x,correctComma=correctComma,na.rm=na.rm)
  
  if(is.matrix(x)){
  # insert col and row names to inner matrix
  if(length(colnames(x))>0) if(length(colnames(x))==ncol(x)){
    x<-rbind(colnames(x),x)
    colnames(x)<-NULL}
  if(length(rownames(x))>0) if(length(rownames(x))==nrow(x)){
    x<-cbind(rownames(x),x)
    rownames(x)<-NULL}}
   
  # output
  return(x)
  }
  
  x<-lapply(x,fun,rotate=rotate,unifyMatrix=unifyMatrix,correctComma=correctComma,
            na.rm=na.rm)
  
  # split matrices
  if(isTRUE(split)){
  x<-lapply(x,splitter)
  # flatten list of lists with matrices to simple list
  x<-flatten(x)
  }

  # escape if no characters left
  if(sum(nchar(unlist(x)))==0) return(x)
  
  # rotate
  if(isTRUE(rotate)) x<-lapply(x,t)
  
  out<-NULL
  
  if(is.list(x)) out<-lapply(x,parseMatrixContent,legend=legend,forceClass=forceClass,
                                    standardPcoding=standardPcoding,decodeP=decodeP,noSign2p=noSign2p,
                                    expandAbbreviations=expandAbbreviations,bracketHandling=bracketHandling,
                             superscript2bracket=superscript2bracket,dfHandling=dfHandling)

  # unify output for better readability of get.stats
  if(isTRUE(unifyMatrix)) out<-lapply(out,unifyOutput)
  
  if(length(out)==0) return(NULL)
  
  names(out)<-names(x)
#  names(out)[1]<-"Table 1"

  if(isTRUE(unlist)){
    n<-rep(names(out),times=unlist(lapply(out,length)))
    if(isTRUE(addTableName)) out<-paste0(n,":: ",unname(unlist(out)))
    if(isFALSE(addTableName)) out<-unname(unlist(out))
  }
    return(out) 

  } # end function matrix2text()


