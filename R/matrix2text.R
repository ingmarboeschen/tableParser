#' matrix2text
#'
#' Converts character matrix content to a screen reader alike readable character string. The parsing is performed row-wise in standard mode. 
#' @param x A character matrix or list of character matrices.
#' @param legend A list with table legend codes extracted from table caption and/or footer with tableParser::legendCodings(). 
#' @param unifyMatrix Logical. If TRUE, matrix cells are unified for better post processing.
#' @param correctComma Logical. If TRUE and unifyMatrix=TRUE, decimal sign commas are converted to dots. 
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label detected in table caption/footer with tableParser::legendCodings().
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param addDF Logical. If TRUE, detected sample size N in caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param standardPcoding Logical. If TRUE, and no other detection of p-value coding is detected, standard coding of p-values is assumed to be: * p<.05, ** p<.01 and *** p<.001.
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param unlist Logical. If TRUE, output is returned as vector with parsed text fromm all listed matrices, else a list with parsed text from each matrix is returned as list. 
#' @param addTableName Logical. If TRUE and unlist=TRUE, table number is added in front of unlisted text lines.
#' @param split Logical. If TRUE, matrix/matrices are split for multi-model tables.
#' @return Character vector with a parsed and human readable form of the input table. The result vector can be further processed with standardStats() to extract and structure the statistical standard test results only.
#' @examples 
#' # some random data
#' x<-rnorm(100)
#' y<-x+rnorm(100)
#' # a model result table...
#' mod<-round(summary(lm(y~x))$coefficients,3)
#' rnames<-c("",rownames(mod))
#' cnames<-colnames(mod)
#' mod<-rbind(cnames,mod)
#' mod<-cbind(rnames,mod)
#' # ...as character result matrix
#' x<-unname(mod);x
#' ## parse matrix to text vector
#' # -as is
#' matrix2text(x,unifyMatrix=FALSE)
#' # -with unified content
#' matrix2text(x,unifyMatrix=TRUE)
#' ## processing of a matrix with two header lines
#' x<-rbind(c("","A","A","B","B"),x);x
#' matrix2text(x,unifyMatrix=FALSE)
#' ## processing of a matrix with two header lines and naming columns 
#' x<-cbind(c("","","C","D"),x);x
#' matrix2text(x,unifyMatrix=FALSE)

#' @export

matrix2text<-function(x,
                      legend=NULL,
                      unifyMatrix=TRUE,correctComma=FALSE,
                      expandAbbreviations=TRUE,
                      superscript2bracket=TRUE,
                      standardPcoding=FALSE,
                      addDF=TRUE,
                      rotate=FALSE,unlist=FALSE,addTableName=TRUE,
                      split=FALSE
){

  # escapes
  if(length(x)==0) return(NULL)
  if(length(unlist(x))==0) return(NULL)

  # single matrix to list
  if(is.matrix(x)) x<-list(x)

  # escape
  if(!is.list(x) | !is.matrix(x[[1]])) stop("x must be a character matrix or a list of character matrices.")
  
  # remove html but convert sub and sup, bold and italic numbers
  x<-lapply(x,function(x) gsub("<sub>","_",gsub("<sup>","^",x)))
  
  x<-lapply(x,function(x) gsub("([0-9])</bold>","\\1^bold",gsub("[0-9]</italic>","\\1^italic",x)))
  x<-lapply(x,function(x) gsub("([0-9])</b>","\\1^bold",gsub("[0-9]</i>","\\1^italic",x)))
  
  x<-lapply(x,function(x) gsub("</*[A-z][^>]*>","",x))
  
  
  # collapse header rows
  x<-lapply(x,headerHandling)
  
  
  # split at lines that are text between numeric lines
  x<-lapply(x,multiTextRowSplit)
  x<-flatten(x)
  
  if(is.matrix(x)) x<-list(x)
  
  x<-lapply(x,prepareMatrix)
  
  fun<-function(x,rotate=FALSE,unifyMatrix=TRUE,correctComma=FALSE){
  # escapes
  if(length(x)==0) return(NULL)
  # set NA to ""
  x[is.na(x)]<-""
  # unify matrix content
  if(unifyMatrix==TRUE) x<-unifyMatrixContent(x,correctComma=correctComma)
  
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
  
  x<-lapply(x,fun,rotate=rotate,unifyMatrix=unifyMatrix,correctComma=correctComma)
  
  # split matrices
  if(split==TRUE){
  x<-lapply(x,splitter)
  # flatten list of lists with matrices to simple list
  x<-flatten(x)
  }

  
  # rotate
  if(rotate==TRUE) x<-lapply(x,t)
  
  out<-NULL
  
  if(is.list(x)) out<-lapply(x,parseMatrixContent,legend=legend,
                                    standardPcoding=standardPcoding,
                                    expandAbbreviations=expandAbbreviations,
                             superscript2bracket=superscript2bracket,addDF=addDF)
  #if(is.matrix(x)|is.vector(x)) out<-unlist(parseMatrixContent(x,legend=legend,
  #                                                             standardPcoding=standardPcoding,
  #                                                             expandAbbreviations=expandAbbreviations))

  # unify output for better readability of get.stats
  if(unifyMatrix==TRUE) out<-lapply(out,unifyOutput)
  
  if(length(out)==0) return(NULL)
  names(out)<-names(x)
  names(out)[1]<-"Table 1"
  #names(out)<-paste("Pable",1:length(out))
  if(isTRUE(unlist)){
    n<-rep(names(out),times=unlist(lapply(out,length)))
    if(isTRUE(addTableName)) out<-paste0(n,":: ",unname(unlist(out)))
    if(!isTRUE(addTableName)) out<-unname(unlist(out))
  }
    return(out) 

  } # end function matrix2text()


