
#' unifyMatrixContent
#' Unifies content of character matrices. E.g.: comas in big numbers and HTML tags are removed. Performs space corrections and unifies hyphens and spaces. 
#' @param x a character matrix.
#' @param letter.convert Logical. If TRUE hex codes will be unified and converted to unicode with JATSdecoder::letter.convert().
#' @param greek2text Logical. If TRUE and 'letter.convert=TRUE', converts and unifies various Greek letters to a text based form (e.g. 'alpha', 'beta'). 
#' @param text2num Logical. If TRUE, textual representations of numbers (words, exponents, fractions) are converted to digit numbers. 
#' @importFrom JATSdecoder letter.convert
#' @importFrom JATSdecoder text2num
#' @export
unifyMatrixContent<-function(x,letter.convert=TRUE,greek2text=TRUE,text2num=TRUE){
  fun<-function(x,letter.convert=TRUE,greek2text=TRUE,text2num=TRUE){
    if(!is.matrix(x)) return(x)
    # number of columns
    nCol<-ncol(x)
    
    # remove comma from big numbers except in columns with df if only one column with df
  #  hasDF<-colSums(matrix(grepl("^[dD][Ff]|[dD][Ff]$|[dD]eg[res\\.]* [ of]*[Ff]re",x),ncol=nCol))
  #  if(sum(hasDF)>1) hasDF[hasDF>0]<-0
  #  i<-which(hasDF==0)
    i<-1:nCol
    while(length(grep("([0-9]),([0-9]{3})",x[,i]))) x[,i]<-gsub("([0-9]),([0-9]{3})","\\1\\2",x[,i])
    
    # x as vector
    x<-as.vector(x)
    
    # super and subscript
    x<-gsub("<sup>","^",x)
    x<-gsub("<sub>","_",x) 
    ## space removal
    # " ^ "
    x<-gsub(" *\\^ *","^",x)
    # " / "
    x<-gsub(" */ *","/",x)
    # remove all other html brackets
    x<-gsub("</*[a-z][^>]*/*>","",x)
    # "*^*" to "**"
    x<-gsub("\\*\\^\\*","**",gsub("\\*\\^\\*","**",x))
    # unify minus/hyphen sign
    x<-gsub("\u2212|\u02D7|\u002D|\u2013","-",x)
    
    # clean up and unify
    x<-gsub("^- ([0-9\\.])","-\\1",x)
    x<-gsub("^\\+ ([0-9\\.])","\\1",x)
    x<-gsub("(+-) ([0-9\\.])","\\1\\2",x)

    x<-gsub("^([0-9\\.-][0-9\\.\\%]*)\\(","\\1 (",x)
    x<-gsub("([0-9])  *\\%","\\1%",x)
    x<-gsub("  *"," ",x)
    # unify Pr(<|t|) -> p
    x<-gsub("Pr *\\([<>]*\\|*[a-zA-Z]\\|*\\)","p",x)
    # sparse cells to empty
    x<-gsub("^ *[-\\.][-\\.]* *$","",x)
    # remove space between letter operator numer
    x<-gsub("([A-z]) *([<=>][<=>]*) *([0-9\\.-])","\\1\\2\\3",x)
    # add coma between number/star-letter-operator
    x<-gsub("([0-9\\*])( [A-z][A-z]*[<=>][<=>]*[-0-9\\.])","\\1,\\2",x)
    if(letter.convert==TRUE) x<-letter.convert(x,greek2text=greek2text)
    # remove tailoring spaces
    x<-gsub("  *$","",x)
    x<-gsub("^  *","",x)
    x<-gsub("  *"," ",x)
    # vector to matrix
    m<-matrix(x,ncol=nCol)
    # remove empty rows/cols
    row.rm<-which(rowSums(m=="",na.rm=TRUE)==ncol(m))
    if(length(row.rm)>0) m<-m[-row.rm,]
    if(!is.matrix(m)){
      if(length(m)==0) return(NULL)
      return(matrix(m,nrow=1))
    }
    
    col.rm<-which(colSums(m=="",na.rm=TRUE)==nrow(m))
    if(length(col.rm)>0) m<-m[,-col.rm]
    if(!is.matrix(m)){
      if(length(m)==0) return(NULL)
      return(matrix(m,ncol=1))
      }
    # output
    return(m)
  }
  
  # apply function
  if(!is.list(x)) return(fun(x,letter.convert=letter.convert,greek2text=greek2text,text2num=text2num))
  if(is.list(x)) return(lapply(x,fun,letter.convert=letter.convert,greek2text=greek2text,text2num=text2num))
}
