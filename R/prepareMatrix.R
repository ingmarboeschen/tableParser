#' prepareMatrix
#' 
#' Prepares character matrix content for parsing. Removes empty rows and columns, extends content from plausible grouping cells to sparse cells, collapses multiple header rows, and splits multiple model tables to a list of single model tables.
#' @param x character matrix
#' @param split logical. If TRUE, multi-model matrices are split into a list of single-model matrices.
#' @param forceClass character. Set matrix-specific handling to one of c("tabled result", "correlation", "matrix, "text").
#' @param na.rm Logical. If TRUE, NA cells are set to empty cells.
#' @returns A character matrix
#' @examples
#' # example matrix
#' x<-cbind(c("","","name","","",""), 
#'          c("group","name","A","B","","C"), 
#'          c("value","","1","2","","3"))
#' x
#' 
#' # apply function
#' prepareMatrix(x)
#' @export

prepareMatrix<-function(x,split=FALSE,forceClass=NULL,na.rm=TRUE){
  # check forceClass
  if(!is.null(forceClass))
    if(sum(is.element(c("tabled result","correlation","matrix","text"),forceClass))!=1)
      stop("Argument 'forceClass' can only be set to one of 'tabled result','correlation', 'matrix', or 'text'.")
  # escapes
  if(length(x)==0) return(x)
  if(length(unlist(x))==0) return(x)
    
  # unify matrix cells
  x<-suppressWarnings(unifyMatrixContent(x,correctComma = FALSE,na.rm=na.rm))
  # remove duplicated 2nd column
  if(ncol(x)>1){
    if(sum(x[,1]==x[,2])==nrow(x)) x<-x[,-2]
    if(!is.matrix(x)) x<-as.matrix(x)
  }
  
  # parse columns with only punctuation to columns in front
  # if colname is duplicated
  i<-which(duplicated(x[1,]))
  i<-i[i>2]
  if(length(i)>0){
    sel<-NULL
    for(j in i) sel<-c(sel, length(grep("^$|^[[:punct:]][[:punct:]]*$",x[-1,j]))==nrow(x)-1)
    i<-i[sel]
    if(length(i)>0){
      for(j in i) x[-1,j-1]<-paste0(x[-1,j-1],x[-1,j])
      x<-x[,-i]
      if(!is.matrix(x)) x<-as.matrix(x)
    }
  }
  
  
  # remove empty lines/cols
  row.rm<-which(rowSums(x=="",na.rm=TRUE)==ncol(x))
  if(length(row.rm)>0) x<-x[-row.rm,]
  if(!is.matrix(x)) return(as.matrix(x))
  col.rm<-which(colSums(x=="",na.rm=TRUE)==nrow(x))
  if(length(col.rm)>0) m<-m[,-col.rm]
  if(!is.matrix(x)) return(as.matrix(x))
  
  # escape if is not matrix with at least 2 rows and 2 cols
  if(nrow(x)==1|ncol(x)==1) return(x)

  # Classify table  
  if(is.null(forceClass))  class<-tableClass(x)
  if(!is.null(forceClass)) class<-forceClass
  
  # no further processing for text matrices
  if(class=="text"|class=="vector") return(x)
  
  # replicate cells in first row that are followed by blank cells
  # cells without content
  i<-grep("^$",x[1,])
  # cells with content
  j<-grep("^..*$",x[1,])
  if(length(i)>0&length(j)>0){
  i[i>grep("^..*$",x[1,])[1]]
  i<-i[i>min(j)]
  while(length(i)>0) {
    x[1,i[1]]<-x[1,i[1]-1]
    i<-grep("^$",x[1,])
    i[i>grep("^..*$",x[1,])[1]]
    i<-i[i>min(j)]
  }
  }
  # paste text from lines with only the same value to first cells
  x<-multiHeaderSplit(x,split=FALSE,class=class)
  # handle empty cells by row (except for correlations)
  if(class!="correlation") x<-rowHandling(x)
  # collapse header rows
  x<-headerHandling(x)
  # paste first text columns to one column
  #x<-textColHandling(x)
  
  ## if second column contains enumerated text 
  # paste first and second col -> "col2 (col1)"
  if(isTRUE(
    grep("^\\(1\\)|^1\\.[A-z ]",x[-1,2])[1]==grep("^\\(2\\)|^2\\.[A-z ]",x[-1,2])[1]-1&
    grep("^\\(1\\)|^1\\.[A-z ]",x[-1,2])[1]==grep("^\\(3\\)|^3\\.[A-z ]",x[-1,2])[1]-2)){
    x[,2]<-paste0(x[,2],rep(" (",length(x[,1])),x[,1],rep(")",length(x[,1])))
    # remove first column
    x<-x[,-1]
    # remove empty brackets and space 
    x<-gsub(" *\\(\\)|^  *","",x)
  }
  # paste first and second col -> "col2 (col1)"
  if(isTRUE(
    grep("^1[\\.]*$",x[-1,2])[1]==grep("^2[\\.]*$",x[-1,2])[1]-1&
    grep("^1[\\.]*$",x[-1,2])[1]==grep("31[\\.]*$",x[-1,2])[1]-2)){
    x[,2]<-paste0(x[,2],rep(" ",length(x[,1])),x[,1])
    # remove first column
    x<-x[,-1]
    # remove space 
    x<-gsub("^  *","",x)
  }
  
  # reclassify if forceClass is null
  if(is.null(forceClass))  class<-tableClass(x)
  
  if(class=="correlation"){
    ## parse first and second column, 
    # if first col has no letters and second column starts with letter in every cell
    if(length(grep("[A-z]",x[-1,1]))==0&length(grep("^[A-z]",x[-1,2]))==(nrow(x)-1)){
      x[,1]<-paste(x[,1],x[,2])
      x<-x[,-2]
    }
    
    # parse first and second row, if first row has no letters and second row starts with letter in every cell
    if(length(grep("[A-z]",x[1,-1]))==0&length(grep("^[A-z]",x[2,-1]))==(ncol(x)-1)){
      x[1,]<-paste(x[1,],x[2,])
      x<-x[-2,]
    }
    
  }
  
  
  # repeat cells in first column if there are empty cells
  if(class!="vector"){
    if(sum(x[-1,1]=="",na.rm=TRUE)>0&length(grep("^[1-9]",x[-1,1]))==0) for(i in which(x[-1,1]==""))  x[i+1,1]<-x[i,1]
    # handle empty cells by column
    x<-colHandling(x)
  }
  

  # as long as multiple fields in second row contain only text, paste to cells in first row and remove row
  while(
    sum(grepl("[0-9]",gsub("[1-9][0-9\\.][- ]*%","",x[2,]))) == 0 & 
    sum(grepl("[A-z]",x[2,])) > 1 & 
    nrow(x) > 2 & 
    # no line with only the same values
    sum(duplicated(x[2,]))!= ncol(x) & 
    # and at least 30% of all other fields contain numbers 
    sum(grepl("[0-9]|^$",x[-1:-2,-1]))/length(x[-1:-2,-1])>.3
  ){
    x[2,][x[1,]==x[2,]]<-""
    x[1,]<-gsub("^  *|  *$","",paste(x[1,],x[2,]))
    x<-x[-2,]
  }


  # split matrices
  if(isTRUE(split)){
    # split at columns for enumerated multi models
    if(is.list(x)){
      for(i in 1:length(x))
      x[[i]]<-splitter(x[[i]])
      }
    if(is.matrix(x)) x<-splitter(x)
    # flatten list of lists with matrices to simple list
    x<-flatten(x)
  }

return(x)      
}

