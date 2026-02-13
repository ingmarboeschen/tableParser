#' docx2matrix
#' 
#' Extracts tables from DOCX documents and returns a list of character matrices.
#' @param x File path to a DOCX input file with tables.
#' @param unifyMatrix Logical. If TRUE, matrix cells are unified for better post-processing (see 'unifyMatrixContent()').
#' @param correctComma Logical. If TRUE, commas used as decimal are converted to dots, big mark commas are removed. 
#' @param replicate Logical. If TRUE, replicates content when splitting connected cells.
#' @examples
#' ## Download an example DOCX file from tableParser's github repo to temp directory 
#' d<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx'
#' tempFile<-paste0(tempdir(),"/","tableExamples.docx")
#' 
#' # on Windows with method="wget"
#' if(grepl("^[A-z]:",tempFile))
#'    download.file(d,tempFile,method="wget")
#' # on all other machines
#' if(!grepl("^[A-z]:",tempFile))
#'    download.file(d,tempFile)
#'    
#' Sys.sleep(.2)
#' 
#' # Extract tables as character matrices
#' docx2matrix(tempFile)
#' @return List with extracted tables as character matrices.
#' @export

docx2matrix<-function(x,unifyMatrix=FALSE,correctComma=FALSE,replicate=TRUE){
  # escapes
  if(gsub(".*\\.docx$","docx",tolower(x[1]))!="docx") stop("Input must be a file path to a DOCX file.")
  if(length(x)>1) stop("Input must be a single DOCX file.")
  if(!file.exists(x[1])) stop("File does not exist.")
  
  tempZip <- file.path(tempdir(), "temp.zip")
  # copy
  file.copy(
    from=normalizePath(x, winslash="/", mustWork=TRUE),
    to=tempZip,overwrite = TRUE,copy.mode=FALSE,copy.date=FALSE)
  
    tempZip <- normalizePath(tempZip, winslash="/", mustWork=TRUE)
    
    # unzip
    a <- utils::unzip(tempZip, exdir = tempdir())
    a <- file.path(tempdir(), "word", "document.xml")
    
    # read xml file
    d<-paste(readLines(a,warn = FALSE),collapse=" ")
    
    # extract tables as vector
    t<-unlist(strsplit(d,"<w:tbl>"))[-1]
    # escape
    if(length(t)==0) return(NULL)

  t<-paste0("<w:tbl>",t)
  t<-grep("<w:tbl>",unlist(strsplit(t,"</w:tbl>")),value=T)
  y<-paste0(t,"</w:tbl>")
  
  # function to convert single table
  tempFun<-function(y,replicate=FALSE){
  if(length(grep("<w:tr>",y))==0) return(NULL)
  # split to row vector
  y<-unlist(strsplit(y,"<w:tr>"))[-1]
  # split to list with cells
  y<-lapply(strsplit(y,"<w:tc>"),"[",-1)

  nRow<-length(y)
  y<-unlist(y)
  
  # repeat cells by rowspan
  ind<-grep("<w:gridSpan",y)
  Nadded<-0
  
  for(i in ind){
  reps<-as.numeric(gsub(".*<w:gridSpan w:val=.([1-9][1-9]*).*","\\1",y[i+Nadded]))-1
  if(isTRUE(replicate)){
    if((i+Nadded)<length(y)&reps>0) y<-c(y[1:(i+Nadded)],rep(y[i+Nadded],reps),y[(i+1+Nadded):length(y)])
    if((i+Nadded)==length(y)&reps>0) y<-c(y[1:(i+Nadded)],rep(y[i+Nadded],reps))
  }
  
  
  if(isFALSE(replicate)){
    if((i+Nadded)<length(y)&reps>0) y<-c(y[1:(i+Nadded)],rep("",reps),y[(i+1+Nadded):length(y)])
    if((i+Nadded)==length(y)&reps>0) y<-c(y[1:(i+Nadded)],rep("",reps))
  }
  Nadded<-Nadded+reps
  }

  y<-matrix(y,nrow=nRow,byrow=T)

  # replicate cells in combined rows
  while(replicate&length(grep("w:val=\"continue\"",y))>0){
   col<-which(colSums(matrix(grepl("w:val=\"continue\"",y),nrow=nRow))>0)[1]
   cell<-grep("continue",y[,col])[1]
   y[cell,col]<-y[cell-1,col]
  }

  # unify sub and superscript
  y<-gsub("(<w[^>]*val=\"subscript\"/><[^>]*><[^>]*>)([A-z0-9])","\\1_\\2",y)
  y<-gsub("(<w[^>]*val=\"subscript\"/><[^>]*>)([A-z0-9])","\\1_\\2",y)
  y<-gsub("(<w[^>]*val=\"subscript\"/>)([A-z0-9])","\\1_\\2",y)
  
  y<-gsub("(<w[^>]*val=\"superscript\"/><[^>]*><[^>]*>)([A-z0-9])","\\1^\\2",y)
  y<-gsub("(<w[^>]*val=\"superscript\"/><[^>]*>)([A-z0-9])","\\1^\\2",y)
  y<-gsub("(<w[^>]*val=\"superscript\"/>)([A-z0-9])","\\1^\\2",y)
  
  # add ^bold/^italic to cells with <w:b/<w:i
  y[grep("<w:b */",y)]<-  paste0(y[grep("<w:b */",y)],"^bold")
  y[grep("<w:i */",y)]<-  paste0(y[grep("<w:i */",y)],"^italic")
  
  # clean up
  y<-gsub("</w:t>[^<]*<w:t>","",y)
  y<-gsub("<[^>]*>| *\\n *","",y)
  #y<-gsub("  *"," ",y)
  #y<-gsub("^ | $","",y)
  }
  
  # apply functions
  y<-lapply(y,tempFun,replicate=replicate)
  if(isTRUE(unifyMatrix)) y<-lapply(y,unifyMatrixContent,correctComma=correctComma) 
  
  return(y)
}

