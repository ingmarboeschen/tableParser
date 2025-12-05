#' docx2matrix
#'
#' Extracts tables from DOCX documents and returns list of character matrices.
#' @param x File path to a DOCX input file with tables.
#' @param replicate Logical. If TRUE, replicates content when splitting connected cells.
#' @return List with extracted tables as character matrices.
#' @export

docx2matrix<-function(x,replicate=TRUE){
  if(gsub(".*\\.docx$","docx",tolower(x))!="docx") stop("x must be a file path to a DOCX file.")
  if(!file.exists(x[1])) readLines(con=x[1])
  tempZip<-paste0(tempdir(),"/temp.zip")
  file.copy(x, tempZip,overwrite=TRUE)
  a<-utils::unzip(tempZip,"word/document.xml",exdir=tempdir())
  
  d<-paste(readLines(a,warn = FALSE),collapse=" ")
  # extract tables as vector
  t<-unlist(strsplit(d,"<w:tbl>"))[-1]
  # escape
  if(length(t)==0) return(NULL)

  t<-paste0("<w:tbl>",t)
  t<-grep("<w:tbl>",unlist(strsplit(t,"</w:tbl>")),value=T)
  y<-paste0(t,"</w:tbl>")
  
  #doc<-xml2::read_xml(a)
  # extract namespace
  #ns<-xml2::xml_ns(doc)
  
  # get tables
  #tbls<-xml2::xml_find_all(doc,".//w:tbl",ns=ns)
  # as character
  #y<-as.character(tbls)

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
  if(replicate==TRUE){
    if((i+Nadded)<length(y)&reps>0) y<-c(y[1:(i+Nadded)],rep(y[i+Nadded],reps),y[(i+1+Nadded):length(y)])
    if((i+Nadded)==length(y)&reps>0) y<-c(y[1:(i+Nadded)],rep(y[i+Nadded],reps))
  }
  
  
  if(replicate==FALSE){
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

  # add ^bold/^italic to cells with <w:b/<w:i
  y[grep("<w:b */",y)]<-  paste0(y[grep("<w:b */",y)],"^bold")
  y[grep("<w:i */",y)]<-  paste0(y[grep("<w:i */",y)],"^italic")
  
  
# clean up
y<-gsub("</w:t>[^<]*<w:t>","",y)
y<-gsub("<[^>]*>| *\\n *","",y)
#y<-gsub("  *"," ",y)
#y<-gsub("^ | $","",y)
}

y<-lapply(y,tempFun,replicate=replicate)
return(y)
}

