#' get.HTML.tables
#'
#' Extracts HTML tables as vector of HTML coded tables from plain HTML code, HTML, HML, XML or CERMXML files.
#' @param x HTML, HML, XML or CERMXML file or character object with HTML-encoded content.
#' @return Character vector with one HTML-encoded table per cell.
#' @export

get.HTML.tables<-function(x){
  # escapes for bad file formats
  if(length(grep("<[A-z][^>]*>.*</[^>]*>",x))==0 & length(x==1))
    if(file.exists(x) &  !is.element(toupper(gsub(".*\\.([A-z][A-z]*)$","\\1",x)),c("HTML","HML","XML","CERMXML")))
    stop("File input must be of either HTML, HML, XML or CERMXML format.")
  
  ## run prechecks then readLines(x) if x is file
  # check if x is of length 0
  if(length(x)==0) return(NULL)
  # check if x is NA
  if(is.na(x)[1]) return(NULL)
  # check if x is character
  stopifnot('"x" must be a HTML encoded file or text' = is.character(x[1]))
  # readLines if x is file
  if(file.exists(x[1])){
    # check if x is of length=1
    stopifnot('File input must be of length=1.'=length(x)==1)
    # read file content
    x<-readLines(x,warn=FALSE,encoding="UTF-8")
  }
  
  # replace newline sign with space
  x<-gsub("\\n"," ",x)
  
  tables<-character(0)
  if(sum(grep("<table[-> ]",x))>0){
    # without table wrap
    if(sum(grep("</table-wrap[> ]",x))==0){
      # split collapsed lines at <table
      tables<-paste(x,collapse=" ")
      tables<-unlist(strsplit2(tables,"<table>|<table [a-z]","before"))
      tables<-unlist(strsplit2(tables,"</table>","after"))
      # select lines with <table>
      tables<-grep("<table>|<table [a-z]|</table>",tables,value=TRUE)
    }
    
    # with table wrap
    if(sum(grep("<table-wrap[> ]",x))>0){
      # split collapsed lines at <table-wrap
      tables<-paste(x,collapse=" ")
      tables<-unlist(strsplit2(tables,"<table-wrap[> ]","before"))
      tables<-unlist(strsplit2(tables,"</table-wrap>","after"))
      # select lines with table wrap
      tables<-grep("<table-wrap[> ]",tables,value=TRUE)
    }
    
  } 
  return(unlist(tables))
}

