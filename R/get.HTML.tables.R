#' get.HTML.tables
#' 
#' Extracts HTML tables as a vector of HTML-coded tables from plain HTML code, HTML, HML, or XML files. If tables are nested within tables, only the inner tables are extracted.
#' @param x HTML, HML, or XML file; or character object with HTML-encoded content.
#' @return Character vector with one HTML-encoded table per cell.
#' @examples 
#' x<-readLines("https://en.wikipedia.org/wiki/R_(programming_language)",warn=FALSE)
#' get.HTML.tables(x)
#' @importFrom JATSdecoder strsplit2
#' @export

get.HTML.tables<-function(x){
  # escapes for bad file formats
  if(length(grep("<[A-z][^>]*>.*</[^>]*>",x))==0 & length(x==1))
    if(file.exists(x) &  !is.element(toupper(gsub(".*\\.([A-z][A-z]*)$","\\1",x)),c("HTML","HML","XML","NXML","CERMXML")))
    stop("File input must be of either HTML, HML, or XML format.")
  
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
      # split in front of <table>
      tables<-unlist(JATSdecoder::strsplit2(tables,"<table>|<table [a-z]","before"))
      # select lines with </table>
      tables<-grep("</table>|</table [a-z]",tables,value=TRUE)
      # split behind </table>
      tables<-unlist(JATSdecoder::strsplit2(tables,"</table>","after"))
      # select lines with <table>
      tables<-grep("<table>|<table [a-z]",tables,value=TRUE)
    }
    
    # with table wrap
    if(sum(grep("<table-wrap[> ]",x))>0){
      # split collapsed lines at <table-wrap
      tables<-paste(x,collapse=" ")
      tables<-unlist(JATSdecoder::strsplit2(tables,"<table-wrap[> ]","before"))
      tables<-unlist(JATSdecoder::strsplit2(tables,"</table-wrap>","after"))
      # select lines with table wrap
      tables<-grep("<table-wrap[> ]",tables,value=TRUE)
    }
    
  } 
  return(unlist(tables))
}

