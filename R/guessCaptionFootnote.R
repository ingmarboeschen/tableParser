#' guessCaptionFootnote
#' 
#' Extracts text blocks around tables within DOCX, HTML, HML, XML, or NXML files in order to return the table captions and footnotes.
#' @param x character. A file path.
#' @param MaxCaptionLength numeric. The maximum number of sentences within a text block that shall be treated as a caption. Text blocks that contain more sentences than this threshold are not extracted.
#' @param MaxFootnoteLength numeric. The maximum number of sentences within a text block that shall be treated as a footnote. Text blocks that contain more sentences than this threshold are not extracted.
#' @examples
#' ## Download an example DOCX file from tableParser's github repo to temp directory 
#' d<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx'
#' download.file(d,paste0(tempdir(),"/","tableExamples.docx"))
#' 
#' ## Download an example HTML file from tableParser's github repo to temp directory 
#' h<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.html'
#' download.file(h,paste0(tempdir(),"/","tableExamples.html"))
#' 
#' ## Extract table captions and footnotes 
#' # DOCX file
#' \donttest{guessCaptionFootnote(paste0(tempdir(),"/","tableExamples.docx"))}
#' # HTML file
#' guessCaptionFootnote(paste0(tempdir(),"/","tableExamples.html"))
#' @return A list with the extracted table captions and footers as vectors of length=number of tables.
#' @importFrom JATSdecoder strsplit2
#' @importFrom JATSdecoder text2sentences
#' @importFrom JATSdecoder letter.convert
#' @export

guessCaptionFootnote<-function(x,
                             MaxCaptionLength=1,
                             MaxFootnoteLength=4){
  
  # escapes for bad file formats
  if(length(grep("^<table",x))==0 & length(x)==1)
    if(file.exists(x) &  !is.element(toupper(gsub(".*\\.([A-z][A-z]*)$","\\1",x)),c("HTML","HML","XML","NXML","CERMXML","DOCX")))
      stop("File input format must be of either DOCX, HTML, HML, NXML, or XML format.")
  
  # get file type
  type<-tolower(gsub(".*\\.([A-z][A-z]*)$","\\1",x[1]))
  isFile<-file.exists(x[1])
  
  #if(gsub(".*\\.docx$","docx",tolower(x))!="docx") stop("x must be a file path to a DOCX file.")
  
  # extract and read xml within docx
  if(is.element("docx",type)){
    # Validate input
    if(!file.exists(x[1]))
      stop("File does not exist: ", x[1])
    
    # temp ZIP path
    tempZip <- file.path(tempdir(), "temp.zip")
    
    # Copy the .docx file (which is a ZIP)
    ok <- file.copy(from = normalizePath(x[1], winslash="/", mustWork=TRUE),
      to = tempZip,overwrite = TRUE)
    
    if(!ok) stop("Failed to copy file")
    
    Sys.sleep(0.2) # Windows file‑locking fix
    
    # Unzip
    unzipped_files <- utils::unzip(zipfile = tempZip, exdir = tempdir(), unzip = "internal")
    
    # Read document.xml
    docxml <- file.path(tempdir(), "word", "document.xml")
    b <- readLines(docxml, warn = FALSE)
    
    i<-grep("<w:tbl>",b)
    # escape if has no tables
    if(length(i)==0) return(list(caption=NULL,footer=NULL))
    
    b<-unlist(JATSdecoder::strsplit2(b,"<w:tbl>|<w:p>","before"))
    b<-unlist(JATSdecoder::strsplit2(b,"</w:tbl>|</w:p>","after"))
    
    s<-grep("<w:tbl>",b)
    e<-grep("</w:tbl>",b)
    
    # collapse content from tables to one row
    for(i in 1:length(s)) 
      b[s[i]]<-paste(b[s[i]:e[i]],collapse = " ")
    for(i in length(s):1) 
      b<-b[-((s[i]+1):e[i])]
    # lines with tables
    i<-grep("<w:tbl>",b)
    i[i==1]<-NA
    
    # remove empty entries in front of tables
    for(j in 1:length(i))
      while(nchar(gsub("<[^>][^>]*>","",b[i[j]-1]))==0){
        b<-b[-(i[j]-1)]
        i<-grep("<w:tbl>",b)
        i[i==1]<-NA
      }
    
    # unify sub and superscript
    b<-gsub("(<w[^>]*val=\"subscript\"/><[^>]*><[^>]*>)([A-z0-9])","\\1_\\2",b)
    b<-gsub("(<w[^>]*val=\"subscript\"/><[^>]*>)([A-z0-9])","\\1_\\2",b)
    b<-gsub("(<w[^>]*val=\"subscript\"/>)([A-z0-9])","\\1_\\2",b)
    
    b<-gsub("(<w[^>]*val=\"superscript\"/><[^>]*><[^>]*>)([A-z0-9])","\\1^\\2",b)
    b<-gsub("(<w[^>]*val=\"superscript\"/><[^>]*>)([A-z0-9])","\\1^\\2",b)
    b<-gsub("(<w[^>]*val=\"superscript\"/>)([A-z0-9])","\\1^\\2",b)
    
    # superscripted 2
    b<-gsub("\u00b3","^2",b)
    
    # extract text block in front of tables
    i[i==1]<-NA
    i[is.element(i-1,i)]<-NA
    
    caption<-JATSdecoder::letter.convert(gsub("<[^>][^>]*>","",b[i-1]))
    caption
    b[i-1]<-"REMOVED"
    
    # remove empty entries behind of tables
    i<-grep("<w:tbl>",b)
    for(j in 1:length(i)){
      while(nchar(gsub("<[^>][^>]*>","",b[i[j]+1]))==0&b[i[j]+1]!="REMOVED" &
            i[j]<length(b) ){
        b<-b[-(i[j]+1)]
        i<-grep("<w:tbl>",b)
      }
    }
    # extract text block behind each table
    i[is.element(i+1,i)|b[i+1]=="REMOVED"]<-NA
    footer1<-gsub("<[^>][^>]*>","",b[i+1])
    
    i[is.element(i+2,i)|is.na(footer1)]<-NA
    footer2<-gsub("<[^>][^>]*>","",b[i+2])
    
    i[is.element(i+3,i)|is.na(footer2)]<-NA
    footer3<-gsub("<[^>][^>]*>","",b[i+3])
    
    footer1[is.na(footer1)]<-""
    footer2[is.na(footer2)|footer2==""]<-"REMOVE"
    footer3[is.na(footer3)|footer3==""]<-"REMOVE"
    
    # add . ad end if has none.
    footer1<-gsub("([0-9A-z])$","\\1.",footer1)
    footer2<-gsub("([0-9A-z])$","\\1.",footer2)
    footer3<-gsub("([0-9A-z])$","\\1.",footer3)
    
    footer<-JATSdecoder::letter.convert(gsub("  *"," ",gsub(" *REMOVED*.*|^  *|  *$","",paste(footer1,footer2,footer3))))
    
    # remove caption, if has more than 2 sentences
    l<-unlist(lapply(lapply(caption,JATSdecoder::text2sentences),length))
    caption[l>MaxCaptionLength]<-""
    
    # remove footer, if has more than 4 sentences
    l<-unlist(lapply(lapply(footer,JATSdecoder::text2sentences),length))
    footer[l>MaxFootnoteLength]<-""
    
    out<-list(caption=caption,footer=footer)
  }
  
  ## for non DOCX files
  if(!is.element("docx",type)){
    a<-readLines(x,warn=FALSE)
    i<-grep("<table[> ]",a)
    # escape
    if(length(i)==0) return(list(caption=NULL,footer=NULL))
    b<-paste(a,collapse=" ")
    # split
    b<-unlist(JATSdecoder::strsplit2(b,"<div[> ]","before"))
    b<-unlist(JATSdecoder::strsplit2(b,"</div[> ]","after"))
    b<-unlist(JATSdecoder::strsplit2(b,"<p[> ]","before"))
    b<-unlist(JATSdecoder::strsplit2(b,"</p[> ]","after"))
    
    b<-unlist(JATSdecoder::strsplit2(b,"<table[> ]","before"))
    b<-unlist(JATSdecoder::strsplit2(b,"</table[> ]","after"))
    
    # collapse content from tables to one row
    s<-grep("<table[> ]",b)
    e<-grep("</table[> ]",b)
    for(i in 1:length(s)) 
      b[s[i]]<-paste(b[s[i]:e[i]],collapse = " ")
    for(i in length(s):1) 
      b<-b[-((s[i]+1):e[i])]
    # lines with tables
    i<-grepl("<table[> ]",b)
    
    # remove empty entries 
    b<-b[nchar(gsub("^ *$|^ ","",gsub("  "," ",gsub("<[^>][^>]*>","",b[]))))>0]
    
    # super and subscript
    b<-gsub("<sup>","^",b)
    b<-gsub("<sub>","_",b) 
    # superscripted 2
    b<-gsub("\u00b3","^2",b)
    
    # extract text block in front of tables
    i<-grep("<table[> ]",b)
    i<-i[i!=1]
    i[is.element(i-1,i)]<-NA
    
    caption<-JATSdecoder::letter.convert(gsub("<[^>][^>]*>","",b[i-1]))
    caption
    b[i-1]<-"REMOVED"
    
    # extract text block behind each table
    i<-grep("<table[> ]",b)
    # footer 1
    i[is.element(i+1,i)|b[i+1]=="REMOVED"]<-NA
    footer1<-gsub("^  *|  *$","",gsub("<[^>][^>]*>","",b[i+1]))
    # set cases with too many sentences to NA
    footer1[unlist(lapply(lapply(footer1,JATSdecoder::text2sentences),length))>MaxFootnoteLength]<-NA
    
    # footer 2
    i[is.element(i+2,i)|is.na(footer1)]<-NA
    footer2<-gsub("^  *|  *$","",gsub("<[^>][^>]*>","",b[i+2]))
    # set cases with too many sentences to NA
    footer2[unlist(lapply(lapply(footer2,JATSdecoder::text2sentences),length))>MaxFootnoteLength]<-NA
    
    ## footer 3
    i[is.element(i+3,i)|is.na(footer2)]<-NA
    footer3<-gsub("^  *|  *$","",gsub("<[^>][^>]*>","",b[i+3]))
    # set cases with too many sentences to NA
    footer3[unlist(lapply(lapply(footer3,JATSdecoder::text2sentences),length))>MaxFootnoteLength]<-NA
    
    footer1[is.na(footer1)]<-""
    footer2[is.na(footer2)|footer2==""]<-"REMOVE"
    footer3[is.na(footer3)|footer3==""]<-"REMOVE"
    
    footer<-JATSdecoder::letter.convert(gsub("  *"," ",gsub(" *REMOVED*.*|^  *|  *$","",paste(footer1,footer2,footer3))))
    
    # remove caption, if has more than MaxCaptionLength sentences
    l<-unlist(lapply(lapply(caption,JATSdecoder::text2sentences),length))
    caption[l>MaxCaptionLength]<-""
    
    # remove footer, if has more than MaxFootnoteLength sentences
    l<-unlist(lapply(lapply(footer,JATSdecoder::text2sentences),length))
    footer[l>MaxFootnoteLength]<-""
    
    out<-list(caption=caption,footer=footer)
  }
  
  return(out)
}

