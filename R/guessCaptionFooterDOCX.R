#' guessCaptionFooterDOCX
#' Extracts text blocks around tables within docx files, in order to return the tables caption/footer.
#' @param x character. A file path to a DOCX file.
#' @param MaxCaptionLength numeric. The maximum number of sentences within a text block, that shall be treated as caption. Text blocks that contain more sentences than this threshold are not extracted at all.
#' @param MaxFooterLength numeric. The maximum number of sentences within a text block, that shall be treated as footer. Text blocks that contain more sentences than this threshold are not extracted at all.
#' @return A list with the extracted table captions and footers as vectors of length=number of tables.
#' @importFrom JATSdecoder strsplit2
#' @importFrom JATSdecoder text2sentences
#' @importFrom JATSdecoder letter.convert
#' @export

guessCaptionFooterDOCX<-function(x,
                                 MaxCaptionLength=1,
                                 MaxFooterLength=4){
  
  if(gsub(".*\\.docx$","docx",tolower(x))!="docx") stop("x must be a file path to a DOCX file.")
  if(!file.exists(x[1])) readLines(con=x[1])
  tempZip<-paste0(tempdir(),"/temp.zip")
  file.copy(x, tempZip,overwrite=TRUE)
  a<-utils::unzip(tempZip,"word/document.xml",exdir=tempdir())
  
  b<-readLines(a,warn=FALSE)
  i<-grep("<w:tbl>",b)
  
  if(length(i)==0) return(list(caption=NULL,footer=NULL))
  
  b<-unlist(strsplit2(b,"<w:tbl>|<w:p>","before"))
  b<-unlist(strsplit2(b,"</w:tbl>|</w:p>","after"))
  
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
  # extract text block in front of tables
  i[i==1]<-NA
  i[is.element(i-1,i)]<-NA
  
  caption<-letter.convert(gsub("<[^>][^>]*>","",b[i-1]))
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
  
  footer<-letter.convert(gsub("  *"," ",gsub(" *REMOVED*.*|^  *|  *$","",paste(footer1,footer2,footer3))))
  
  # remove caption, if has more than 2 sentences
  l<-unlist(lapply(lapply(caption,text2sentences),length))
  caption[l>MaxCaptionLength]<-""
  
  # remove footer, if has more than 4 sentences
  l<-unlist(lapply(lapply(footer,text2sentences),length))
  footer[l>MaxFooterLength]<-""
  
  return(list(caption=caption,footer=footer))
}

