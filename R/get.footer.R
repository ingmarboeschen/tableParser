#' get.footer
#' 
#' Extracts the content of HTML <table-wrap-foot>-tag/s.
#' @param x A vector with HTML-coded tables.
#' @param rm.html logical. If TRUE, all HTML tags are removed, <sub> converts to '_', and <sup> to '^'.
#' @param sentences logical. If TRUE, a sentence vector is returned.
#' @param letter.convert logical. If TRUE, hexadecimal letters are converted to Unicode and unified with JATSdecoder::letter.convert.
#' @returns A character vector with the extracted footer text and NULL for no footer text.
#' @importFrom JATSdecoder strsplit2
#' @importFrom JATSdecoder text2sentences
#' @importFrom JATSdecoder letter.convert
#' @export

get.footer<-function(x,rm.html=TRUE,sentences=FALSE,letter.convert=TRUE){
  footer<-rep(character(0))
  ind<-grepl("<table-wrap-foot",x)
  if(sum(ind)>0){
    # select and clean up lines with <table-wrap-foot
    footer<-lapply(strsplit2(x,"</*table-wrap-foot",type="before"),function(x) grep("^<table-wrap-foot",x,value=TRUE))
    footer<-lapply(footer,function(x) gsub("^<table-wrap-foot[^>]*>","",x))
    # split at html <br> or </p>
    footer<-lapply(footer,function(x) unlist(strsplit(x,"<p/>|</p>|</*br/*>|\\\\n")))
    # convert to sentences
    if(sentences==TRUE) footer<-lapply(footer,text2sentences)
    # remove html tags, and convert sup and sub
    footer<-lapply(footer,function(x) gsub("<sup>","^",gsub("<sub>","_",x)))
    footer<-lapply(footer,function(x) gsub("</*sup/*>"," ",gsub("</*sub/*>"," ",x)))
    footer<-lapply(footer,function(x) gsub("</*break/*>"," ",x))
    if(rm.html==TRUE) footer<-lapply(footer,function(x) gsub("</*[a-z][^>]*/*>","",x))
    # clean up tailoring white spaces
    footer<-lapply(footer,function(x) gsub("^ | $","",gsub("  *"," ",x)))
    # hex/special letter conversion
    # unify letters
    if(letter.convert==TRUE)  footer<-lapply(footer,letter.convert)
  }
  footer<-unlist(footer)
  footer<-footer[nchar(footer)>0]
  return(footer)
}
