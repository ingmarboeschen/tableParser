#' get.caption
#' 
#' Extracts the content of HTML <caption>-tags.
#' @param x A vector with HTML-coded tables.
#' @param rm.html logical. If TRUE, all HTML tags are removed, <sub> converts to '_', and <sup> to '^'.
#' @param sentences logical. If TRUE, a sentence vector is returned.
#' @param letter.convert logical. If TRUE, hexadecimal letters are converted to Unicode and unified with JATSdecoder::letter.convert.
#' @returns A character vector with the extracted caption text and NULL for no caption text 
#' @importFrom JATSdecoder strsplit2
#' @importFrom JATSdecoder text2sentences
#' @importFrom JATSdecoder letter.convert
#' @export

get.caption<-function(x,rm.html=TRUE,sentences=FALSE,letter.convert=TRUE){
  caption<-NULL
  ind<-grepl("<caption",x)
  if(sum(ind)>0){
    # select and clean up lines with <caption
    caption<-lapply(JATSdecoder::strsplit2(x,"</*caption",type="before"),function(x) grep("^<caption",x,value=TRUE))
    caption<-lapply(caption,function(x) gsub("^<captio[^>]*>","",x))
    # split at html <br> or </p>
    caption<-lapply(caption,function(x) unlist(strsplit(x,"<p/>|</p>|</*br/*>|\\\\n")))        # convert to sentences
    if(sentences==TRUE) caption<-lapply(caption,JATSdecoder::text2sentences)
    # remove html tags
    caption<-lapply(caption,function(x) gsub("<sup>","^",gsub("<sub>","_",x)))
    caption<-lapply(caption,function(x) gsub("</*sup/*>"," ",gsub("</*sub/*>"," ",x)))
    caption<-lapply(caption,function(x) gsub("</*break/*>"," ",x))
    if(rm.html==TRUE) caption<-lapply(caption,function(x) gsub("</*[a-z][^>]*/*>","",x))
    # clean up tailoring white spaces
    caption<-lapply(caption,function(x) gsub("^ | $","",gsub("  *"," ",x)))
    # unify letters
    if(letter.convert==TRUE)  caption<-lapply(caption,JATSdecoder::letter.convert)
  }
  caption<-unlist(caption)
  caption<-caption[nchar(caption)>0]
  return(caption)
}

