#' table2text
#' 
#' Parses tabled content from HTML-coded content, or HTML, DOCX, or PDF file to human-readable text vector. Before parsing, header lines are collapsed and connected cells are broken up.
#' @param x A vector with HTML tables, or a single file path to an HTML, XML, HML, PDF, or DOCX file.
#' @param unifyMatrix Logical. If TRUE, matrix cells are unified for better post-processing.
#' @param correctComma Logical. If TRUE and unifyMatrix=TRUE, decimal sign commas are converted to dots. 
#' @param decodeP Logical. If TRUE, imputes the converts the detected p-value codings to text with seperator ';;' (e.g., '1.23*' -> '1.23;; p<.01')
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, standard coding of p-values is assumed to be * p<.05, ** p<.01, and ***p<.001.
#' @param noSign2p Logical. If TRUE, imputes 'p>maximum of coded p-values' to cells that are not coded to be significant.
#' @param unifyStats Logical. If TRUE, output is unified for better post-processing (e.g., "p-value"->"p").
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footnote.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param dfHandling Logical. If TRUE, the detected sample size N in the caption/footnote is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom.
#' @param bracketHandling Logical. If TRUE and if possible, decodes numbers in brackets.
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param na.rm Logical. If TRUE, NA cells are set to empty cells.
#' @param addDescription Logical. If TRUE, the attributes table caption and table footnote are added in front of the extracted character content for better readability.
#' @param unlist Logical. If TRUE, output is returned as a vector.
#' @param addTableName Logical. If TRUE and unlist=TRUE, the table number is added in front of unlisted text lines.
#' @importFrom JATSdecoder letter.convert
#' @examples
#' ## - Download example DOCX file
#' d<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx'
#' download.file(d,paste0(tempdir(),"/","tableExamples.docx"))
#' 
#' # Parse tabled content from example file to text vectors.
#' table2text(paste0(tempdir(),"/","tableExamples.docx"))
#'
#' ## - Download example HTML file
#' h<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.html'
#' download.file(h,paste0(tempdir(),"/","tableExamples.html"))
#' 
#' # Parse tabled content from example file to text vectors.
#' table2text(paste0(tempdir(),"/","tableExamples.html"),unlist=TRUE,addDescription=TRUE)
#' 
#' \donttest{
#' ## - Download example PDF file
#' p<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.pdf'
#' download.file(p,paste0(tempdir(),"/","tableExamples.pdf"))
#'               
#' # Parse tabled content from example file to text vectors.
#' table2text(paste0(tempdir(),"/","tableExamples.pdf"),decodeP=TRUE,standardPcoding=TRUE)
#' }
#' @return A list with text vectors of the parsed table content by table. The text vector in each list element can be further processed with 'JATSdecoder::standardStats()' to extract and structure the statistical standard test results.
#' @export

table2text<-function(x,
                     unifyMatrix=TRUE,
                     unifyStats=FALSE,
                     expandAbbreviations=TRUE,
                     superscript2bracket=TRUE,
                     standardPcoding=FALSE,
                     decodeP=TRUE,
                     noSign2p=FALSE,
                     bracketHandling=TRUE,
                     dfHandling=FALSE,
                     rotate=FALSE,
                     correctComma=TRUE,
                     na.rm=TRUE,
                     addDescription=TRUE,
                     unlist=FALSE,
                     addTableName=TRUE
){
  
  # preparation/escapes
  if(is.list(x)|is.matrix(x))
    stop("x must be a vector of HTML tables, or a single file path to an HTML, XML, HML, PDF, or DOCX file.")
  if(length(x)==0)  return(NULL)
  caption<-NULL;footer<-NULL;legend<-NULL;m<-NULL;file<-FALSE
  # extract matrices and legend (caption and footnotes)
  m<-table2matrix(x,rm.html = TRUE,unifyMatrix = unifyMatrix)
  # get attributes
  caption<-unname(lapply(m,function(x) attributes(x)$caption))
  caption<-unlist(lapply(caption,paste,collapse=" "))
  footer<-unname(lapply(m,function(x) attributes(x)$footer))
  footer<-unlist(lapply(footer,paste,collapse=" "))
  # paste caption and footer
  legend <- gsub("^  *","",paste(gsub("([^[:punct:]])$","\\1.",caption),footer))
  # set legend to empty, if is non existent
  if(length(legend)!=length(m)) legend[1:length(m)]<-""
  
  # function to parse single matrix
  fun<-function(x,
                unifyMatrix=TRUE,unifyStats=FALSE,decodeP=FALSE,noSign2p=FALSE,
                expandAbbreviations=TRUE,superscript2bracket=TRUE,
                standardPcoding=FALSE,dfHandling=TRUE,bracketHandling=TRUE,
                rotate=FALSE,correctComma=FALSE,na.rm=TRUE,
                legend=NULL,unlist=FALSE){
    
    if(length(x)==0)  return(NULL)
    if(unifyMatrix==TRUE) x<-unifyMatrixContent(x,correctComma=correctComma,na.rm=na.rm)
    if(length(x)==0) return(NULL)
    
    # convert matrix to text
     out<-matrix2text(x,legend=legend,
                   expandAbbreviations=expandAbbreviations,
                   decodeP=decodeP,noSign2p=noSign2p,
                   bracketHandling=bracketHandling,
                   superscript2bracket=superscript2bracket,
                   unifyMatrix=FALSE,standardPcoding=standardPcoding,
                   dfHandling=dfHandling,na.rm=na.rm,
                   rotate=rotate,unlist=unlist
                   )
     
  if(!is.list(out)&isTRUE(unifyStats)) out<-unifyStats(out)
  if(is.list(out)&isTRUE(unifyStats)) out<-lapply(out,unifyStats)
     # output
  return(out)
  }

  # escape
  if(length(m)==0) return(NULL)
  output<-list()
  # apply function
  uniqueWarnings({
  for(i in 1:length(m)) 
    output[[i]]<-unname(unlist(fun(m[[i]],unifyMatrix=unifyMatrix,
                      unifyStats=unifyStats,decodeP=decodeP,noSign2p=noSign2p,
                      expandAbbreviations=expandAbbreviations,
                      superscript2bracket=superscript2bracket,bracketHandling=bracketHandling,
                      standardPcoding=standardPcoding,dfHandling=dfHandling,
                      correctComma=correctComma,rotate=rotate,na.rm=na.rm,
                      legend=legend[i],unlist=FALSE)))
  })
  
  # escape
  if(length(output)==0) return(NULL)
  
  if(isTRUE(addDescription))
    if(length(output)==length(caption))
      for(i in 1:length(output)) 
         output[[i]]<-grep("caption: $|footer: $",c(
                       paste0("caption: ",paste(caption[i],collapse = " ")),
                       paste0("footer: ",paste(footer[i],collapse = " ")),
                       output[[i]]),invert=TRUE,value=TRUE)
      
  # set attributes
  if(length(caption)==length(output) & !isTRUE(addDescription)){
    for(i in 1:length(m)){
      attributes(output[[i]])$caption <- caption[i]
      attributes(output[[i]])$footer <- footer[i]
    }
  }
  
  # name the listed output
  names(output)<-paste("Table",1:length(output))
  
  # remove all but one empty cell
  #output<-lapply(output,function(x){
  #  if(sum(nchar(x)==0)>0){
  #    i<-which(nchar(x)==0)
  #    if(length(i)==length(x)) return("")
  #    if(length(i)<length(x)) return(x[x!=""]) 
  #  }})
  
  # unlist with table names
  if(isTRUE(unlist)){
    n<-rep(names(output),times=unlist(lapply(output,length)))
    if(isTRUE(addTableName)) output<-paste0(n,":: ",unname(unlist(output)))
    if(!isTRUE(addTableName)) output<-unname(unlist(output))
  }  
  return(output)
  }


