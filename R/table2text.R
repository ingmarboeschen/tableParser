#' table2text
#'
#' Parses tabled content from HTML-coded content, or HTML, DOCX, or PDF file to human-readable text vector. Before parsing, header lines are collapsed and connected cells are broken up.
#' @param x A vector with HTML tables, or a single file path to an HTML, XML, HML, PDF, or DOCX file.
#' @param unifyMatrix Logical. If TRUE, matrix cells are unified for better post-processing.
#' @param unifyStats Logical. If TRUE, output is unified for better post-processing (e.g., "p-value"->"p").
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footer.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param addDF Logical. If TRUE, detected sample size N in the caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, standard coding of p-values is assumed to be * p<.05, ** p<.01, and ***p<.001.
#' @param noSign2p Logical. If TRUE, imputes 'p>maximum of coded p-values' to cells that are not coded to be significant.
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param correctComma Logical. If TRUE and unifyMatrix=TRUE, decimal sign commas are converted to dots. 
#' @param na.rm Logical. If TRUE, NA cells are set to empty cells.
#' @param addDescription Logical. If TRUE, the table caption and footer are added before the extracted table content for better readability.
#' @param unlist Logical. If TRUE, output is returned as a vector.
#' @param addTableName Logical. If TRUE and unlist=TRUE, the table number is added in front of unlisted text lines.
#' @importFrom JATSdecoder letter.convert
#' @return A list with text vectors of the parsed table content by table. The text vector in each list element can be further processed with JATSdecoder::standardStats() to extract and structure the statistical standard test results.
#' @export

table2text<-function(x,
                     unifyMatrix=TRUE,
                     unifyStats=FALSE,
                     expandAbbreviations=TRUE,
                     superscript2bracket=TRUE,
                     standardPcoding=FALSE,
                     noSign2p=FALSE,
                     addDF=FALSE,
                     rotate=FALSE,
                     correctComma=FALSE,
                     na.rm=TRUE,
                     addDescription=TRUE,
                     unlist=FALSE,addTableName=TRUE
){
  
  # preparation/escapes
  if(is.list(x)|is.matrix(x))
    stop("x must be a vector of HTML tables, or a single file path to an HTML, XML, HML, PDF, or DOCX file.")
  if(length(x)==0)  return(NULL)

  caption<-NULL;footer<-NULL;legend<-list();m<-NULL;file<-FALSE
  
  # get tables as matrix if x is a file
  if(length(x)==1 & file.exists(x[1])){ 
    file<-TRUE
    # get file type
    type<-tolower(gsub(".*\\.([A-z][A-z]*)$","\\1",x))
    # escape
    if(!is.element(type,c("cermxml","xml","nxml","html","hml","pdf","docx"))){
      stop("Input file format must be either HTML, XML, HML, PDF, or DOCX.")
    }
    # docx 
    if(is.element(type,c("docx"))){
      m<-docx2matrix(x)
      
      if(length(m)>0){
      # extract and combine legend text
      y<-guessCaptionFooterDOCX(x)
      caption<-lapply(y$caption,function(x) return(x))
      footer<-lapply(y$footer,function(x) return(x))
      for(i in 1:length(m)){
        legend[[i]]<-c(caption[[i]],footer[[i]])
      }
      }
      
    }
    # pdf
    if(type=="pdf"){
      x<-tabulapdf::extract_tables(x,output="matrix")
      m<-lapply(x,as.matrix)
    }
    # HTML 
    if(is.element(type,c("cermxml","xml","nxml","html","hml"))){
      x<-get.HTML.tables(x)
      
    }
    if(length(x)==0)  return(NULL)
    
    }# end if is file
  
  # for HTML tables with legend text
  if(!is.list(x)&!is.matrix(x)){
  if(length(grep("<table",x[1]))>0){
    # split multiple tables inside of one <table-wrap>-tag
    x<-multiTable(x)
    # extract and combine legend text
    caption<-lapply(x,get.caption)
    footer<-lapply(x,get.footer)
    for(i in 1:length(x)){
      legend[[i]]<-c(caption[[i]],footer[[i]])
    }
    
    # convert HTML tables to matrix
    m<-table2matrix(x,letter.convert=TRUE,rm.html=TRUE,replicate=TRUE,
                    collapseHeader=TRUE)
  
  }else{
    if(!file.exists(x[1])) 
      stop("x must be a vector of HTML tables, or a single file path to an HTML, XML, HML, PDF, or DOCX file.")
  }
  }
  
  # prepare and escape if no matrix extracted
  if(is.matrix(m)) m<-list(m)
  if(!is.list(m)) return(NULL)
  
  # set legend to empty, if non is existent
  if(length(legend)==0) legend[1:length(m)]<-""

  # function to convert matrix with matrix2text
  fun<-function(x,
                unifyMatrix=TRUE,unifyStats=FALSE,noSign2p=FALSE,
                expandAbbreviations=TRUE,superscript2bracket=TRUE,
                standardPcoding=FALSE,addDF=TRUE,
                rotate=FALSE,correctComma=FALSE,na.rm=TRUE,
                legend=NULL,unlist=FALSE){
    
    if(length(x)==0)  return(NULL)
    if(unifyMatrix==TRUE) x<-unifyMatrixContent(x,correctComma=correctComma,na.rm=na.rm)
    if(length(x)==0) return(NULL)
    
    # convert matrix to text
     out<-matrix2text(x,legend=legend,
                   expandAbbreviations=expandAbbreviations,noSign2p=noSign2p,
                   superscript2bracket=superscript2bracket,
                   unifyMatrix=FALSE,standardPcoding=standardPcoding,
                   addDF=addDF,na.rm=na.rm,
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
  uniqueWarnings({
  for(i in 1:length(m)) 
    output[[i]]<-unname(unlist(fun(m[[i]],unifyMatrix=unifyMatrix,
                      unifyStats=unifyStats,noSign2p=noSign2p,
                      expandAbbreviations=expandAbbreviations,
                      superscript2bracket=superscript2bracket,
                      standardPcoding=standardPcoding,addDF=addDF,
                      correctComma=correctComma,rotate=rotate,na.rm=na.rm,
                      legend=legend[[i]],unlist=FALSE)))
  })
  
  # escape
  if(length(output)==0) return(NULL)
  
  if(isTRUE(addDescription))
  for(i in 1:length(output)) 
    output[[i]]<-grep("caption: $|footer: $",c(
      paste0("caption: ",paste(caption[[i]],collapse = " ")),
      paste0("footer: ",paste(footer[[i]],collapse = " ")),
                   output[[i]]),invert=TRUE,value=TRUE)
      
    
  # name the listed output
    names(output)<-paste("Table",1:length(output))
      
  # unlist with table names
  if(isTRUE(unlist)){
    n<-rep(names(output),times=unlist(lapply(output,length)))
    if(isTRUE(addTableName)) output<-paste0(n,":: ",unname(unlist(output)))
    if(!isTRUE(addTableName)) output<-unname(unlist(output))
  }  
  return(output)
  }


