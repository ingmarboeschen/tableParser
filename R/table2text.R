#' table2text
#'
#' Parses tabled content from HTML coded content or HTML, DOCX or PDF file to human readable text vector. Before parsing, header lines are collapsed and connected cells are broken up.
#' @param x A vector with HTML tables or a single file path to an HTML, XML, CERMXML, HML, PDF or DOCX file.
#' @param unifyMatrix Logical. If TRUE, matrix cells are unified for better post processing.
#' @param unifyStats Logical. If TRUE, output is unified for better post processing (e.g.: "p-value"->"p").
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footer.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param addDF Logical. If TRUE, detected sample size N in caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, standard coding of p-values is assumed to be * p<.05, ** p<.01 and ***p<.001.
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param correctComma Logical. If TRUE and unifyMatrix=TRUE, decimal sign commas are converted to dots. 
#' @param addDescription Logical. If TRUE, table caption and footer are added before the extracted table content for better readability.
#' @param unlist Logical. If TRUE, output is returned as vector.
#' @param addTableName Logical. If TRUE and unlist=TRUE, table number is added in front of unlisted text lines.
#' @importFrom JATSdecoder letter.convert
#' @return List with parsed tabled content as elements. The text vector in each list element can be further processed with JATSdecoder::standardStats() to extract and structure the statistical standard test results.
#' @export

table2text<-function(x,
                     unifyMatrix=TRUE,
                     unifyStats=FALSE,
                     expandAbbreviations=TRUE,
                     superscript2bracket=TRUE,
                     standardPcoding=FALSE,
                     addDF=TRUE,
                     rotate=FALSE,
                     correctComma=FALSE,
                     addDescription=TRUE,
                     unlist=FALSE,addTableName=TRUE
){
  # preparation/escapes
  if(is.list(x)|is.matrix(x))
    stop("x must be a vector of HTML tables or a single file path to an HTML, XML, CERMXML, HML, PDF or DOCX file.")
  if(length(x)==0)  return(NULL)

  caption<-NULL;footer<-NULL;legend<-list();m<-NULL;file<-FALSE
  
  # get tables as matrix if x is file
  if(length(x)==1 & file.exists(x[1])){ 
    file<-TRUE
    # get file type
    type<-tolower(gsub(".*\\.([A-z][A-z]*)$","\\1",x))
    # escape
    if(!is.element(type,c("cermxml","xml","html","hml","pdf","docx"))){
      stop("Input file format must be either HTML, XML, CERMXML, HML, PDF or DOCX.")
    }
    # docx 
    if(is.element(type,c("docx"))){
      m<-docx2matrix(x)
    }
    # pdf
    if(type=="pdf"){
      x<-tabulapdf::extract_tables(x,output="matrix")
      m<-lapply(x,as.matrix)
    }
    # HTML 
    if(is.element(type,c("cermxml","xml","html","hml"))){
      x<-get.HTML.tables(x)
      
    }
    if(length(x)==0)  return(NULL)
    
    }# end if is file
  
  # for HTML tables with legend text
  if(!is.list(x)&!is.matrix(x)){
  if(length(grep("<table",x[1]))>0){
    # split multiple tables inside of one <table-wrap>-tag
    x<-multiTable(x)
    # extract and combine legend text, then extract codings
    caption<-lapply(x,get.caption)
    footer<-lapply(x,get.footer)
    for(i in 1:length(x)){
#      temp<-legendCodings(c(caption[[i]],footer[[i]]))
#      if(length(temp)==0) temp<-legendCodings("")
#      legend[[i]]<-temp
      legend[[i]]<-c(caption[[i]],footer[[i]])
    }
    
    # convert HTML tables to matrix
    m<-table2matrix(x,letter.convert=TRUE,rm.html=TRUE,replicate=TRUE,
                    collapseHeader=TRUE)
  
  }else{
    if(!file.exists(x[1])) 
      stop("x must be a vector of HTML tables or a single file path to an HTML, XML, CERMXML, HML, PDF or DOCX file.")
  }
  }
  
  # prepare and escape if no matrix extracted
  if(is.matrix(m)) m<-list(m)
  if(!is.list(m)) return(NULL)
  
  # set legend to empty, if non is existent
  if(length(legend)==0) legend[1:length(m)]<-""

  # function to convert matrix with matrix2text
  fun<-function(x,
                unifyMatrix=TRUE,unifyStats=FALSE,
                expandAbbreviations=TRUE,superscript2bracket=TRUE,
                standardPcoding=FALSE,addDF=TRUE,
                rotate=FALSE,correctComma=FALSE,
                legend=NULL,unlist=FALSE){
    
    if(length(x)==0)  return(NULL)
    if(unifyMatrix==TRUE) x<-unifyMatrixContent(x,correctComma=correctComma)
    if(length(x)==0) return(NULL)
    
    # convert matrix to text
     out<-matrix2text(x,legend=legend,
                   expandAbbreviations=expandAbbreviations,
                   superscript2bracket=superscript2bracket,
                   unifyMatrix=FALSE,standardPcoding=standardPcoding,
                   addDF=addDF,
                   rotate=rotate,unlist=unlist
                   )
     
  if(!is.list(out)&unifyStats==TRUE) out<-unifyStats(out)
  if(is.list(out)&unifyStats==TRUE) out<-lapply(out,unifyStats)
     # output
  return(out)
  }

  # escape
  if(length(m)==0) return(NULL)
  
  output<-list()
  for(i in 1:length(m)) 
    output[[i]]<-unname(unlist(fun(m[[i]],unifyMatrix=unifyMatrix,
                      unifyStats=unifyStats,
                      expandAbbreviations=expandAbbreviations,
                      superscript2bracket=superscript2bracket,
                      standardPcoding=standardPcoding,addDF=addDF,
                      correctComma=correctComma,rotate=rotate,
                      legend=legend[[i]],unlist=FALSE)))
  
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


