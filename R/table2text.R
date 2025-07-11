#' table2text
#'
#' Parses tabled content from HTML coded content or HTML, DOCX or PDF file to text.
#' @param x A vector with HTML tables or a single file path to an HTML, XML, CERMXML, HML, PDF or DOCX file..
#' @param unifyMatrix Logical. If TRUE, matrix cells are unifiedfor better post processing.
#' @param unifyStats Logical. If TRUE, output is unified for better post processing (e.g.: "p-value"->"p").
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footer.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parantheses.
#' @param addDF Logical. If TRUE, detected sample size N in caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, standard coding of p-values is assumed to be * p<.05, ** p<.01 and ***p<.001.
#' @param addTableName Logical. If TRUE, table number is added before the parsed text lines.
#' @importFrom JATSdecoder letter.convert
#' @return A List with parsed table content per HTML table. The result vector in each list element can be further processed with standardStats() to extract and structure the statistical standard test results only.
#' @export

table2text<-function(x,
                     unifyMatrix=TRUE,
                     unifyStats=FALSE,
                     expandAbbreviations=TRUE,
                     superscript2bracket=TRUE,
                     standardPcoding=FALSE,
                     addDF=TRUE,
                     addTableName=TRUE
){
  # preparation/escapes
  if(is.list(x))
    stop("x must be a vector of HTML tables or a single file path to an HTML, XML, CERMXML, HML, PDF or DOCX file.")
  if(length(x)==0)  return(NULL)
  
  caption<-NULL;footer<-NULL;legend<-list();m<-NULL
  
  # get tables as matrix if is file
  if(length(x)==1 & file.exists(x[1])){ 
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
      x<-get.tables(x)
    }
    
    }# end if is file
  
  # for HTML tables with legend text
  if(!is.list(x)&!is.matrix(x))
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
  
  }
  
  # prepare and escape if no matrix extracted
  if(is.matrix(m)) m<-list(m)
  if(!is.list(m)) return(NULL)
  
  # set legend to empty, if non is existant
  if(length(legend)==0) legend[1:length(m)]<-""
  
  fun<-function(x,
                unifyMatrix=TRUE,
                unifyStats=FALSE,
                expandAbbreviations=TRUE,
                superscript2bracket=superscript2bracket,
                standardPcoding=FALSE,addDF=addDF,
                legend=NULL){
    
    if(length(x)==0)  return(NULL)
    if(unifyMatrix==TRUE) x<-unifyMatrixContent(x)

  # convert matrix to text
  out<-matrix2text(x,legend=legend,
                   expandAbbreviations=expandAbbreviations,
                   superscript2bracket=superscript2bracket,
                   unifyMatrix=FALSE,standardPcoding=standardPcoding,addDF=addDF
                   )
  if(unifyStats==TRUE) out<-unifyStats(out)
  # output
  return(out)
  }

  
  # apply function
#  output<-lapply(m,fun,
#                 unifyMatrix=unifyMatrix,
#                 unifyStats=unifyStats,
#                 expandAbbreviations=expandAbbreviations,
#                 standardPcoding=standardPcoding,addDF=addDF)
  
if(length(m)==0) return(NULL)
  
  output<-list()
  for(i in 1:length(m)) 
    output[[i]]<-fun(m[[i]],unifyMatrix=unifyMatrix,
                      unifyStats=unifyStats,
                      expandAbbreviations=expandAbbreviations,
                     superscript2bracket=superscript2bracket,
                      standardPcoding=standardPcoding,addDF=addDF,
                      legend=legend[[i]])
  
  
  if(length(output)==0) return(NULL)
  # name the output
  if(addTableName==TRUE)  names(output)<-paste("table",1:length(output))
  #output<-mapply(c,output)
  
  return(output)
  }


