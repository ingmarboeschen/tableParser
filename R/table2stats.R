#' table2stats
#' 
#' Extracts tabulated statistical results from scientific articles in XML, HTML, HML, DOCX or PDF format.
#' @param x Input. Either a file path to an XML, HTML, HML, DOCX or PDF file or matrix object or vector of plain HTML coded tables.
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, then standard coding of p-values is assumed to be * p<.05, ** p<.01 and ***p<.001.
#' @param correctComma Logical. If TRUE, decimal sign commas are converted to dots. 
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footer.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param stats.mode Select a subset of test results by p-value checkability for output. One of: c("all", "checkable", "computable", "uncomputable").
#' @param checkP Logical. If TRUE, detected p-values and recalculated p-values will be checked for consistency
#' @param alpha Numeric. Defines the alpha level to be used for error assignment.
#' @param criticalDif Numeric. Sets the absolute maximum difference in reported and recalculated p-values for error detection.
#' @param alternative Character. Select test sidedness for recomputation of p-values from t-, r- and beta-values. One of c("undirected", "directed"). If "directed" is specified, p-values for directed null-hypothesis are added to the table but still require a manual inspection on consistency of the direction.
#' @param estimateZ Logical. If TRUE, detected beta-/d-values are divided by the reported standard error "SE" to estimate Z-values ("Zest") for observed beta/d and computation of p-values. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE, capital letter T is treated as t-statistic.
#' @param rm.na.col Logical. If TRUE, removes all columns with only NA.
#' @param addTableName Logical. If TRUE, table number is added in front of the eaxtracted results. 
#' @importFrom tabulapdf extract_tables
#' @importFrom JATSdecoder letter.convert
#' @importFrom JATSdecoder standardStats
#' @importFrom JATSdecoder pCheck
#' @return A data.frame object with the extracted statistical standard results and recalculated p-values and a rudimentary, optional consistency check for reported p-values (if 'checkP=TRUE'). 
#' @export

table2stats<-function(x,
                      standardPcoding=FALSE,
                      correctComma=FALSE,
                      expandAbbreviations=TRUE,
                      superscript2bracket=TRUE,
                      stats.mode="all",
                      checkP=FALSE,
                      alpha=.05,
                      criticalDif=.02,
                      alternative="undirected",
                      estimateZ=FALSE,
                      T2t=FALSE,
                      addTableName=TRUE,
                      rm.na.col=TRUE
                      ){
  
  # prechecks
  if(!is.element(stats.mode,c("all", "checkable", "computable", "uncomputable"))) stop('Argument "stats.mode" must be either "all", "checkable", "computable", or "uncomputable".')
  if(!is.element(alternative,c("undirected", "directed"))) stop('Argument "alternative" must be either "undirected", or "directed".')
  
  
  raw<-NULL;stats<-NULL;type<-NULL;legend<-list()
  if(length(x)==0) return(NULL)
  # if x is one file
  if(!is.matrix(x)&!is.list(x))
    if(length(x)==1 & sum(file.exists(x[1]))>0){
    # get file type
    type<-tolower(gsub(".*\\.([A-z][A-z]*)$","\\1",x))
    
    # escape
    if(!is.element(type,c("cermxml","xml","html","hml","pdf","docx"))){
      stop("Input file format must be either HTML, XML, CERMXML, HML, PDF or DOCX.")
    }  
  
  # HTML 
    if(is.element(type,c("cermxml","xml","html","hml"))){
      tabs<-get.tables(x)
      
      text<-table2text(tabs,unifyMatrix=TRUE,
                       expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                       superscript2bracket=superscript2bracket,
                       standardPcoding=standardPcoding,addDescription=FALSE)
    }

    # PDF 
      if(type=="pdf"){
        tabs<-tabulapdf::extract_tables(x,output="matrix")
        tabs<-lapply(tabs,as.matrix)
        text<-lapply(tabs,matrix2text,
                     unifyMatrix=TRUE,
                     expandAbbreviations=expandAbbreviations,correctComma=correctComma,,
                     superscript2bracket=superscript2bracket,
                     standardPcoding=standardPcoding)
      }
  # DOCX 
      if(type=="docx"){
#        y<-docxtractr::read_docx(x)
#        tabs<-docxtractr::docx_extract_all_tbls(y,guess_header=F)
#        tabs<-lapply(tabs,as.matrix)
        tabs<-docx2matrix(x,replicate=TRUE)
        text<-lapply(tabs,matrix2text,
                     unifyMatrix=TRUE,
                     expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                     superscript2bracket=superscript2bracket,
                     standardPcoding=standardPcoding)
      }
  }# end if(file.exist)
  
  #######################
  ## if x is not a file
  if(is.null(type)){  
    # for lists
    if(is.list(x)){
       if(is.matrix(x[[1]])) text<-lapply(x,matrix2text, 
                                          unifyMatrix=TRUE,correctComma=correctComma,
                                          expandAbbreviations=expandAbbreviations,
                                          superscript2bracket=superscript2bracket,
                                          standardPcoding=standardPcoding)
       if(is.vector(x[[1]])){
         tabs<-get.tables(unlist(x))
         text<-table2text(tabs,
                          unifyMatrix=TRUE,
                          expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                          superscript2bracket=superscript2bracket,
                          standardPcoding=standardPcoding,addDescription=FALSE)
       }
    } 
    # for matrices
    if(is.matrix(x)) text<-matrix2text(x,
                                       unifyMatrix=TRUE,
                                       expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                                       superscript2bracket=superscript2bracket,
                                       standardPcoding=standardPcoding)
    # vector with HTML tables
    if(is.vector(x)&!is.matrix(x)&!is.list(x)){
      tabs<-get.tables(x)
      text<-table2text(tabs,
                       unifyMatrix=TRUE,
                       expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                       superscript2bracket=superscript2bracket,
                       standardPcoding=standardPcoding,addDescription=FALSE)
    }
  } # end if is no file

  # name the output
  if(length(text)>0&is.list(text)) names(text)<-paste("table",1:length(text))
  # add table name
  if(isTRUE(addTableName)){
  name<-names(text)
  if(length(name)>0) for(i in 1:length(name)) text[[i]]<-paste0("unified standard stats in ",name[i],": ",text[[i]])
  }
  
  # get copy with results for output
  raw<-unname(unlist(text))
  raw<-grep("[<=>][-\\.0-9][\\.0-9]*",raw,value=TRUE)
  
  # unlist and unify stats
  text<-unname(unlist(lapply(text,unifyStats)))
  
  # handle df1 and df2 in ANOVAs
  text<-unlist(lapply(text,anovaHandler))
  
  # split text
  
  ###########################
  ### apply split functions to lines of parsed text
  text<-letter.convert(text,greek2text=TRUE)
  text<-splitLastStat(text)
  text<-splitCIs(text)
  #text<-splitTFZB(text)

  # extract standard results
  stats<-standardStats(text,
                       stats.mode=stats.mode,
                       checkP=checkP,
                       alpha=alpha,
                       criticalDif=criticalDif,
                       alternative=alternative,
                       estimateZ=estimateZ,
                       T2t=T2t,
                       rm.na.col=rm.na.col)
#  return(list(parsedStats=raw,standardStats=stats))
  return(stats)
}

