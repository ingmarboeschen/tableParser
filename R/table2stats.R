#' table2stats
#' 
#' Extracts tabulated statistical results from documents in XML, HTML, HML, DOCX or PDF format.
#' @param x Input. Either a file path to an XML, HTML, HML, DOCX or PDF file or matrix object or vector of plain HTML coded tables.
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, then standard coding of p-values is assumed to be * p<.05, ** p<.01 and ***p<.001.
#' @param noSign2p Logical. If TRUE, imputes 'p>maximum of coded p-values' to cells that are not coded to be significant.
#' @param correctComma Logical. If TRUE, decimal sign commas are converted to dots. 
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footer.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param stats.mode Select a subset of test results by p-value checkability for output. One of: c("all", "checkable", "computable", "uncomputable").
#' @param checkP Logical. If TRUE, detected p-values and recalculated p-values will be checked for consistency.
#' @param alpha Numeric. Defines the alpha level to be used for error assignment.
#' @param criticalDif Numeric. Sets the absolute maximum difference in reported and recalculated p-values for error detection.
#' @param alternative Character. Select test sidedness for recomputation of p-values from t-, r- and beta-values. One of c("undirected", "directed"). If "directed" is specified, p-values for directed null-hypothesis are added to the table but still require a manual inspection on consistency of the direction.
#' @param estimateZ Logical. If TRUE, detected beta-/d-values are divided by the reported standard error "SE" to estimate Z-values ("Zest") for observed beta/d and computation of p-values. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE, capital letter T is treated as t-statistic.
#' @param addDF Logical. If TRUE, detected sample size N in caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param rm.na.col Logical. If TRUE, removes all columns with only NA.
#' @param collapse Logical. If TRUE, the result is collapsed to a single data.frame object. Else, a list of data.frames with length = n matrices is returned.
#' @param addTableName Logical. If TRUE, table number is added in front of the eaxtracted results. 
#' @importFrom tabulapdf extract_tables
#' @importFrom JATSdecoder letter.convert
#' @importFrom JATSdecoder standardStats
#' @importFrom JATSdecoder pCheck
#' @return A data.frame object with the extracted statistical standard results, recalculated p-values and a rudimentary, optional consistency check for reported p-values (if 'checkP=TRUE'). 
#' @export

table2stats<-function(x,
                      standardPcoding=FALSE,
                      noSign2p=TRUE,
                      correctComma=FALSE,
                      rotate=FALSE,
                      expandAbbreviations=TRUE,
                      superscript2bracket=TRUE,
                      stats.mode="all",
                      checkP=FALSE,
                      alpha=.05,
                      criticalDif=.02,
                      alternative="undirected",
                      estimateZ=FALSE,
                      T2t=FALSE,
                      addDF=TRUE,
                      collapse=TRUE,
                      addTableName=FALSE,
                      rm.na.col=TRUE
                      ){
  
  # prechecks
  if(!is.element(stats.mode,c("all", "checkable", "computable", "uncomputable"))) stop('Argument "stats.mode" must be either "all", "checkable", "computable", or "uncomputable".')
  if(!is.element(alternative,c("undirected", "directed"))) stop('Argument "alternative" must be either "undirected", or "directed".')
  # prepare objects
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
      tabs<-get.HTML.tables(x)
      
      text<-uniqueWarnings(table2text(tabs,unifyMatrix=TRUE,na.rm=TRUE,
                       expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                       superscript2bracket=superscript2bracket,rotate=rotate,noSign2p=noSign2p,
                       standardPcoding=standardPcoding,addDescription=FALSE,addDF=addDF))
    }

    # PDF 
      if(type=="pdf"){
        tabs<-tabulapdf::extract_tables(x,output="matrix")
        tabs<-lapply(tabs,as.matrix)
        text<-uniqueWarnings(lapply(tabs,matrix2text,
                     unifyMatrix=TRUE,noSign2p=noSign2p,na.rm=TRUE,
                     expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                     superscript2bracket=superscript2bracket,
                     standardPcoding=standardPcoding,rotate=rotate,unlist=TRUE,addTableName=FALSE,addDF=addDF))
      }
  # DOCX 
      if(type=="docx"){

        text<-uniqueWarnings(table2text(x,unifyMatrix=TRUE,na.rm=TRUE,
                         expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                         superscript2bracket=superscript2bracket,rotate=rotate,noSign2p=noSign2p,
                         standardPcoding=standardPcoding,addDescription=FALSE,addDF=addDF))
        # or
        #tabs<-docx2matrix(x,replicate=TRUE)
        # text<-lapply(tabs,matrix2text,
      #               unifyMatrix=TRUE,noSign2p=noSign2p,
      #               expandAbbreviations=expandAbbreviations,correctComma=correctComma,
      #               superscript2bracket=superscript2bracket,
      #               standardPcoding=standardPcoding,rotate=rotate,
      #               unlist=TRUE,addTableName=FALSE)
      }
  }# end if(file.exist)
  
  #######################
  ## if x is not a file
  if(is.null(type)){  
    # for lists
    if(is.list(x)){
       if(is.matrix(x[[1]])) text<-uniqueWarnings(lapply(x,matrix2text, noSign2p=noSign2p,na.rm=TRUE,
                                          unifyMatrix=TRUE,correctComma=correctComma,
                                          expandAbbreviations=expandAbbreviations,
                                          superscript2bracket=superscript2bracket,
                                          standardPcoding=standardPcoding,rotate=rotate,
                                          unlist=TRUE,addTableName=FALSE,addDF=addDF))
       if(is.vector(x[[1]])){
         tabs<-get.HTML.tables(unlist(x))
         text<-uniqueWarnings(table2text(tabs,
                          unifyMatrix=TRUE,noSign2p=noSign2p,na.rm=TRUE,
                          expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                          superscript2bracket=superscript2bracket,rotate=rotate,
                          standardPcoding=standardPcoding,addDescription=FALSE,addDF=addDF))
       }
    } 
    # for matrices
    if(is.matrix(x)) text<-uniqueWarnings(matrix2text(x,
                                       unifyMatrix=TRUE,noSign2p=noSign2p,na.rm=TRUE,
                                       expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                                       superscript2bracket=superscript2bracket,rotate=rotate,
                                       standardPcoding=standardPcoding,addDF=addDF))
    # vector with HTML tables
    if(is.vector(x)&!is.matrix(x)&!is.list(x)){
      tabs<-get.HTML.tables(x)
      text<-uniqueWarnings(table2text(tabs,
                       unifyMatrix=TRUE,noSign2p=noSign2p,na.rm=TRUE,
                       expandAbbreviations=expandAbbreviations,correctComma=correctComma,
                       superscript2bracket=superscript2bracket,rotate=rotate,
                       standardPcoding=standardPcoding,addDescription=FALSE,addDF=addDF))
    }
  } # end if is no file

  # name the output
  if(length(text)>0&is.list(text)) names(text)<-paste("Unified standard stats in Table",1:length(text))
  # add table name to result text
  if(isTRUE(addTableName)){
   name<-names(text)
   if(length(name)>0) for(i in 1:length(name)) text[[i]]<-paste0("Unified standard stats in ",name[i],": ",text[[i]])
  }
  
  
###########  
  fun<-function(text){
  # get copy with results for output
  raw<-unname(unlist(text))
  raw<-grep("[<=>] *[-\\.0-9][\\.0-9]*",raw,value=TRUE)
  
  # unlist and unify stats
  text<-unname(unlist(lapply(text,unifyStats)))
  
  # convert ns to p>.05
  if(standardPcoding==TRUE) text<-gsub("([0-9])*[,:;\\^] *[Nn]\\.*[Ss]\\.*$","\\1;; p>.05",text)
  if(standardPcoding==TRUE) text<-gsub("([0-9])*[,:;\\^] *[Nn]\\.*[Ss]\\.*([,:; ])","\\1;; p>.05\\2",text)
  
  # remove space around operators
  text<-gsub(" *([<=>][<=>]*) *","\\1",text)        
  
  # remove num=text
  text<-gsub("[0-9][0-9]*[<=>][<=>]*[A-z][A-z]*","",text)
  
  #remove brackets if is followed by letter-anything except results
  text<-gsub(" \\(([A-z][^\\)]*)\\)([^<=>])"," \\1\\2",text)
  text<-gsub(" \\[([A-z][^]]*)\\]([^<=>])"," \\1\\2",text)
  
  # handle df1 and df2 in ANOVAs
  if(isTRUE(addDF))
    text<-unname(unlist(lapply(text,anovaHandler)))
  
  ###########################
  text<-letter.convert(text,greek2text=TRUE)
  
  # replace codedP-p-codedP -> p-codedP
  # if has not p-codedP-p-codedP
  if(length(grep(" p[<=>][<=>]*[\\.0][\\.0-9]*;; p[<=>][<=>]*[\\.0][\\.0-9]*,[ A-z]* p[<=>][<=>]*[\\.0][\\.0-9]*;; p[<=>][<=>]*[\\.0][\\.0-9]*",text))==0){
    text<-gsub(";; p[<=>][<=>]*[\\.0][\\.0-9]*(,[ A-z]* [pP][<=>][<=>]*[\\.0][\\.0-9]*;; p[<=>][<=>]*[\\.0][\\.0-9]*)","\\1\\2",text)
    text<-gsub(";; p[<=>][<=>]*[\\.0][\\.0-9]*(,[ A-z]* [pP]\\(.*\\)[<=>][<=>]*[\\.0][\\.0-9]*;; p[<=>][<=>]*[\\.0][\\.0-9]*)","\\1\\2",text)
  }
  
  ### splitting of lines
  # before ", ... b=" in model tables with "b=" more than once
  if(length(grep("[Cc]onstant|[Ii]ntercept| [Mm]odel[^a-z]|[ -]R\\^*2|^R\\^*2|AIC|BIC|nformation [Cc]riter",text))>0 & 
     length(grep(" [bB][<=>]| [Bb]eta[<=>]",text))>0)
        text<-splitBeta(text)

  text<-splitLastStat(text)
  text<-splitCIs(text)
  
  # set df1 and df2 in lines with F-values
  i<-grep(" F(<=>)[0-9\\.]",text)
  text[i]<-gsub("df[^=]*(=[0-9][\\.0-9]*)(, .*)df[^=]*(=[0-9][\\.0-9]*)","df1\\1\\2df2\\3",text[i])
  
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
  # remove lines with only beta or d values
  if(length(stats)>0){
    if(sum(!is.na(stats$beta))>0){
      beta<-!is.na(stats$beta)
      keep<-rowSums(!is.na(stats))>2|!beta
      stats<-stats[keep,]
      if(nrow(stats)==0) stats<-NULL
    }
  }  
  if(length(stats)>0){
    if(sum(!is.na(stats$d))>0){
      d<-!is.na(stats$d)
      keep<-rowSums(!is.na(stats))>2|!d
      stats<-stats[keep,]
      if(nrow(stats)==0) stats<-NULL
    }
  }
  
  # remove empty captures (only result)
  if(length(stats)>0)
    if(ncol(stats)==1) 
      stats<-NULL
  if(length(stats)>0){
    keep<-rowSums(!is.na(stats))>=2
    stats<-stats[keep,]
    if(nrow(stats)==0) stats<-NULL
  }
  
    
    return(stats)
  }

  # apply fun 
  if(isTRUE(collapse)) out<-uniqueWarnings(fun(text))
  if(!isTRUE(collapse)) out<-uniqueWarnings(lapply(text,fun))
  # output
  return(out)
  }


