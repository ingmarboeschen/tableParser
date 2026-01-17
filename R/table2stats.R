#' table2stats
#' 
#' Extracts tabulated statistical results from documents in XML, HTML, HML, DOCX, or PDF format.
#' @param x Input. Either a file path to an XML, HTML, HML, DOCX, or PDF file; or a matrix object; or a vector of plain HTML-coded tables.
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, then standard coding of p-values is assumed to be * p<.05, ** p<.01, and ***p<.001.
#' @param noSign2p Logical. If TRUE, imputes 'p>maximum of coded p-values' to cells that are not coded to be significant.
#' @param correctComma Logical. If TRUE, decimal sign commas are converted to dots. 
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footer.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param stats.mode Select a subset of test results by p-value checkability for output. One of: c("all", "checkable", "computable", "uncomputable").
#' @param checkP Logical. If TRUE, detected p-values and recalculated p-values will be checked for consistency.
#' @param alpha Numeric. Defines the alpha level to be used for error assignment.
#' @param criticalDif Numeric. Sets the absolute maximum difference in reported and recalculated p-values for error detection.
#' @param alternative Character. Select test sidedness for recomputation of p-values from t-, r-, and beta-values. One of c("undirected", "directed"). If "directed" is specified, p-values for directed null hypotheses are added to the table but still require a manual inspection of the consistency of the direction.
#' @param estimateZ Logical. If TRUE, detected beta-/d-values are divided by the reported standard error "SE" to estimate Z-values ("Zest") for observed beta/d and computation of p-values. Note: This is only valid if Gauss-Markov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE, capital letter T is treated as a t-statistic.
#' @param dfHandling Logical. If TRUE, detected sample size N in the caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param rm.na.col Logical. If TRUE, removes all columns with only NA.
#' @param collapse Logical. If TRUE, the result is collapsed to a single data frame object. Else, a list of data frames with length = n matrices is returned.
#' @param addTableName Logical. If TRUE, the table number is added in front of the extracted results. 
#' @examples 
#' ## - Download example DOCX file
#' d<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx'
#' download.file(d,paste0(tempdir(),"/","tableExamples.docx"))
#' 
#' # Extract the detected statistical standard results and validate the reported and coded 
#' # p-values with the recalculated p-values.
#' table2stats(paste0(tempdir(),"/","tableExamples.docx"), checkP=TRUE, estimateZ=TRUE)
#' 
#' ## - Download example HTML file
#' h<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.html'
#' download.file(h,paste0(tempdir(),"/","tableExamples.html"))

#' # Extract the detected statistical standard results and validate the reported and coded 
#' # p-values with the recalculated p-values.
#' table2stats(paste0(tempdir(),"/","tableExamples.html"), checkP=TRUE, estimateZ=TRUE)

#' # - Download example PDF file
#' \donttest{
#' p<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.pdf'
#' download.file(p,paste0(tempdir(),"/","tableExamples.pdf"))
#' 
#' # Extract the detected statistical standard results and validate the reported and  
#' # standard coded as well as not coded p-values with the recalculated p-values.
#' table2stats(paste0(tempdir(),"/","tableExamples.pdf"), checkP=TRUE, estimateZ=TRUE, 
#' standardPcoding=TRUE, noSign2p=FALSE)
#' }
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
                      dfHandling=TRUE,
                      collapse=TRUE,
                      addTableName=FALSE,
                      rm.na.col=TRUE
                      ){
  
  # prechecks
  if(!is.element(stats.mode,c("all", "checkable", "computable", "uncomputable"))) stop('Argument "stats.mode" must be either "all", "checkable", "computable", or "uncomputable".')
  if(!is.element(alternative,c("undirected", "directed"))) stop('Argument "alternative" must be either "undirected", or "directed".')
  # prepare objects
  raw<-NULL;stats<-NULL;type<-NULL
  if(length(x)==0) return(NULL)
  
  ### convert tables to text
  text<-uniqueWarnings(table2text(x, decodeP=TRUE,noSign2p=noSign2p,na.rm=TRUE,
                              unifyMatrix=TRUE,correctComma=correctComma,
                              expandAbbreviations=expandAbbreviations,
                              superscript2bracket=superscript2bracket,bracketHandling=TRUE,
                              standardPcoding=standardPcoding,rotate=rotate,
                              unlist=TRUE,addTableName=FALSE,dfHandling=dfHandling,addDescription=FALSE))
  
  # name the output
  if(length(text)>0&is.list(text)) names(text)<-paste("Unified standard stats in Table",1:length(text))
  # add table name to result text
  if(isTRUE(addTableName)){
   name<-names(text)
   if(length(name)>0) for(i in 1:length(name)) text[[i]]<-paste0("Unified standard stats in ",name[i],": ",text[[i]])
  }
  
  
  ########### 
  ## extract statistics
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
  if(isTRUE(dfHandling))
    text<-unname(unlist(lapply(text,anovaHandler)))
  
  ###########################
  text<-JATSdecoder::letter.convert(text,greek2text=TRUE)
  
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
  stats<-JATSdecoder::standardStats(text,
                       stats.mode=stats.mode,
                       checkP=checkP,
                       alpha=alpha,
                       criticalDif=criticalDif,
                       alternative=alternative,
                       estimateZ=estimateZ,
                       T2t=T2t,
                       rm.na.col=rm.na.col)
  
  # remove lines with only beta, or d values
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

  # apply fun to extract statistics
  if(isTRUE(collapse)) out<-uniqueWarnings(fun(text))
  if(!isTRUE(collapse)) out<-uniqueWarnings(lapply(text,fun))
  # output
  return(out)
  }


