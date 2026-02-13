#' table2stats
#' 
#' Extracts tabulated statistical results from documents in XML, HTML, HML, DOCX, or PDF format. The tabled content is collapsed into a text string with table2text(), which is then processed with standardStats() from the 'JATSdecoder' package. It detects most standard statistics (t, Z, chi^2, F, r, d, beta, SE, r, d, eta^2, omega^2, OR, RR, p-values), decodes encoded p-values to text and recalculates and checks p-values if possible.
#' @param x Input. Either a file path to an XML, HTML, HML, DOCX, or PDF file; or a matrix object; or a vector of plain HTML-coded tables.
#' @param standardPcoding Logical. If TRUE, and no other detection of coding is detected, then standard coding of p-values is assumed to be * for p<.05, ** for p<.01, and *** for p<.001.
#' @param noSign2p Logical. If TRUE, imputes 'p>maximum of coded p-values' to cells that are not coded to be significant.
#' @param checkP Logical. If TRUE, detected p-values and recalculated p-values will be checked for consistency.
#' @param alpha Numeric or "auto". Defines the alpha level to be used for error assignment. If set to "auto", table notes are screened for reports of alpha levels, 1-alpha confidence intervals and correction procedures for multiple testing. If no reported alpha levels is detected, the value is set to the widely used standard 'alpha=.05'.
#' @param criticalDif Numeric. Sets the absolute maximum difference in reported and recalculated p-values for error detection.
#' @param alternative Character. Select test sidedness for recomputation of p-values from t-, r-, and beta-values. One of c("undirected", "directed"). If "directed" is specified, p-values for directed null hypotheses are added to the table but still require a manual inspection of the consistency of the direction.
#' @param estimateZ Logical. If TRUE, detected beta-/d-values are divided by the reported standard error "SE" to estimate Z-values ("Zest") for observed beta/d and computation of p-values. Note: This is only valid if Gauss-Markov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param correctComma Logical. If TRUE, decimal sign commas are converted to dots. 
#' @param T2t Logical. If TRUE, capital letter T is treated as a t-statistic.
#' @param dfHandling Logical. If TRUE, detected sample size N in the caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @param stats.mode Select a subset of test results by p-value checkability for output. One of: c("all", "checkable", "computable", "uncomputable").
#' @param collapse Logical. If TRUE, the result is collapsed to a single data frame object. Else, a list of data frames with length = n matrices is returned.
#' @param rotate Logical. If TRUE, matrix content is parsed by column.
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label from table caption/footer.
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param rm.na.col Logical. If TRUE, removes all columns with only NA.
#' @param addTableName Logical. If TRUE, the table number is added in front of the extracted results, when collapsed to a single data frame with 'collapse=TRUE'. 
#' @seealso \code{\link[JATSdecoder]{get.stats}} for extracting statistical results from textual resources.
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
#' # Note: Due to the messy table extraction with tabulapdf::extract_tables(), the  
#' # extraction of the statistical results is less precise here.
#' }
#' @importFrom tabulapdf extract_tables
#' @importFrom JATSdecoder letter.convert
#' @importFrom JATSdecoder standardStats
#' @importFrom JATSdecoder get.multi.comparison
#' @importFrom JATSdecoder get.alpha.error
#' @importFrom JATSdecoder pCheck
#' @return A data.frame object with the extracted statistical standard results, recalculated p-values and a rudimentary, optional consistency check for reported p-values (if 'checkP=TRUE'). 
#' @export

table2stats<-function(x,
                      standardPcoding=FALSE,
                      checkP=FALSE,
                      noSign2p=FALSE,
                      criticalDif=.02,
                      alternative="undirected",
                      estimateZ=FALSE,
                      T2t=FALSE,
                      correctComma=TRUE,
                      stats.mode="all",
                      alpha="auto",
                      dfHandling=TRUE,
                      collapse=TRUE,
                      addTableName=TRUE,
                      rotate=FALSE,
                      expandAbbreviations=TRUE,
                      superscript2bracket=TRUE,
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
                              unifyStats=TRUE,
                              expandAbbreviations=expandAbbreviations,
                              superscript2bracket=superscript2bracket,bracketHandling=TRUE,
                              standardPcoding=standardPcoding,rotate=rotate,
                              unlist=FALSE,addTableName=FALSE,dfHandling=dfHandling,
                              addDescription=TRUE)
                       )
  o_name<-paste("Unified standard stats in ",names(text))
  # rename the output
  if(length(text)>0&is.list(text)) names(text)<-o_name #paste("Unified standard stats in Table",1:length(text))

    
  ########### 
  ## extract statistics
  fun<-function(text,alpha){
  correctionWarn<-NULL;alphaMinWarn<-NULL;alphaWarn<-NULL
    
  legendText<-unname(grep("^caption:|^footer:",unlist(text),value=TRUE))
  # has report of correction method?
  correction<-JATSdecoder::get.multi.comparison(legendText)
  alphaCorrected<-JATSdecoder::get.alpha.error(legendText)$corrected_alpha
  # report warning
  if(length(correction)>0)#&length(alphaCorrected)==0)
    correctionWarn<-(paste0(paste(correction,collapse=", ")," correction for multiple testing was detected. If the reported p-values were corrected, a comparison with the uncorrected p-values calculated here and the uncorrected alpha level may not be appropiate."))

  if(alpha=="auto"){
  alphaMin<-JATSdecoder::get.alpha.error(legendText)$alpha_min
  alphaMin<-alphaMin[!is.na(alphaMin)]
  
  if(length(alphaCorrected)>0)
    if(isTRUE(alphaMin<alphaCorrected))
      alphaMinWarn<-("The detected minimal alpha level is lower than the detected corrected alpha level. The lower alpha level is used for checks.")
  
  alpha<-alphaMin
  
  if(length(alpha)==0|isTRUE(alpha>.2)|isTRUE(alpha<=0)) {
    alpha<-.05
    alphaWarn<-("No adequate alpha level detected. The checks are performed on the standard alpha=.05, if possible.")
    }
  }
  
  # only parsed text 
  text<-grep("^caption:|^footer:",text,value=TRUE,invert=TRUE)
  # get copy with results for output
  raw<-unname(unlist(text))
  raw<-grep("[<=>] *-*[\\.0-9][\\.0-9]*",raw,value=TRUE)
  
  # convert ns to p>.05
  if(isTRUE(standardPcoding)) text<-gsub("([0-9])*[,:;\\^] *[Nn]\\.*[Ss]\\.*$","\\1;; p>.05",text)
  if(isTRUE(standardPcoding)) text<-gsub("([0-9])*[,:;\\^] *[Nn]\\.*[Ss]\\.*([,:; ])","\\1;; p>.05\\2",text)
  
  # remove space around operators
  text<-gsub(" *([<=>][<=>]*) *(-*[\\.0-9])","\\1\\2",text)        
  
  # remove num=text
  text<-gsub("[0-9][0-9]*[<=>][<=>]*[A-z][A-z]*","",text)
  
  #remove brackets if is followed by letter-anything except results
  text<-gsub(" \\(([A-z][^\\)]*)\\)([^<=>])"," \\1\\2",text)
  text<-gsub(" \\[([A-z][^]]*)\\]([^<=>])"," \\1\\2",text)
  
  ###########################
  text<-JATSdecoder::letter.convert(text,greek2text=TRUE)
  
  # replace codedP-p-codedP -> p-codedP
  text<-gsub(";; p[<=>][<=>]*[\\.0][\\.0-9]*(,[ A-z]* [pP][<=>][<=>]*[\\.0][\\.0-9]*;; p[<=>][<=>]*[\\.0][\\.0-9]*)","\\1",text)
  text<-gsub(";; p[<=>][<=>]*[\\.0][\\.0-9]*(,[ A-z]* [pP]\\(.*\\)[<=>][<=>]*[\\.0][\\.0-9]*;; p[<=>][<=>]*[\\.0][\\.0-9]*)","\\1",text)

  ### splitting of lines
  # before ", ... b=" in model tables with "b=" more than once
  if(length(grep("[Cc]onstant|[Ii]ntercept| [Mm]odel[^a-z]|[ -]R\\^*2|^R\\^*2|AIC|BIC|nformation [Cc]riter",text))>0 & 
     length(grep(" [bB][<=>]| [Bb]eta[<=>]",text))>0)
        text<-splitBeta(text)

  text<-splitLastStat(text)
  text<-splitCIs(text)
  
  # split after coded p if no result is in fromt
  while(length(grep("^[^<=>]*;; p[<=>][<=>]*[\\.0][0-9\\.]*[^0-9\\.] ",text))>0)
         text<-gsub("","",unlist(strsplit(gsub("^([^<=>]*;; p[<=>][<=>]*[\\.0][0-9\\.]*)([^0-9\\.] )","\\1SPLITHERE\\2",text),"SPLITHERE")))
  # remove punctuation in front
  text<-gsub("^[[:punct:]] ","",text)
  
  # set df1 and df2 in lines with F-values
  i<-grep(" F(<=>)[0-9\\.]",text)
  text[i]<-gsub("df[^=]*(=[0-9][\\.0-9]*)(, .*)df[^=]*(=[0-9][\\.0-9]*)","df1\\1\\2df2\\3",text[i])
  
  # if has N=num, replace df in [rt](df)=num -> [rt](N-2)=num
  text<-unlist(lapply(text,correctDF))
  
  #text<-splitTFZB(text)
  text<-unifyStats(text)

  # extract standard results
  stats<-JATSdecoder::standardStats(text,
                       stats.mode=stats.mode,
                       checkP=checkP,
                       alpha=alpha,
                       criticalDif=criticalDif,
                       alternative=alternative,
                       estimateZ=estimateZ,
                       T2t=T2t,
                       rm.na.col=FALSE)
  
  # add raw input
  
  # remove lines with only beta, or d or t values
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
  if(length(stats)>0){
    if(sum(!is.na(stats$t))>0){
      tval<-!is.na(stats$t)
      keep<-rowSums(!is.na(stats))>2|!tval
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
  
  if(length(stats)==0) return(stats)

  
  if(isTRUE(checkP)){
  # add alpha as column
  stats$alpha4check<-alpha
  # add correction method as column?
  stats$correction_meth<-paste(correction,collapse=", ")
  
  # set to NA if no recalucaltedP
  stats$alpha4check[is.na(stats$recalculatedP)]<-NA
  stats$correction_meth[is.na(stats$recalculatedP)]<-NA
  # return captured warnings, if recalculatedP is present
  if(sum(!is.na(stats$recalculatedP))>0){
    if(!is.null(correctionWarn)) warning(correctionWarn)
    if(!is.null(alphaMinWarn)) warning(alphaMinWarn)
#    if(!is.null(alphaWarn)) warning(alphaWarn)
   }
  }
  
  return(stats)
  }

  # apply fun to extract statistics
  #if(isFALSE(collapse)) 
  out<-uniqueWarnings(lapply(text,fun,alpha=alpha))
  
  #if(isTRUE(collapse)) out<-uniqueWarnings(fun(unlist(text),alpha=alpha))
  
  # unlist   
  if(length(out)>0)
  if(isTRUE(collapse)){
    times<-lapply(out,function(x) ifelse(isTRUE(nrow(x)>0),nrow(x),0))
    tab<-gsub(".*Table ","",rep(names(out),times))
    temp<-NULL
    for(i in 1:length(out)) if(length(out[[i]])>0) temp<-rbind(temp,out[[i]])
    out<-temp
    if(isTRUE(nrow(out)>0)) out<-data.frame(Table=tab,out)
    
  }
  
  # remove columns with only NA
  if(length(out)>0)
    if(isTRUE(rm.na.col)){
      if(isTRUE(collapse)) out<-noNA(out)
      if(isFALSE(collapse)) out<-lapply(out,noNA)
    }
      
  # output
  return(out)
  }

# remove columns with only NA
noNA<-function(x){
  if(!is.data.frame(x)) return(x)
  if(nrow(x)==0) return(x)
  i<-which(colSums(is.na(x))==nrow(x))
  # exclude result and recalculated p and following
  i<-i[i>1]
  i<-i[i<grep("recalculatedP",names(x))]
  x<-x[,-i]
  return(x)
}