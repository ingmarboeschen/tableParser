#' unifyStats
#' Unifies textual representations of statististical results.
#' @param x A text string as vector.
#' @importFrom JATSdecoder letter.convert
#' @returns A unified text string.
#' @export
unifyStats<-function(x){
  stats<-letter.convert(x,greek2text=TRUE)
  # unify minus/hyphen sign
  x<-gsub("\u2212|\u02D7|\u002D|\u2013","-",x)
  # unify beta 
  x<-gsub("\u00DF|\u03b2","b",x)
  
  x<-text2num(x,percentage=FALSE,words=FALSE)
  
  # clean up white space
  stats<-gsub(" *([<=>]) *","\\1",stats)        
  # correct ", =letter"
  stats<-gsub(", *=([A-z])",", \\1",stats)
  # remove num-tailed
  stats<-gsub(" *\\(*2[- ]*tailed\\)* *"," ",stats)
  stats<-gsub(" *\\(*2[- ]*sided\\)* *"," ",stats)
  # unify CI:[num, num]    
  stats<-gsub("(\\.*[1-9][0-9]\\%*[ -]CI)[= ]*([-\\.0-9][\\.0-9]*[ ]*[^<=>0-9][ ]*[-\\.0-9][\\.0-9]*)(, [a-zA-Z]|$)","\\1: [\\2]\\3",stats)
  stats<-gsub("(\\.*[1-9][0-9]\\%*[ -]CI)[= ]*([-\\.0-9][\\.0-9]* to [-\\.0-9][\\.0-9]*)","\\1: [\\2]",stats)
  # value +-value
  stats<-gsub("\\(*([A-z][A-z]*) *\\+- *([A-z][A-z]*)\\)*=([-\\.0-9][\\.0-9]*) *\\+- *([-\\.0-9][\\.0-9]*)\\)*","\\1=\\3, \\2=+-\\4",stats)
  # 2 CIs to [CI]
  stats<-gsub("(\\.*[1-9][0-9]\\%*)[ -]*[Cc]onfidence [iI]nterval[^,]*=([-\\.0-9][\\.0-9]*), \\.*[1-9][0-9]\\%* [Cc]onfidence [Ii]nterval[^,]*=([-\\.0-9][\\.0-9]*)","\\1 CI: [\\2; \\3]",stats)
  stats<-gsub("[Cc]onfidence [iI]nterval([^a-z])","CI\\1",stats)
  stats<-gsub("%CI","% CI",stats)
  stats<-gsub("(\\.*[1-9][0-9]\\%*)[ -]*[Cc][Ii][^,]*=([-\\.0-9][\\.0-9]*), \\.*[1-9][0-9]\\%* [Cc][Ii][^,]*=([-\\.0-9][\\.0-9]*)","\\1 CI: [\\2; \\3]",stats)
  stats<-gsub("(% CI)[^=]*=(-*[0\\.][0\\.]*[0-9]*), [^%]*% CI[^=]*=(-*[0\\.][0\\.]*[0-9]*)","\\1: [\\2, \\3]",stats)
  # %=num ->num%
  stats<-gsub(" *(^/)\\%=([0-9\\.][0-9\\.]*)|^ *\\%=([0-9\\.][0-9\\.]*)","\\1\\2%",stats)
  # remove ": " at beginning
  stats<-gsub("^[:=] ","",stats)
  # unify p-value
  #stats<-gsub("[Ss]ignificance|p[[:punct:]]*[a-zA-Z][[:punct:]])","p",stats)
  stats<-gsub("[Vv]alues* of [Pp]([^a-z])","p\\1",stats)
  stats<-gsub(" [pP][- ][Vv]alues* *([0-9\\.])| [pP] *([0-9\\.])"," p=\\1\\2",stats)
  stats<-gsub("([^a-zA-Z]*)[pP][- ][Vv]alue([<>= ])","\\1p\\2",stats)
  stats<-gsub("[,:;] [Ss]ig[nifckazet \\.\\:]*([<=>])",", p\\1",stats)
  stats<-gsub(" P*([<=>])"," p\\1",stats)
  
  # Correlation
  stats<-gsub("[Cc]orr*elation *([<=>])","r\\1",stats)
  # remove "statistic"/"value
  stats<-gsub("[- ][Ss]tatistics*([<=>])","\\1",stats)
  stats<-gsub("[- ][Vv]alues*([<=>])","\\1",stats)
  # degrees of freedom
  stats<-gsub("[dD]eg[res\\.]* *o*f* [fF]reed[a-z]*","df",stats)
  # chi2/R2
  stats<-gsub("[Cc]hi[- ][Ss]quared*","chi2",stats)
  stats<-gsub(" R[- ][Ss]quared*","R^2",stats)
  stats<-gsub(" R\\^*2[ :]*([0-9\\.])"," R^2=\\1",stats)
  # unify df1 and df2
  stats<-gsub("F *([<=>][<=>]* *[0-9\\.][0-9\\.]*),.* df [\\(\\[]*([0-9\\.][0-9\\.,]*)[,;] ([0-9\\.][0-9\\.]*)[\\)]*","F(\\2, \\3)\\1",stats)
  #stats<-gsub(" df [\\(\\[]*([0-9\\.][0-9\\.,]*)[,;] ([0-9\\.][0-9\\.,]*)[\\)]*"," df1=\\1, df2=\\2",stats)
  
  # unify parameter/coefficient/Estimate
  stats<-gsub("[pP]aram[\\.etrs] *([<=>])|[Cc]oef[\\.ficents]* *([<=>])| [Ee]st[\\.imates]* *([<=>])|^[Ee]st[\\.imates]* *([<=>])"," beta\\1\\2\\3\\4",stats)
  stats<-gsub(" [Pp]redictors* *([<=>])|^[Pp]redictors* *([<=>])"," beta\\1",stats)
  # standard error
  stats<-gsub("[Ss]td*\\.*[- ]*[Ee]rr*[or\\.]*([^a-z])|[Ss]tandardi*[sz]*e*d*[- ][Ee]rr*or|S\\.[- ]*E\\.","SE\\1",stats)
  stats<-gsub("\\(SE\\)=","SE=",stats)
  # unify Sum of squares/Odds ratio/Risk Ratio
  stats<-gsub("Sum of squares","SSq",stats)
  stats<-gsub("[Oo]dd*s*[ -][Rr]atios*","OR",stats)
  stats<-gsub("[Rr]is[ck]*[ -][Rr]atios*","RR",stats)
  # percent to %
  stats<-gsub("[pP]ercentage=([0-9\\.])|[pP]ercent=([0-9\\.])","\\1\\2%",stats)
  stats<-gsub("([^<=>])=([<=>])","\\1\\2",stats)
  stats<-gsub("  *"," ",stats)
  stats<-gsub("^  *","",stats)
  # beta-anything to anything-beta
  stats<-gsub("([bB]eta)[- ]\\(*([-a-zA-z \\.]*)\\)* *([<=>])","\\2 \\1\\3",stats)
  # b/t/r/F/Z-anything to anything-b/t...
  stats<-gsub(" ([bBtTrFZz]) \\(*([-a-zA-z \\.]*)\\)* *([<=>])"," \\2 \\1\\3",stats)
  # SE-anything to anything-SE
  stats<-gsub(" (SE)[- ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",stats)
  # OR/RR-anything to anything-OR/RR
  stats<-gsub(" ([OR]R)[- ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",stats)
  # OR/RR-anything to anything-OR/RR
  stats<-gsub("(R\\^2)[- ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",stats)
  
  # clean up
  stats<-gsub(" SE SE="," SE=",stats)
  stats<-gsub(" p p="," p=",stats)
  stats<-gsub("Bias Bias=","Bias=",stats)
  
  stats<-gsub(" ,",",",gsub(", , ",", ",gsub("  |, p ,|, p $"," ",stats)))
  # , %; num=num%
  stats<-gsub(", *\\%|, = *[-0-9\\.][0-9\\.]*","",stats)
  #    stats<-gsub("^  *","",gsub(", *$","",gsub("[-0-9\\.][0-9\\.]* *= *[-0-9\\.][0-9\\.\\%]*,* *","",stats)))
  # remove bracket around non numbers
  #stats<-gsub(" \\(([^0-9[:punct:] ][^0-9[:punct:] ]*)\\)"," \\1",stats)
  # remove "^letter"
  stats<-gsub("\\^[A-z][A-z]*","",stats)
  # remove number=numer
  stats<-gsub(",[[:alpha:]]*[0-9][0-9]*=[-\\.0-9][\\.0-9]*","",stats)
  stats<-gsub("  *"," ",stats)
  stats<-gsub("^ | $","",stats)
  #  remove space before/after operator 
  stats<-gsub(" *([<>=][<>=]*) *","\\1",stats)
  # enter comma if missing between results
  stats<-gsub("([<=>][<=>]*[-\\.0-9][\\.0-9]*) ([a-zA-Z0-9]*[<=>][<=>]*[-\\.0-9][\\.0-9]*)","\\1, \\2",stats)
  return(stats)
}
