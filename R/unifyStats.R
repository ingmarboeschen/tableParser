#' unifyStats
#' 
#' Unifies many textual representations of statistical results in text vectors created with table2text(). This uniformization is needed for a more precise extraction of standard results with the function standardStats() from the 'JATSdecoder' package.
#' @param x A text vector with the parsed table content.
#' @param dfHandling Logical. If TRUE, ANOVA specific handling of degrees of freedom (df) is performed. The detected residual df (df2) is imputed behind the faktor df (df1). Note: Should only be activated, when unifyStats() is applied to the collapsed content of a single table.
#' @importFrom JATSdecoder letter.convert
#' @examples
#' # Example matrix
#' m<-rbind(c("","ß","Standard Error","Pr(>|t|)"),
#' c("(Intercept)","1,234.5","123.4","1.3e-4"),
#' c("Variable 1","1,2",".04","2.4*10^-5"),
#' c("R^2",".23","*","-"))
#' m
#' 
#' # parsed content
#' text<-parseMatrixContent(unifyMatrixContent(m, correctComma = TRUE))
#' text
#' 
#' # unified stats
#' unifyStats(text)
#' @returns A unified text string.
#' @export
unifyStats<-function(x,dfHandling=FALSE){
  temp<-x
  x<-letter.convert(x,greek2text=TRUE)
  # unify minus/hyphen sign
  x<-gsub("\u2212|\u02D7|\u002D|\u2013","-",x)
  # superscripted 2
  x<-gsub("\u00b3","^2",x)
  
  # unify beta 
  x<-gsub("\u00DF|\u03b2","b",x)
  # text to number
  x<-text2num(x,percentage=FALSE,words=FALSE)
  # clean up white space around operator-number
  x<-gsub(" *([<=>][<=>]*) *(-*[\\.0-9])","\\1\\2",x)        
  # add space in "num.num,num.num" -> "num.num, num.num"
  x<-gsub("([0-9]\\.[0-9][0-9]*),([0-9][0-9]*\\.[0-9])","\\1, \\2",x)
  # correct ", =letter"
  x<-gsub(", *=([A-z])",", \\1",x)
  # remove num-tailed
  x<-gsub(" *\\(*2[- ]*tailed\\)* *"," ",x)
  x<-gsub(" *\\(*2[- ]*sided\\)* *"," ",x)
  # remove "+" in "=+num"
  x<-gsub("([<=>])\\+([0-9])","\\1\\2",x)
  x<-gsub("([<=>])\\+(\\.[0-9])","\\1\\2",x)
  
  # move stats behind comma without operator to end if does not have stat in front of operator
  x<-gsub(", [pP] ([^,;=<>]*[a-z][a-z])([<=>][<=>]*[\\.01])",", \\1 p\\2",x)
  x<-gsub(", [Dd]\\.* *[Ff]\\.* ([^,;=<>]*[a-z][a-z])([<=>][<=>]*[1-9])",", \\1 df\\2",x)
  x<-gsub(", ([BbFRdZpzt]) ([^,;=<>]*[a-z][a-z])([<=>][<=>]*-*[\\.0-9])",", \\2 \\1\\3",x)
  x<-gsub(", ([Ss][t]*[DdEe]) ([^,;=<>]*[a-z][a-z])([<=>][<=>]*-*[\\.0-9])",", \\2 \\1\\3",x)
  x<-gsub(", ([OR][R]) ([^,;=<>]*[a-z][a-z])([<=>][<=>]*-*[\\.0-9])",", \\2 \\1\\3",x)
  x<-gsub(", eta_*[pg]*\\^*2 ([^,;=<>]*[a-z][a-z])([<=>][<=>]*[\\.0-9])",", \\1 eta2\\2",x)
  x<-gsub(", chi\\^*2 ([^,;=<>]*[a-z][a-z])([<=>][<=>]*[\\.0-9])",", \\1 chi2\\2",x)
  x<-gsub(", [b]eta ([^,;=<>]*[a-z][a-z])([<=>][<=>]*[\\.0-9])",", \\1 beta\\2",x)
  
  # remove bracket around result and add comma
  x<-gsub("([0-9]),* \\(([A-z][-_A-z]*\\^*2* *= *-*[\\.0-9][\\.0-9]*)\\)","\\1, \\2,",x)
  # clean up comma
  x<-gsub(",,",",",x)
  x<-gsub(", *$","",x)
  # semicolon to comma
  x<-gsub("([0-9]); ([^0-9\\.])","\\1, \\2",x)
  # unify CI: [num, num]    
  x<-gsub("[cC]onfidence [iI]ntervall*s*|[Cc]onf\\.*[ -][Ii]nt\\.*e*r*v*a*l*","CI",x)
  x<-gsub("(\\.*[1-9][0-9]\\%*)[ -]*C\\.*I\\.*[:=]","\\1 CI:",x)
  x<-gsub(" CI: ([0-9\\.\\% ]*)CI="," \\1CI: ",x)
  x<-gsub("(\\.*[1-9][0-9]\\%*[ -]*CI)[:= ]*(-*[\\.0-9][\\.0-9]*[ ]*[^<=>0-9][ ]*-*[\\.0-9][\\.0-9]*)(, [a-zA-Z]|$)","\\1: [\\2]\\3",x)
  x<-gsub("(\\.*[1-9][0-9]\\%*[ -]*CI)[:= ]*-*([\\.0-9][\\.0-9]* to -*[\\.0-9][\\.0-9]*)","\\1: [\\2]",x)
  x<-gsub("([\\(\\[]-*[\\.0-9][\\.0-9]*) *- (-*[\\.0-9][\\.0-9]*)","\\1; \\2",x)
  x<-gsub("(CI[:=] *-*[\\(\\[]*-*[\\.0-9][\\.0-9]*) *- *([\\.0-9][\\.0-9]*)","\\1; \\2",x)
  x<-gsub("(CI[:=] *-*[\\(\\[]*-*[\\.0-9][\\.0-9]*) *to *(-*[\\.0-9][\\.0-9]*)","\\1; \\2",x)
  x<-gsub("(CI[:=]) *\\(*(-*[\\.0-9][\\.0-9]*; -*[\\.0-9][\\.0-9]*)\\)*","\\1 [\\2]",x)
  x<-gsub("([^A-z]CI)[:=] (-*[0-9\\.][0-9\\.]*)-([0-9\\.][0-9\\.]*)","\\1: [\\2; \\3]",x)
  x<-gsub("([^A-z]CI)[:=] *\\((-*[0-9\\.][0-9\\.]*)[;,] (-*[0-9\\.][0-9\\.]*)\\)","\\1: [\\2; \\3]",x)
  
  # add bracket to "=num-num" in lines with ci
  i<-grep("[0-9]\\%[ -]*C\\.*I\\.*",x)
  x[i]<-gsub("([<=>])(-*[0-9\\.][0-9\\.]*)-([0-9\\.][0-9\\.]*)","\\1[\\2; \\3]",x[i])
  
  # value +-value
  x<-gsub("\\(*([A-z][A-z]*) *\\+- *([A-z][A-z]*)\\)*=([-\\.0-9][\\.0-9]*) *\\+- *([-\\.0-9][\\.0-9]*)\\)*","\\1=\\3, \\2=+-\\4",x)
  # 2 CIs to [CI]
  x<-gsub("(\\.*[1-9][0-9]\\%*)[ -]*[Cc]onfidence [iI]nterval[^,]*=([-\\.0-9][\\.0-9]*), \\.*[1-9][0-9]\\%* [Cc]onfidence [Ii]nterval[^,]*=([-\\.0-9][\\.0-9]*)","\\1 CI: [\\2; \\3]",x)
  x<-gsub("[Cc]onfidence [iI]nterval([^a-z])","CI\\1",x)
  x<-gsub("%CI","% CI",x)
  x<-gsub("(\\.*[1-9][0-9]\\%*)[ -]*[Cc][Ii][^,]*=([-\\.0-9][\\.0-9]*), \\.*[1-9][0-9]\\%* [Cc][Ii][^,]*=([-\\.0-9][\\.0-9]*)","\\1 CI: [\\2; \\3]",x)
  x<-gsub("(% CI)[^=]*=(-*[0\\.][0\\.]*[0-9]*), [^%]*% CI[^=]*=(-*[0\\.][0\\.]*[0-9]*)","\\1: [\\2, \\3]",x)
  # %=num ->num%
  x<-gsub(" *(^/)\\%=([0-9\\.][0-9\\.]*)|^ *\\%=([0-9\\.][0-9\\.]*)","\\1\\2%",x)
  # remove ": " at beginning
  x<-gsub("^[:=] ","",x)
  # unify p-value
  #x<-gsub("[Ss]ignificance|p[[:punct:]]*[a-zA-Z][[:punct:]])","p",x)
  x<-gsub("[Vv]alues* of [Pp]([^a-z])","p\\1",x)
  x<-gsub(" [pP][- ][Vv]alues* *([0-9\\.])| [pP] ([0-9\\.])"," p=\\1\\2",x)
  x<-gsub("([^a-zA-Z]*)[pP][- ][Vv]alue([<>= ])","\\1p\\2",x)
  x<-gsub(" [Ss]ig[nifckazet \\.\\:]*([<=>][<=>]*[\\.0])"," p\\1",x)
  x<-gsub(" [Ss]ig[nifckazet \\.\\:]*([<=>][<=>]*1[\\.0]*)$"," p\\1",x)
  x<-gsub(" [Ss]ig[nifckazet \\.\\:]*([<=>][<=>]*1[\\.0]*, )"," p\\1",x)
  # unify Pr(<|letter|) -> p
  x<-gsub(" Pr* *\\([<>]*\\|*[A-z]\\|*\\)([<=>])"," p\\1",x)
  x<-gsub(" p *\\([<>]*\\|*[A-z]\\|*\\)([<=>])"," p\\1",x)
  x<-gsub(" P *([<=>])"," p\\1",x)
  x<-gsub(" \\([pP]\\) *([<=>])"," p\\1",x)
  x<-gsub(" \\([pP]\\) *(\\([Ss]ig[nifcante\\.]*\\)[<=>])"," p\\1",x)

  # Correlation
  x<-gsub("([Cc]orr*elation) *([<=>])","\\1 r\\2",x)
  # remove "statistic"/"value
  x<-gsub("[- ][Ss]tatistics*([<=>])","\\1",x)
  x<-gsub("[- ][Vv]alues*([<=>])","\\1",x)
  # and other text behind "-" in front of result
  x<-gsub("([A-z]) *- *[A-z]*([<=>][<=>]*-*[\\.0-9])","\\1\\2",x)
  
  # degrees of freedom
  x<-gsub("[dD][fF]1, *[dD][fF]2","df1, df2",x)
  x<-gsub("[dD]eg[res\\.]* *o*f* [fF]reed[a-z]*","df",x)
  x<-gsub(" [dD]\\.* *[fF]\\.* *([<=>])"," df\\1",x)
  x<-gsub(" [dD]\\.* *[fF]\\.*_([1-2][<=>])"," df\\1",x)
  x<-gsub(" \\([dD]*\\.*[fF]*\\.* *=* ([1-9][0-9\\.]*)[,;] *([1-9][0-9\\.])*\\)( F)=","\\3(\\1, \\2)=",x)
  x<-gsub("[Dd]en\\.*u*m*\\.*e*r*a*t*o*r* *[Dd]\\.*[Ff]\\.*","df2",x)
  x<-gsub("[Nn]um\\.*e*r*a*t*o*r* *[Dd]\\.*[Ff]\\.*","df1",x)
  x<-gsub(" *[Dd]\\.*[Ff]\\.*[-_ ][Dd]en\\.u*m*\\.*e*r*a*t*o*r*","df2",x)
  x<-gsub(" *[Dd]\\.*[Ff]\\.*[-_ ][Nn]um\\.*e*r*a*t*o*r*","df1",x)
  i<-grep(" F[<=>]",x)
  x[i]<-gsub(" [dD]\\.* *[fF]\\.*[- _]*[A-z]*([<=>][0-9\\.][0-9\\.]*),[^,:]* [dD]\\.* *[fF]\\.*[- _]*[A-z]*([<=>][0-9\\.][0-9\\.]*)"," df1\\1, df2\\2",x[i])
  x[i]<-gsub(" [dD]\\.* *[fF]\\.*[\\( ]*[A-z]*[\\)]*([<=>][0-9\\.][0-9\\.]*),[^,:]* [dD]\\.* *[fF]\\.*[\\( ]*[A-z]*[\\)]*([<=>][0-9\\.][0-9\\.]*)"," df1\\1, df2\\2",x[i])
  x[i]<-gsub(" [dD]\\.* *[fF]\\.*: \\(([1-9][0-9\\.]*), *([1-9][0-9\\.]*)\\)"," df1=\\1, df2=\\2",x[i])
  x[i]<-gsub(" [dD]\\.* *[fF]\\.*=([1-9][0-9\\.]*), [^,:=]* [dD]\\.* *[fF]\\.*="," df1=\\1, df2=\\2",x[i])
  x[i]<-gsub(" [dD]\\.* *[fF]\\.*[^,:=]*=([1-9][0-9\\.]*), ([1-9][0-9\\.]*)"," df1=\\1, df2=\\2",x[i])
  
  # chi2/R2
  x<-gsub("[Cc]hi[- ][Ss]quared*","chi2",x)
  x<-gsub(" R[- ][Ss]quared*","R^2",x)
  x<-gsub(" R\\^*2[ :]*([0-9\\.])"," R^2=\\1",x)
  # eta^2
  x<-gsub("([^A-z])eta[^a-z\\^]2=","\\1eta^2=",x)
 # x<-gsub("([^A-z])eta_[A-z]*\\^*2=","\\1eta^2=",x)
  x<-gsub("([^A-z])eta([PGpg])\\^*2=","\\1eta_\\2^2=",x)
  
  # unify df1 and df2
  x<-gsub(" F *([<=>][<=>]* *[0-9\\.][0-9\\.]*),.* df [\\(\\[]*([0-9\\.][0-9\\.,]*)[,;] ([0-9\\.][0-9\\.]*)[\\)]*"," F(\\2, \\3)\\1",x)
  x<-gsub("df=([1-9][0-9\\.]*)[,;] ([1-9][0-9\\.]*,.* F=)","df1=\\1, df2=\\2",x)
  x<-gsub("( F=.*) df=([1-9][0-9\\.]*)[,;] ([1-9][0-9\\.]*)","\\1 df1=\\2, df2=\\3",x)
  
  #x<-gsub(" df [\\(\\[]*([0-9\\.][0-9\\.,]*)[,;] ([0-9\\.][0-9\\.,]*)[\\)]*"," df1=\\1, df2=\\2",x)
  # eta^2/d
  x<-gsub("eta\\^2[ _][^<=>]*([<=>])","eta^2\\1",x)
  x<-gsub(" d[ _][^<=>]*([<=>])"," d\\1",x)
  
  # unify parameter/coefficient/Estimate
  x<-gsub("([pP]aram[\\.etrs]*) *([<=>])|([Cc]oef[\\.ficents]*) *([<=>])|( [Ee]st[\\.imates]*) *([<=>])|^([Ee]st[\\.imates]*) *([<=>])",
          "\\1\\3\\5\\7 beta\\2\\4\\6\\8",x)
  x<-gsub("( [Pp]redictors*) *([<=>])|^[Pp]redictors* *([<=>])","\\1 beta\\2",x)
  # standard error
  x<-gsub("[Ss]td*\\.*[- ]*[Ee]rr*[or\\.s]*([^a-z])|[Ss]tandardi*[sz]*e*d*[- ][Ee]rr*[or\\.s]*|S\\.[- ]*E\\.","SE\\1",x)
  x<-gsub("\\(SE\\)([<=>])","SE\\1",x)
  # unify Sum of squares/Odds ratio/Risk Ratio
  x<-gsub("(Sum of squares)","\\1 SSq",x)
  x<-gsub(" [Oo]dd*s*[ -][Rr]atios*"," OR",x)
  x<-gsub(" [Rr]is[ck]*[ -][Rr]atios*"," RR",x)
  # percent to %
#  x<-gsub("[pP]ercentage=([0-9\\.])|[pP]ercent=([0-9\\.])","\\1\\2%",x)
  x<-gsub("([^<=>])=([<=>])","\\1\\2",x)
  x<-gsub("  *"," ",x)
  x<-gsub("^  *","",x)
  
  # move percent sign behind =num if is in front of =
  x<-gsub("([^0-9])%(=\\.*[0-9][0-9\\.]*)([^\\.0-9%]*[,;] [^0-9\\(])","\\1\\2%\\3",x)
  x<-gsub("([^0-9])%(=\\.*[0-9][0-9\\.]*)([^\\.0-9%]*)$","\\1\\2%",x)
  
  # remove space before bracket with df
  x<-gsub("( [A-z]) *\\(([dD]\\.*[fF]\\.* *= *[0-9\\.])","\\1(\\2",x)

  # beta-anything to anything-beta
  x<-gsub("([bB]eta)[-_ ]\\(*([-a-zA-z \\.]*)\\)* *([<=>])","\\2 \\1\\3",x)
  # b/t/r/F/Z-anything to anything-b/t...
  x<-gsub(" ([bBtTrFZz]) \\(*([-a-zA-z \\.]*)\\)* *([<=>])"," \\2 \\1\\3",x)
  # SE-anything to anything-SE
  x<-gsub(" (SE)[-_ ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",x)
  # OR/RR-anything to anything-OR/RR
  x<-gsub(" ([OR]R)[-_ ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",x)
  # OR/RR-anything to anything-OR/RR
  x<-gsub("(R\\^2)[-_ ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",x)
  
  # add "b=" to effect in lines with SE
  x<-gsub("( [Ee]ffects*)([<=>][<=>]*-*[\\.0-9][\\.0-9]*[,;]*[^<=>]* \\(*SE[<=>][<=>]*[\\.0-9])"," \\1 b\\2",x)
  x<-gsub("( [Ee]ffects*)([<=>][<=>]*-*[\\.0-9][\\.0-9]*[,;]*[^<=>]* p[<=>][<=>]*[0-9\\.][0-9\\.]*[,;]*[^<=>]* SE[<=>][<=>]*[\\.0-9])"," \\1 b\\2",x)
  
  # remove badly set space in "=. num"
  x<-gsub("([<=>]\\.) ([0-9])","\\1\\2",x)
  
  # move number of F=num behind F()= in "F()=!num, F=num"
  x<-gsub("(F_*\\([1-9][0-9]*,[^\\)]*\\))[<=>][^\\.0-9][^\\.0-9=]* F([<=>][\\.0-9][\\.0-9]*)","\\1\\2",x)
  
  x<-gsub("Bayes[- ][fF]actors*","BF",x)
  # clean up
  x<-gsub(" SE SE="," SE=",x)
  x<-gsub(" p p="," p=",x)
  x<-gsub("Bias Bias=","Bias=",x)
  
  x<-gsub(" ,",",",gsub(", , ",", ",gsub("  |, p ,|, p $"," ",x)))
  # , %; num=num%
  x<-gsub(", *\\%|, = *[-0-9\\.][0-9\\.]*","",x)
  # remove "^letter"
  x<-gsub("\\^[A-z][A-z]*","",x)
  # remove number=numer
  x<-gsub(",[[:alpha:]]*[0-9][0-9]*=[-\\.0-9][\\.0-9]*","",x)
  x<-gsub("  *"," ",x)
  x<-gsub("^ | $","",x)
  #  remove space before/after operator 
  x<-gsub(" *([<>=][<>=]*) *(-*[\\.0-9])","\\1\\2",x)
  # enter comma if missing between results
  x<-gsub("([<=>][<=>]*[-\\.0-9][\\.0-9]*) ([a-zA-Z0-9]*[<=>][<=>]*[-\\.0-9][\\.0-9]*)","\\1, \\2",x)
  
  # correct df in lines with F-value
  i<-grep(" F=[0-9\\.]",x)
  x[i]<-gsub(" df=([1-9][\\.0-9]*), ([1-9][\\.0-9]*),"," df1=\\1, df2=\\2",x[i])
  x[i]<-gsub(" df=\\(([1-9][\\.0-9]*), ([1-9][\\.0-9]*)\\),"," df1=\\1, df2=\\2",x[i])
  x
  
  ## if has line with constant/intercept/model
  if(length(grep("[Cc]onstant|[Ii]ntercept| [Mm]odel[^a-z]|[ -]R\\^*2|^R\\^*2|AIC|BIC|nformation [Cc]riter|[GN ]LM[^A-z]",x))>0){
  ## add b= in lines with SE= but without b= nor d=
  i<-grepl(" SE[<=>]",x) & !grepl(" d\\|*[<=>]| [bB][<=>]| [bB]eta[<=>]",x)
    # if no potential other standard stat is in front of SE 
    x[i]<-gsub("([a-z][a-z0-9]*[a-z])([<=>][<=>]*-*[\\.0-9][0-9\\.]*([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])
    x[i]<-gsub("([a-z][a-z][a-z]*[_ ][0-9]*)([<=>][<=>]*-*[\\.0-9]*[0-9]([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])
    x[i]<-gsub("([A-Z][A-Z][A-Z]*[_ ][0-9]*)([<=>][<=>]*-*[\\.0-9]*[0-9]([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])
    x[i]<-gsub("([a-z][a-z][a-z]*[_ ][0-9][-A-z_0-9]*)([<=>][<=>]*-*[\\.0-9]*[0-9]([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])
  }
  
  ## add b= in lines with SE= and Model num=
  i<-grepl(" SE[<=>]",x) & grepl("^G*LM *[0-9]*[<=>]| G*LM *[0-9]*[<=>]|[Mm]odel *[0-9]*[<=>]",x)
  # if no potential other standard stat is in front of SE 
  x[i]<-gsub("([a-z][a-z0-9]*[a-z])([<=>][<=>]*-*[\\.0-9][0-9\\.]*([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])
  x[i]<-gsub("([a-z][a-z][a-z]*[_ ][0-9]*)([<=>][<=>]*-*[\\.0-9]*[0-9]([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])
  x[i]<-gsub("([A-Z][A-Z][A-Z]*[_ ][0-9]*)([<=>][<=>]*-*[\\.0-9]*[0-9]([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])
  x[i]<-gsub("([a-z][a-z][a-z]*[_ ][0-9][-A-z_0-9]*)([<=>][<=>]*-*[\\.0-9]*[0-9]([;,];* p[<=>][<=>]*[0-9\\.][0-9\\.]*)*[,;];*[^,<=>]* [Ss][Ee][<=>])","\\1 b\\2",x[i])

# CI=num, CI=num -> CI[num; num]
  x<-gsub("([^A-Z]CI)=(-*[\\.0-9][\\.0-9]*),[^,]*CI=(-*[\\.0-9][\\.0-9]*)","\\1: [\\2; \\3]",x)
  x<-gsub("([^A-Z]CI)[ Llower]*[LimtB]*=(-*[\\.0-9][\\.0-9]*),[^,]*CI[ Uuper]*[LimtB]*=(-*[\\.0-9][\\.0-9]*)","\\1: [\\2; \\3]",x)
  x<-gsub("([^A-Z]CI)[ Llower]*[LimtB]*=(-*[\\.0-9][\\.0-9]*),[^,]*[Uu][Ll]=(-*[\\.0-9][\\.0-9]*)","\\1: [\\2; \\3]",x)
  x<-gsub("[Llower]*[-LimtB]*(CI)=(-*[\\.0-9][\\.0-9]*),[^,]*[ Uuper]*[-LimtB]*.*CI=(-*[\\.0-9][\\.0-9]*)","\\1: [\\2; \\3]",x)
  x<-gsub("(\\%[- ]*CI[^<=>,;]*) lower[^<=>,;]*=(-*[0-9\\.][0-9\\.]*),[^<=>,;]* upper[^<=>,;]*=(-*[0-9\\.][0-9\\.]*)","\\1: [\\2; \\3]",x)
  x<-gsub("(, [-0-9\\.\\% ]*CI)=(-*[\\.0-9][\\.0-9]*)[,;] (-*[\\.0-9][\\.0-9]*)","\\1: [\\2; \\3]",x)
  
  x<-gsub("CI *CI","CI",x)
  # clean up p
  x<-gsub(", p: (p[<=>])",", \\1",x)
  x<-gsub(", p: *(;; p[<=>])","\\1",x)
  x<-gsub(" p: *(;; p[<=>])","\\1",x)
  
  # remove all but last coded p in lines with one normal p-value
  if(length(x)>0){
  fun<-function(x){
  multiCodedP<-length(unlist(strsplit(x,";; p[<=>]")))>2
  oneP<-length(unlist(strsplit(x,"[^;] p[<=>]")))==2
  if(multiCodedP & oneP){
    temp<-unlist(strsplit(x,";; p"))
    temp[2:(length(temp)-1)]<-
      gsub("^[<=>][<=>]*[0]*\\.[0-9][0-9],",",",temp[2:(length(temp)-1)])
    temp[length(temp)]<-paste0(";; p",temp[length(temp)])
    x<-paste0(temp,collapse="")
  }
  return(x)
  }
  x<-unlist(lapply(x,fun))
  }
  
  # df1, df2=num1,num2 -> df1=num1, df2=num2
  x<-gsub("(df1)([,;] df2=)([1-9][0-9\\.]*)[,;] *([1-9][\\.0-9])*","\\1=\\3\\2\\4",x)
  x<-gsub("df1[,;] df1([<=>])","df1\\1",x)
#  x<-gsub("d(f1[,;] df2([<=>])","df1\\1",x)
  x<-gsub(", p(;; p[<=>])","\\1",x)
  # special processing of ANOVA results (add df2 to df1)
  if(isTRUE(dfHandling)) {
    x<-anovaHandler(x)
    
    # extract N and add/replace as df to [rt]=num -> [rt](N-2)=num but no df=num by row
    funDF<-function(x){
      # escape if has df=num
      if(length(grep(" [dD]\\.*[fF]\\.* *= *[1-9]|^[dD]\\.*[fF]\\.$",x))>0) return(x)
      
      if(length(grep(" [rt][<=>][<=>]*-*[\\.0-9]|^[rt][<=>][<=>]*-*[\\.0-9]",x))>0){
        if(length(grep("[rt][<=>][<=>]*-*[\\.0-9][\\.0-9]*,.* [Nn]=([1-9][0-9]*)",x))>0){
          N<-suppressWarnings(as.numeric(gsub(".*[rt][<=>][<=>]*-*[\\.0-9][\\.0-9]*,.* [Nn]=([1-9][0-9]*).*","\\1",x)))
          if(!is.na(N)) x<-gsub("(.* [rt])([<=>][<=>]*-*[\\.0-9][\\.0-9]*,*.* [Nn]=[1-9][0-9]*.*)",paste0("\\1(",N-2,")\\2"),x)
          if(!is.na(N)) x<-gsub("^([rt])([<=>][<=>]*-*[\\.0-9][\\.0-9]*,*.* [Nn]=[1-9][0-9]*.*)",paste0("\\1(",N-2,")\\2"),x)
        }
        
        if(length(grep("[ \\(][Nn]=[1-9][0-9]*.* [rt][<=>][<=>]*-*[\\.0-9][\\.0-9]*|^[Nn]=[1-9][0-9]*.* [rt][<=>][<=>]*-*[\\.0-9][\\.0-9]*",x))>0){
          N<-suppressWarnings(as.numeric(gsub(".*[Nn]=([1-9][0-9]*).* [rt][<=>][<=>]*-*[\\.0-9].*","\\1",x)))
          if(!is.na(N)) x<-gsub("(.*[ \\(][Nn]=[1-9][0-9]*.* [rt])([<=>][<=>]*-*[\\.0-9])",paste0("\\1(",N-2,")\\2"),x)
          if(!is.na(N)) x<-gsub("^([Nn]=[1-9][0-9]*.* [rt])([<=>][<=>]*-*[\\.0-9])",paste0("\\1(",N-2,")\\2"),x)
        }
      }
      
      
      return(x)
    }
    x<-unlist(lapply(x,funDF))
    
  }
  # warning
  if(sum(!is.element(x,temp))>0) warning("Some textual result representations have been unified for a smoother extraction of statistical standard results.",call. = FALSE)
  
  return(x)
}
