#' unifyStats
#' 
#' Unifies many textual representations of statistical results in text vectors created with table2text(). This uniformisation is needed for a more precise extraction of standard results with JATSdecoder::standardStats().
#' @param x A text string with parsed tabled results.
#' @importFrom JATSdecoder letter.convert
#' @returns A unified text string.
#' @export
unifyStats<-function(x){
  x<-letter.convert(x,greek2text=TRUE)
  # unify minus/hyphen sign
  x<-gsub("\u2212|\u02D7|\u002D|\u2013","-",x)
  # unify beta 
  x<-gsub("\u00DF|\u03b2","b",x)
  # text to number
  x<-text2num(x,percentage=FALSE,words=FALSE)
  # clean up white space around operator
  x<-gsub(" *([<=>][<=>]*) *","\\1",x)        
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
  
  
  # unify CI:[num, num]    
  x<-gsub("[cC]onfidence [iI]ntervall*s*|[Cc]onf\\.*[ -][Ii]nt\\.*e*r*v*a*l*","CI",x)
  x<-gsub("(\\.*[1-9][0-9]\\%*[ -]C\\.*I)[= ]*([-\\.0-9][\\.0-9]*[ ]*[^<=>0-9][ ]*[-\\.0-9][\\.0-9]*)(, [a-zA-Z]|$)","\\1: [\\2]\\3",x)
  x<-gsub("(\\.*[1-9][0-9]\\%*[ -]C\\.*I\\.*)[= ]*([-\\.0-9][\\.0-9]* to [-\\.0-9][\\.0-9]*)","\\1: [\\2]",x)
  x<-gsub("([\\(\\[]-*[\\.0-9][\\.0-9]*) *- (-*[\\.0-9][\\.0-9]*)","\\1; \\2",x)
  
  x<-gsub("(C\\.*I.*[:=] *[\\(\\[]-*[\\.0-9][\\.0-9]*) - ([\\.0-9][\\.0-9]*)","\\1; \\2",x)
  x<-gsub("(C\\.*I.*[:=] *[\\(\\[]-*[\\.0-9][\\.0-9]*)-([\\.0-9][\\.0-9]*)","\\1; \\2",x)
  
  
  # add bracket to "=num-num" in lines with ci
  i<-grep("[0-9]\\% C\\.*I\\.*",x)
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
  x<-gsub("Pr *\\([<>]*\\|*[a-zA-Z]\\|*\\)","p",x)
  x<-gsub(" P *([<=>])"," p\\1",x)
  
  # Correlation
  x<-gsub("([Cc]orr*elation) *([<=>])","\\1 r\\2",x)
  # remove "statistic"/"value
  x<-gsub("[- ][Ss]tatistics*([<=>])","\\1",x)
  x<-gsub("[- ][Vv]alues*([<=>])","\\1",x)
  # and other text behind "-"
  x<-gsub("([A-z]) *- *[A-z]*([<=>])","\\1\\2",x)
  
  # degrees of freedom
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
  x<-gsub("([pP]aram[\\.etrs]) *([<=>])|([Cc]oef[\\.ficents]*) *([<=>])|( [Ee]st[\\.imates]*) *([<=>])|^([Ee]st[\\.imates]*) *([<=>])",
          "\\1\\3\\5\\7 beta\\2\\4\\6\\8",x)
  x<-gsub("( [Pp]redictors*) *([<=>])|^[Pp]redictors* *([<=>])","\\1 beta\\2",x)
  # standard error
  x<-gsub("[Ss]td*\\.*[- ]*[Ee]rr*[or\\.]*([^a-z])|[Ss]tandardi*[sz]*e*d*[- ][Ee]rr*or|S\\.[- ]*E\\.","SE\\1",x)
  x<-gsub("\\(SE\\)=","SE=",x)
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
  x<-gsub("([bB]eta)[- ]\\(*([-a-zA-z \\.]*)\\)* *([<=>])","\\2 \\1\\3",x)
  # b/t/r/F/Z-anything to anything-b/t...
  x<-gsub(" ([bBtTrFZz]) \\(*([-a-zA-z \\.]*)\\)* *([<=>])"," \\2 \\1\\3",x)
  # SE-anything to anything-SE
  x<-gsub(" (SE)[- ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",x)
  # OR/RR-anything to anything-OR/RR
  x<-gsub(" ([OR]R)[- ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",x)
  # OR/RR-anything to anything-OR/RR
  x<-gsub("(R\\^2)[- ]([-a-zA-z \\.]*)([<=>])"," \\2 \\1\\3",x)
  
  # add "d=" to effect in lines with SE
  x<-gsub("( [Ee]ffect)([<=>][<=>]*-*[\\.0-9][\\.0-9]*, SE=[\\.0-9])"," \\1 d\\2",x)
  
  # remove badly set space in "=. num"
  x<-gsub("([<=>]\\.) ([0-9])","\\1\\2",x)
  
  # move number of F=num behind F()= in "F()=!num, F=num"
  x<-gsub("(F_*\\([1-9][0-9]*,[^\\)]*\\))[<=>][^\\.0-9][^\\.0-9=]* F([<=>][\\.0-9][\\.0-9]*)","\\1\\2",x)
  
  # clean up
  x<-gsub(" SE SE="," SE=",x)
  x<-gsub(" p p="," p=",x)
  x<-gsub("Bias Bias=","Bias=",x)
  
  x<-gsub(" ,",",",gsub(", , ",", ",gsub("  |, p ,|, p $"," ",x)))
  # , %; num=num%
  x<-gsub(", *\\%|, = *[-0-9\\.][0-9\\.]*","",x)
  #    x<-gsub("^  *","",gsub(", *$","",gsub("[-0-9\\.][0-9\\.]* *= *[-0-9\\.][0-9\\.\\%]*,* *","",x)))
  # remove bracket around non numbers
  #x<-gsub(" \\(([^0-9[:punct:] ][^0-9[:punct:] ]*)\\)"," \\1",x)
  # remove "^letter"
  x<-gsub("\\^[A-z][A-z]*","",x)
  # remove number=numer
  x<-gsub(",[[:alpha:]]*[0-9][0-9]*=[-\\.0-9][\\.0-9]*","",x)
  x<-gsub("  *"," ",x)
  x<-gsub("^ | $","",x)
  #  remove space before/after operator 
  x<-gsub(" *([<>=][<>=]*) *","\\1",x)
  # enter comma if missing between results
  x<-gsub("([<=>][<=>]*[-\\.0-9][\\.0-9]*) ([a-zA-Z0-9]*[<=>][<=>]*[-\\.0-9][\\.0-9]*)","\\1, \\2",x)
  
  # correct df in lines with F-value
#  i<-grep(" F=[0-9\\.]",x)
#  x[i]<-gsub(" df=([1-9][\\.0-9]*), ([1-9][\\.0-9]*),"," df1=\\1, df2=\\2",x[i])
#  x[i]<-gsub(" df=\\(([1-9][\\.0-9]*), ([1-9][\\.0-9]*)\\),"," df1=\\1, df2=\\2",x[i])
#  x
  
  ## if has line with with constant/intercept/model
  if(length(grep("[Cc]onstant|[Ii]ntercept| [Mm]odel[^a-z]|[ -]R\\^*2|^R\\^*2|AIC|BIC|nformation [Cc]riter",x))>0){
  ## add b= in lines with SE= but without b= nor d=
  i<-grepl(" SE[<=>]",x) & !grepl(" d\\|*[<=>]| [bB][<=>]| [bB]eta[<=>]",x)
    # if no potential other standard stat is in front of SE 
    x[i]<-gsub("([a-z][a-z0-9]*[a-z])([<=>][<=>]*-*[\\.0-9][0-9\\.]*[; p<>=05\\.]*,[^,]* [Ss][Ee]=)","\\1 b\\2",x[i])
    x[i]<-gsub("([a-z][a-z][a-z]* [0-9]*)([<=>][<=>]*[-\\.0-9]*[0-9][; p<>=05\\.]*,[^,]* [Ss][Ee]=)","\\1 b\\2",x[i])
    x[i]<-gsub("([a-z][a-z][a-z]* [0-9][-A-z_0-9]*)([<=>][<=>]*[-\\.0-9]*[0-9][; p<>=05\\.]*,[^,]* [Ss][Ee]=)","\\1 b\\2",x[i])
  }
  
  # clean up p
  x<-gsub(", p: (p[<=>])",", \\1",x)
  x<-gsub(", p: *(;; p[<=>])","\\1",x)
  x<-gsub(" p: *(;; p[<=>])","\\1",x)
  
  
  # special processing of ANOVA results (add df2 to df1)
  x<-anovaHandler(x)
  return(x)
}
