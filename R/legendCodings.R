#' legendCodings
#' 
#' Extracts the coding of p-values, brackets, abbreviations, superscripts and the reported sample size/s with 'N=number' from tables caption and footer notes/text. 
#' @param x An HTML coded table or plain textual input of table caption and/or footer text.
#' @returns A list with detected p-value and superscript codings, abbreviations and reported sample size/s.
#' @importFrom JATSdecoder grep2
#' @importFrom JATSdecoder strsplit2
#' @importFrom JATSdecoder text2num
#' @importFrom JATSdecoder text2sentences
#' @importFrom JATSdecoder letter.convert
#' @examples 
#' x<-"+ p>.05, ^**p<.01, SSq, Sum of Squares, ^a t-test, n=120. 
#' POS: perceived organizational support, JP; job performance.
#' Numbers in parenthesis are standard errors. 
#' Bold values indicate significance at p<.05."
#' legendCodings(x)
#' @export

legendCodings<-function(x){
  if(length(x)==0|is.matrix(x)) return(NULL)
  if(is.list(x)) x<-unlist(x)
  cap<-NULL;foot<-NULL
  if(length(grep("^<table",x))==0) x<-text2sentences(x)
  # get caption and footer if input is html table
  if(length(grep("^<table",x))>0){
    cap<-get.caption(x,sentences=TRUE)
    foot<-get.footer(x,sentences=TRUE)
    x<-c(foot,cap)
  }
  if(length(x)==0) return(NULL)
  
  out<-c(get.pCodes(x),
    get.bracketCodes(x),
    get.HTMLcodes(x),
    get.CronbachAlpha(x),
    get.abbr(text=x,footer=foot),
    get.N(x),
    get.DV(x),
    get.sup(x)
  )
  # superscripted * for significant to psign and p<.05
  if(
    length(grep("\\*",out$superscript))>0 &
    length(grep("[Ss]ignif",out$sup_label))>0 & 
    length(grep("^\\*$",out$psign))==0
    ){
  out$psign<-c("*",out$psign)   
  out$pval<-c("p<.05",out$pval)
  out$sup_label<-out$sup_label[grep("\\*",out$superscript,invert=TRUE)]
  out$superscript<-out$superscript[grep("\\*",out$superscript,invert=TRUE)]
  }
  return(out)
  
}

# get superscript letter coding
get.sup<-function(x){
  if(length(x)==0) return(list(sup=NULL,sup_label=NULL))
  # add ^ to stars without ^
  x<-gsub("^([^\\^0-9])(\\*\\**)","\\1^\\2",x)
  # has lines with supersript
  sup<-grep("\\^[^0-9]",x,value=T)  
  if(length(sup)==0) return(list(sup=NULL,sup_label=NULL))
  x<-gsub(" *([<=>][<=>]*) *","\\1",x)
  x<-unlist(strsplit(x,"[,;:] *| and |\\. "))
  # remove dot at end
  x<-gsub("\\. *$","",x)
  # remove first/second third...
  x<-gsub("\\^st|\\^nd|\\^rd|\\^th","",x)
  # add space after first superscripted letter
  x<-gsub("\\^\\(*([A-z])\\) *([A-z])","^\\1 \\2",x)
  # get superscript and label
  sup<-grep("\\^[^0-9]",unlist(strsplit2(x,"\\^[^0-9]","before")),value=T)
  label<-gsub("^ |\\. *$","",gsub(".*\\^[^ ]*|.*\\^[A-z][A-z]* ","",sup))
  label<-gsub("^\\**","",label)
  #remove text behind opening bracket if has no closing bracket
  i<-grep("\\)",label,invert=TRUE)
  label[i]<-gsub(" *\\(.*","",label[i])
  label<-gsub("^\\) *","",label)
  label<-gsub("^\\) *","",label)
  label<-gsub("([0-9])[[:punct:]]*$","\\1",label)
  #remove text behind closing bracket if has no opening bracket
  i<-grep("\\(",label,invert=TRUE)
  label[i]<-gsub("\\).*","",label[i])
  
  sup<-gsub("(\\^\\*\\**).*|.*(\\^[A-z][^ ]*) .*|.*(\\^[^0-9]).*","\\1\\2\\3",sup)
  # remove p values
  i<-grep("^[pP] *[<=>][<=>]* *[0\\.]|[^A-z][pP] *[<=>][<=>]* *[0\\.]|^\\*|[Pp][- ][Vv]alue|^[<=>]0*\\.[10][105][01]*",label,invert=TRUE)
  label<-label[i]
  sup<-sup[i]
  
  i<-grep("[Ss]ign.* 0*\\.[10][105]*[01]*|\\.[10][105]*[01]*.*level|level.*\\.[10][105]*[01]*",text2num(label),invert=TRUE)
  label<-label[i]
  sup<-sup[i]
  
  # add sup to label if starts with =num
  i<-grep("^[<=>][<=>]*[0-9]",label)
  label[i]<-paste0(sup[i],label[i])
  
  # clean up spaces
  label<-gsub("  *"," ",label)
  label<-gsub(" $|^ ","",label)
  
  # label must have at least four letters
  i<-nchar(label)>3
  label<-label[i]
  sup<-sup[i]
  return(list(superscript=sup,sup_label=label))
}

# get dependent variable
get.DV<-function(x){
  DV1<-NULL;DV2<-NULL;DV3<-NULL;DV4<-NULL;DV5<-NULL
  # split
  x<-unlist(strsplit(x,"[,;.] | and "))
  # select and extract D1-D5
  i<-grep("[Ee]ffects* of .* on ",x)
  DV1<-gsub("[[:punct:]]*$","",gsub(".*ffects* of .* (on|to) ","",x[i]))
  i<-grep("[Ee]ffects* on ",x)
  DV2<-gsub("[[:punct:]]*$","",gsub(".*ffects* (on|to) ","",x[i]))
  i<-grep("[Dd]ependent [Vv]ariable *[:=]",x)
  DV3<-gsub("[[:punct:]]*$","",gsub(".*ependent [Vv]ariable *([:=]|is) *","",x[i]))
  i<-grep(" (as|is)( | the )[Dd]ependent [Vv]ariable",x)
  DV4<-gsub("[[:punct:]]*$","",gsub(" (as|is)( | the )[dD]ependent [Vv]ariable.*","",x[i]))
  i<-grep("[Pp]redict[a-z]* (of|for) ",x)
  DV5<-gsub("[[:punct:]]*$","",gsub(".*[Pp]redict[a-z]* (of|for) ","",x[i]))
  # paste and return results
  DV<-unique(c(DV1,DV2,DV3,DV4,DV5))
  #DV<-paste("DV: ",DV)
  return(list(DV=DV))  
}


###########################################
get.pCodes<-function(x){
  # select lines with number
  x<-grep("[0-9]",x,value=TRUE)
  # escape
  if(length(x)==0) return(list(psign=NULL,pval=NULL))
  
  # remove html in inline-formula tag
  i<-grep("inline-formula",x)
  x[i]<-gsub("</*[a-z][^>]*>","",x[i])
  # remove second ^ in ^x^x twice
  x<-gsub("(\\^[^\\^])\\^","\\1",gsub("(\\^[^\\^])\\^","\\1",x))
  # remove point at end
  x<-gsub("\\.$","",x)
  # n.s./vs. -> ns/vs
  x<-gsub("([nNv])\\.*([sS])\\.*","\\1\\2",x)
  
  # add percent behind listed percent
  x<-gsub("([0-9]),* and( [0-9][.0-9]*%)","\\1%,\\2",x)
  x<-gsub("([0-9])(, [0-9][.0-9]*%)","\\1%\\2",x)
  x<-gsub("([0-9])(, [0-9][.0-9]*%)","\\1%\\2",x)
  # unify numbers
  x<-text2num(x)
  # remove space between operators
  x<-gsub("([<=>]) ([<=>])","\\1\\2",x)
  x<-gsub(" *([<=>][<=>]*) *","\\1",x)
  # correct .[punctA-z]
  x<-gsub("[\\.;]([[:punct:]A-z])",". \\1",x)
  # remove space in letter operator number
  x<-gsub(" *([<=>][<=>]*) *([\\.0-9])","\\1\\2",x)
  
  # remove text between brackets
  x<-gsub(" \\([A-z0-9][A-z0-9 -]*\\)","",x)
  
  # remove space between . and nuber
  x<-gsub("([pP][<=>][<=>]*)0*[\\.,] ([0-9])","\\1.\\2",x)
  # convert , to . as decimal sign in numbers with 0,num
  x<-gsub("([<=>])0*,([0-9])","\\1.\\2",x)
  # add missing p in linies with "* < num"
  x<-gsub("\\*([<=>][<=>]*0*\\.[0-9][0-9])","* p\\1",x)
  # add space between punctuation-letter at start of line
  x<-gsub("^([[:punct:]][[:punct:]]*)([A-z])","\\1 \\2",x)
  # remove:
  x<-gsub(":","",x)
  # remove "and" behind ","
  x<-gsub(", and ",", ",x)
  x<-gsub(" and ",", ",x)
  # remove "'" around * or letters 
  x<-gsub("[\\*A-z]'","\\*",x)
  x<-gsub("'[\\*A-z]","\\*",x)
  # Pr and capital P to p
  x<-gsub("(.)[pP]r*( *[<=>][<=>]* *0*\\.[0-9])","\\1p\\2",x)
  x<-gsub("^[pP]r*( *[<=>][<=>]* *0*\\.[0-9])","p\\1",x)
  # remove -value
  x<-gsub("-value","",x)
  x
  # alpha-level to p-value in lines with 
  i<-grep("level.*signific|[Ss]ignif.*level",x)
  x[i]<-gsub("\u03B1 *= *|\u1D6FC *= *|&#945 *= *","p<",x[i])
  x[i]<-gsub("([^p][^<])([0\\.]*[0-9][0-9]*) levels* ","\\1 p<\\2 ",x[i])
  # cyrillic Er to p
  x<-gsub("\u0420|\u0440","p",x)
  # rho to p
  x<-gsub("\u03C1","p",x)
  # split at sentences
  x<-unlist(strsplit(x,"\\. "))
  
  # remove starting "a"
  x<-gsub("^[Aa] ([a-z])","\\1",x)
  # add "*10^" in num-num
  x<-gsub("([0-9])(-[0-9])","\\1*10^\\2",x)
  
  # unify numbers
  x<-text2num(x,percentage=FALSE)
  
  # remove sign behind "*" or dagger if is followed by "p<"
  x<-gsub("(\\*)[^\\*]( *p[<=>])","\\1\\2",x)
  x<-gsub("(\u202[01])[^\u2020\u2021]]( *p[<=>])","\\1\\2",x)
  
  # add space between anything-p-value
  x<-gsub("([^ ])([pP][<=>][<=>]*0*\\.[0-9])","\\1 \\2",x)
  # add space in front of * or dagger
  x<-gsub("([^*])\\*","\\1 *", x)
  x<-gsub("([^\u2020\u2021])\\\u2020\u2021","\\1 *", x)

  # add % behind numbers in list of percentual values that do not have % yet
  #i<-grep("[0-9]\\%",x)
  #for(j in 1:3) x[i]<-gsub("([0-9\\.]*[0-9])([,;]* [A-z]* [0-9\\.][0-9\\.]*\\%)","\\1%\\2",x[i])
  #for(j in 1:3) x[i]<-gsub("([0-9\\.]*[0-9])([,;] [0-9\\.][0-9\\.]*\\%)","\\1%\\2",x[i])
  
  # select lines with ".number"
  x<-grep("\\.[0-9]",x,value=TRUE)
  # escape
  if(length(x)==0) return(list(psign=NULL,pval=NULL))
  
  
  # add "p<" in lines with "signific" and "level" and "number" without "p<=>"
  i<-grep2(c("[sS]ignific","level","\\.[0-9]"),x,value=FALSE)
  j<-grep("[pP] *[<=>][<=>]* *[\\.0]",x,invert=TRUE)
  i<-i[is.element(i,j)]
  x[i]<-gsub("  *"," ",gsub("(0*\\.[0-9][0-9]*)"," p<\\1",x[i]))
  x
  # remove first bracket around p-values
  x<-gsub("\\((.*p[<=>][<=>]*0*\\.[0-9][^\\)]*)\\)","\\1",x)

  # remove "[,;]" in ", p-value" in lines with one p-value
  i<-which(nchar(x)-nchar(gsub("[,;] p[<=>]","",x))==4)
  x[i]<-gsub("[,;]( p[<=>])","\\1",x[i])
  
  # clean up in lines with stars and p-values
  #i<-grep2(c("signific","level","\\.[0-9]"),x,value=FALSE)
  #stars<-gsub("[^\\*]*(\\*\\**)[^\\*]*","\\1",x[i])
  #stars
  #values<-gsub(".*(p[<=>][<=>]*[0\\.][0-9\\.]*)[^0-9]*.*","\\1",x[i])
  #values
  #x[i]<-paste(stars,values)
  
  # add p-value in lines with "significant at"
  x<-gsub(" the "," ",x)
  i<-grep2(c("[Ss]ignific[a-z]* [ao][tf] [^pP]","\\.[0-9]"),x,value=FALSE)
  x[i]<-gsub("[Ss]ignific[a-z]* [ao][tf][^0-9\\.pP]*([0-9\\.][0-9\\.]*)[^0-9]*","p<\\1",x[i])
  x
  
  # if has list of p-values parse p-values behind pSign "*" or dagger: \u2020\u2021
  i<-grep("[pP] *[<=>][<=>]* *0*\\.[0-9][0-9]*[;,] [pP] *[<=>][<=>]* *0*\\.[0-9]",x)
  if(length(i)>0){ 
    x[i]<-paste(
      gsub("[^\\*\u2020\u2021]","",grep("\\*|\u2020|\u2021",unlist(strsplit(x[i]," ")),value=T)),
      grep("p[<=>]",unlist(strsplit(x[i]," ")),value=T)
      ,collapse=" ")
  }

  # remove = after punctuation
  x<-gsub("([[:punct:]]) *= *([A-z])","\\1 \\2",x)
  
  ## extract p Codes
  pattern<-"[pP] *[<=>][<=>]* *0*\\.[0-9][0-9]*"
  pCodes <- grep(pattern,x,value=TRUE)
  pCodes

  # escape
  if(length(pCodes)==0) return(list(psign=NULL,pval=NULL))
  # split sentences
  pCodes <- unlist(strsplit(pCodes,"[,;\\.] | and ",pCodes))
  pCodes <- grep(pattern,pCodes,value=TRUE)
  
  
  ###########################################
  # remove text in front and behind * p-value
  pCodes <- gsub(".*[^[:punct:]]([[:punct:]][[:punct:]]* *[pP] *[<=>][<=>]* *0*\\.[0-9][0-9]*).*","\\1",pCodes)
  pCodes <- gsub("^([[:punct:]][[:punct:]]* *[pP] *[<=>][<=>]* *0*\\.[0-9][0-9]*).*","\\1",pCodes)
  # remove p in brackets
  pCodes <- grep("\\([Pp] *[<=>]",pCodes,value=T,invert=T)
  # remove starting/end sign
  pCodes <- gsub("^[\\.\\(\\)\"]*","",pCodes)
  pCodes <- gsub("[\\.\\)\"]*$","",pCodes)
  
  pCodes <- grep(pattern,pCodes,value=TRUE)
  
  pCodes
  
  # escape
  if(length(pCodes)==0) return(list(psign=NULL,pval=NULL))
  
  # extract signs and  values
  psign <- gsub("\\.","",gsub("(.*) *[pP] *[<=>][<=>]* *0*\\.[0-9][0-9]*","\\1",pCodes))
  pval <- gsub(".*[^A-z]([pP] *[<=>][<=>]* *0*\\.[0-9][0-9]*)","\\1",pCodes)
  
  
  ## extract psigns that have "n.s." for non significant
  i<-grep("[ [:punct:]][nN]\\.*[sS]\\.*[ [:punct:]]",paste0(" ",psign," "))
  pvalNS<-pval[i]
  psignNS<-psign[i]
  
  # and temporarly remove
  if(length(i)>0){
    pval<-pval[-i]
    psign<-psign[-i]}
  
  # remove text except "ns"
  psignNS<-gsub(".*([nN]\\.*[sS]\\.*).*","\\1",psignNS)
  # and extract p-value
  pvalNS<-gsub(".*(p[<=>][<=>]*0*\\.[0-9]*).*","\\1",pvalNS)
  
  # if x has n.s. and psignNS is empty, try to extract from x
  i<-grep("[ [:punct:]][nN]\\.*[sS]\\.*[ [:punct:]]",paste0(" ",x," "))
  if(length(i)==1&length(psignNS)==0){
    ## extract psigns that have "n.s." for non significant
    pvalNS<-"maxP"
    psignNS<-"ns"
  }
  
  #####################################
  # remove words (more than two letters)
  psign<-gsub("  *"," ",gsub("[A-z][A-z][A-z]*","",psign))
  
  # clean up
  pval<-gsub("[Pp] *([<=>][<=>]*) *([0-9\\.][0-9\\.]*).*","p\\1\\2",pval)
  pval<-gsub("(\\.[0-9][0-9]*)\\..*","\\1",pval)
  pval<-gsub("^[A-z] *(p *< *[0-9\\.]*).*","\\1",pval)
  pval<-gsub("p *([<=>][<=>]*) *","p\\1",pval)
  pval<-gsub("([<=>][<=>]*)0\\.","\\1.",pval)
  pval<-gsub("0$","",pval)
  pval
  psign
  ## clean up psign
  # remove tailoring ' in psign
  psign<-gsub("^'*([^'][^']*)'*$","\\1",psign)
  # remove text behind : and ",; "at beginning
  psign<-gsub("[:].*|^[,; ]","",psign)
  # remove everything till first star or dagger and behind last
  psign<-gsub("[^\\*\u2020\u2021]*([\\*\u2020\u2021])","\\1",psign)
  psign<-gsub("^([\\*][\\*]*)[^\\*]*","\\1",psign)
  psign<-gsub("^([\u2020\u2021][\u2020\u2021]*)[^\u2020\u2021]*","\\1",psign)
  psign<-gsub(" .*|\\n|[\\)\\( ]|\"","",psign)
  psign<-gsub("^([^A-z][^A-z]*)[A-z][A-z]*.*","\\1",psign)
  psign<-gsub("([\\*\u2020\u2021])[^\\*\u2020\u2021]*$","\\1",psign)
  # remove entries with more than 1 letter in psigns or only numbers
  j<-nchar(gsub("[A-z][[:punct:]][A-z][A-z]*|^[0-9][0-9]*$","",psign))>0
  pval<-pval[j]
  psign<-psign[j]
  psign
  
  #####################################
  # remove letters that are not a, b, c or d
  psign<-gsub("[e-zE-Z]","",psign)
  
  # remove [,;] at end
  psign<-gsub("([^,;])[,;]$","\\1",psign)

  # remove psigns with number, brackets or /
  j <- grep("[0-9]|\\[|\\]|/|[^-]-|-[^-]",psign,invert=TRUE)
  pval<-pval[j]
  psign<-psign[j]
  
  # clean up spaces
  psign<-gsub(" ","",psign)
  pval<-gsub("( ) *"," ",pval)
  
  # remove tailoring "'"
  psign<-gsub("[^']'","",gsub("'[^']","",psign))
  
 
  # remove psigns with more than one letter
  j<-grep("[A-z][A-z]",psign,invert=TRUE)
  pval<-pval[j]
  psign<-psign[j]
    
  # remove un- and badly captured psigns
  j <- nchar(psign)>0 & nchar(psign)<=3
  pval<-pval[j]
  psign<-psign[j]
  j <- nchar(psignNS)>0 & nchar(gsub("[NSns\\.]","",psignNS))==0
  pvalNS<-pvalNS[j]
  psignNS<-psignNS[j]
  
  # replace maxP with biggest p<num, else remove ns coding
  if(length(pvalNS)>0) if(pvalNS=="maxP"&length(grep("p<0*\\.",pval))>0){
      temp<-suppressWarnings(max(as.numeric(
        gsub("p<0*","",grep("p<",pval,value=TRUE))
        )))
      pvalNS<-paste0("p>",temp)
      # remove bad captures
      pvalNS<-grep("Inf|NA",pvalNS,value=TRUE,invert=TRUE)
      if(length(pvalNS)==0) psignNS<-character(0)
      }
  
  # remove bad captures
  if(length(pvalNS)>0) if(pvalNS=="maxP"){
    psignNS<-character(0)
    pvalNS<-character(0)
  }
  
  # add temporarly entries for non significant
  pval<-c(pval,pvalNS)
  psign<-c(psign,psignNS)
  
  # remove bad entries
  j<-grep("[=\\|<>,]",psign,invert=TRUE)
  psign<-psign[j]
  pval<-pval[j]
  
  # remove duplicated signs
  j<-which(duplicated(psign))
  if(length(j)>0){
    pval<-pval[-j]
    psign<-psign[-j]
  }
  
  if(length(pval)==0) return(list(psign=NULL,pval=NULL))

  out<-list(psign=psign,pval=pval)
  return(out)
}

get.N<-function(x){
  # correct .[punctA-z]
  x<-gsub("[\\.;]([[:punct:]A-z])",". \\1",x)
  # remove big-mark ,
  x<-gsub("([0-9]),([0-9]{3}([^0-9]))","\\1\\2",x)
  
  # remove space between operators
  x<-gsub("([<=>]) ([<=>])","\\1\\2",x)
  # remove space in letter operator number
  x<-gsub(" *([<=>][<=>]*) *([\\.0-9])","\\1\\2",x)
  # remove brackets
  x<-gsub("[\\(\\)]|\\[|\\]","",x)
  
  x<-unlist(strsplit2(x," [Nn]=",type="before"))
  x<-gsub("^[[:punct:]]*([Nn]=)","\\1",x)
  # select lines
  x<-grep("[^a-z][Nn]=[1-9]|^[Nn]=[1-9]",x,value=TRUE)
  # escape
  if(length(x)==0) return(list(N=NULL))
  # extract n
  n<-gsub("^([Nn]=[1-9][-0-9]*).*","\\1",x)
  n<-gsub(".* ([Nn]=[1-9][-0-9]*).*","\\1",n)
  return(list(N=n))
}

get.HTMLcodes<-function(x){
  # select lines
  x<-grep("[Bb]old|[Ii]talic",x,value=TRUE)
  
  # correct .[punctA-z]
  x<-gsub("[\\.;]([[:punct:]A-z])",". \\1",x)
  # remove space between operators
  x<-gsub("([<=>]) ([<=>])","\\1\\2",x)
  # remove space in letter operator number
  x<-gsub(" *([<=>][<=>]*) *([\\.0-9])","\\1\\2",x)
  # remove:
  x<-gsub(":","",x)
  # Pr to p
  x<-gsub("(.)[pP]r( *[<=>][<=>]* *0*\\.[0-9])","\\1p\\2",x)
  # remove -value
  x<-gsub("-values*","",x)
  # remove brackets
  x<-gsub("\\(\\)","",x)
  
  # alpha-level to p-value in lines with 
  i<-grep("level.*signific|signif.*level",x)
  x[i]<-gsub("\u03B1 *= *|\u1D6FC *= *|&#945 *= *","p<",x[i])
  
  # kyrillic Er to p
  x<-gsub("\u0420|\u0440","p",x)
  # rho to p
  x<-gsub("\u03C1","p",x)
  # split at sentences
  x<-unlist(strsplit(x,"[\\.,;] "))
  
  # select lines
  x<-grep("[Bb]old|[Ii]talic",x,value=TRUE)
  
  boldP<-NULL;italicP<-NULL
  
if(length(grep("[Cc]ronbach'*s* alpha|[Ii]ntern.*consist",x))>0){
    # and escape if has alpha
    if(length(grep("[Bb]old",x))>0) boldP<-"CrAlpha"
    if(length(grep("[Ii]talic",x))>0)  italicP<-"CrAlpha"
    } 
  
  # insert p<.05 if only significant, but no number is detected
  i<-grep("p[<=>][<=>]*|[0-9]",x,value=TRUE)
  if(length(i)==0)
    x<-gsub("[Ss]ignificant[a-z]*","p<.05",x)
    
  # select lines
  x<-grep("[0-9]",x,value=TRUE)
  x<-grep("p[<=>][<=>]*",x,value=TRUE)
  #escape
  if(length(x)==0) return(list(boldP=boldP,italicP=italicP))
  
  # unify numbers
  x<-text2num(x, percentage=FALSE)
  
  # extract p-values
  bold<-grep("p[<=>][<=>]* *0*\\.[0-9]",grep("[Bb]old",x,value=TRUE),value=TRUE)
  italic<-grep("p[<=>][<=>]* *0*\\.[0-9]",grep("[Ii]talic",x,value=TRUE),value=TRUE)
  
  bold<-gsub(".*(p[<=>][<=>]* *0*\\.[0-9][0-9]*)[^0-9].*","\\1",bold)
  bold<-gsub(".*(p[<=>][<=>]* *0*\\.[0-9][0-9]*)[^0-9]*","\\1",bold)
  italic<-gsub(".*(p[<=>][<=>]* *0*\\.[0-9][0-9]*)[^0-9].*","\\1",italic)
  italic<-gsub(".*(p[<=>][<=>]* *0*\\.[0-9][0-9]*)[^0-9]*","\\1",italic)
  
  return(list(bold=unique(c(bold,boldP)),italic=unique(c(italic,italicP))))
  
}

############################
get.bracketCodes<-function(x){
  # correct sentences ends
  x<-gsub("\\.([A-z[:punct:]])",". \\1",x)
  # correct space%
  x<-gsub("[ -]\\%","%",x)
  x<-gsub("([0-9][0-9]*\\.[0-9][0-9]*\\.)([0-9])","\\1 \\2",x)
  # split sentences
  x<-unlist(strsplit(x,"[;,] | and |\\. "))
  ## extract lines with parentheses and brackets 
  paren <- grep("[Pp]arenthe",x,value=TRUE)
  brack <- grep("[Bb]racket",x,value=TRUE)
  x<-letter.convert(x,greek2text=TRUE)
  
  # and reduce to targeted terms to extract
  patterns<-"HDI|SD|SE|CI|[eE]rror|[dD]eviation|[iI]nterval|[pP]ercent|[Vv]alue|[Ss]tatistic|[Cc]ronbach'*s* alpha|consisten"
  i<-grep(patterns,brack)
  brack<-brack[i]
  i<-grep(patterns,paren)
  paren<-paren[i]
 
  # escape
  if(length(paren)==0&length(brack)==0) return(list(bracket=NULL,parentheses=NULL))
  # unify parentheses content
  i1<-grep("[Ss]tandard[ -][Ee]rror|[^A-Z]SE[^A-Z]",paren)
  paren[i1]<-"SE"
  i2<-grep("[Ss]tandard[ -][Dd]eviation|[^A-Z]SD[^A-Z]",paren)
  paren[i2]<-"SD"
  i3<-grep("[Hh]ighest[ -][Dd]ensity.*[Ii]nterv|[^A-Z]HDI[^A-Z]",paren)
  paren[i3]<-"HDI"
  
  i4<-grep("[Cc]onfidence[ -][Ii]nterval|[Cc]red[ei]b.*[ -][Ii]nterva|[^A-Z]CI[^A-Z]",paren)
  # CI with space in front
  paren[i4]<-gsub(".* (0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]onfidence[ -][Ii]nterva.*","\\1 CI",paren[i4])
  paren[i4]<-gsub(".* (0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]red[ei]b.*[ -][Ii]nterva.*","\\1 CI",paren[i4])
  paren[i4]<-gsub(".* (0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][Cc][Ii].*","\\1 CI",paren[i4])
  # CI without space in front
  paren[i4]<-gsub("^(0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]onfidence[ -][Ii]nterva.*","\\1 CI",paren[i4])
  paren[i4]<-gsub("^(0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]red[ei]b.*[ -][Ii]nterva.*","\\1 CI",paren[i4])
  paren[i4]<-gsub("^(0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][Cc][Ii].*","\\1 CI",paren[i4])
  # 0.95 -> 99%
  paren<-gsub("^0*\\.([0-9][0-9]*) CI$","\\1% CI",paren)
  # all other to "CI
  paren[i4][grep("[Cc]onfidence[ -][Ii]nterval|[Cc]red[ei]b.*[ -][Ii]nterva|[^A-Z]CI[^A-Z]",paren[i4])]<-"CI"
  
  i5<-grep("[Vv]alue|[Ss]tatistic",paren)
  paren[i5]<-gsub(".* ([A-z])[ -].*|.* ([A-Z][A-Z]*) .*","\\1",paren[i5])
  paren[i5]<-gsub("^([A-z])[-].*|^([B-Z][A-Z]*) .*","\\1",paren[i5])

  i6<-grep("[Cc]ronbach'*s* alpha|[Ii]nternal.* consist",paren)
  paren[i6]<-"CrAlpha"
  
  
  # extract brackets content
  i1<-grep("[Ss]tandard[ -][Ee]rror|[^A-Z]SE[^A-Z]",brack)
  brack[i1]<-"SE"
  i2<-grep("[Ss]tandard[ -][Dd]eviation|[^A-Z]SD[^A-Z]",brack)
  brack[i2]<-"SD"
  i3<-grep("[Hh]ighest[ -][Dd]ensity.*[Ii]nterv|[^A-Z]HDI[^A-Z]",brack)
  brack[i3]<-"HDI"
  
  i4<-grep("[Cc]onfidence[ -][Ii]nterval|[Cc]red[ei]b.*[ -][Ii]nterva|[^A-Z]CI[^A-Z]",brack)  # with space in front
  brack[i4]<-gsub(".* (0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]onfidence[ -][Ii]nterva.*","\\1 CI",brack[i4])
  brack[i4]<-gsub(".* (0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]red[ei]b.*[ -][Ii]nterva.*","\\1 CI",brack[i4])
  brack[i4]<-gsub(".* (0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][Cc][Ii].*","\\1 CI",brack[i4])
  # without space in front
  brack[i4]<-gsub("^(0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]onfidence[ -][Ii]nterva.*","\\1 CI",brack[i4])
  brack[i4]<-gsub("^(0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][A-z]* *[Cc]red[ei]b.*[ -][Ii]nterva.*","\\1 CI",brack[i4])
  brack[i4]<-gsub("^(0*\\.*[0-9][0-9]*\\.*[0-9]*\\%*)[ -][Cc][Ii].*","\\1 CI",brack[i4])
  # 0.95 -> 99%
  brack<-gsub("^0*\\.([0-9][0-9]*) CI$","\\1% CI",brack)
  # all other to "CI"
  brack[i4][grep("[Cc]onfidence [Ii]nterval|[Cc]red[ei]b.* [Ii]nterva|[^A-Z]CI[^A-Z]",brack[i4])]<-"CI"
  
  i5<-grep("[Vv]alue|[Ss]tatistic",brack)
  brack[i5]<-gsub(".* ([A-z]) .*|.* ([A-Z][A-Z]*) .*","\\1",brack[i5])
  
  i6<-grep("[Cc]ronbach'*s* alpha|[Ii]nternal.* consist",brack)
  brack[i6]<-"CrAlpha"
  
  # reduce to plausible values
  brack<-brack[nchar(gsub("[0-9\\%\\.\\-]*","",brack))<=3&nchar(brack)>0]
  paren<-paren[nchar(gsub("[0-9\\%\\.\\-]*","",paren))<=3&nchar(paren)>0]
  
  # unique values only
  brack<-unique(brack)
  paren<-unique(paren)
  
  if(length(paren)==0) paren<-NULL
  if(length(brack)==0) brack<-NULL
  
  out<-list(bracket=brack,parentheses=paren) 
  return(out)
  }


get.CronbachAlpha<-function(x){
  alpha<-NULL
  # correct sentences ends
  x<-gsub("\\.([A-z[:punct:]])",". \\1",x)
  # split sentences
  x<-unlist(strsplit(x,"[;,] | and |\\. "))
  # select lines with diagonal
  x<-grep("[Cc]ronbach",x,value=TRUE)
  x<-letter.convert(x,greek2text=TRUE)
  x<-grep("[Aa]lpha|[iI]ntern.*consisten|[Rr]eliabili",x,value=TRUE)
  
  alpha<-NULL
  if(length(grep("[bB]old",x))>0) alpha<-c(alpha,"bold")
  if(length(grep("[Ii]talic",x))>0) alpha<-c(alpha,"italic")
  if(length(grep("[Dd]iagonal",x))>0) alpha<-c(alpha,"diagonal")
  
  return(list(alpha=alpha))
}


## get abbreviations and corresponding values
get.abbr<-function(text=NULL,footer=NULL){
  x<-text
  # escape
  if(length(x)==0&length(footer)==0) return(list(abbreviation=NULL,label=NULL))
  
  abb<-NULL; full<-NULL
  
  # correct [.;][A-z]
  x<-gsub("[\\.;]([A-z])",". \\1",x)
  # unify seperators around abbreviation
  x<-gsub(",( [A-Z][[:punct:]A-Z0-9]*), ",";\\1, ",x)
  x<-gsub(",( [A-Z][[:punct:]A-Z0-9]*)[;:] ",";\\1, ",x)
  x<-gsub("^([A-Z][[:punct:]A-Z0-9]*)[;:] ","\\1, ",x)

  # split at sentences
  x<-unlist(strsplit(x,"\\. |\\n"))
  x<-gsub("^[,;] and ","",unlist(strsplit2(x,"[,;] and ",type="before")))
  # remove space around -
  x<-gsub(" *- *","-",x)
  # ", and " to
#  x<-gsub(", and ",", ",x)
  # remove small letter and numbers at start of line
  x<-gsub("^\\^*[a-z0-9]([A-Z][a-z])","\\1",x)
  
  # remove html in inline-formula tag and all further html
  i<-grep("inline-formula",x)
  x[i]<-gsub("</*[a-z][^>]*>","",x[i])
  
  # select lines with at least two capital letters or capital letter and number
  x<-grep("[A-Z][-a-z_0-9]*[A-Z]|[A-Z][-0-9A-Z_]|[A-Z][a-z]*.[,;=] ",x,value=TRUE)
  x
  # split at "and" after abbreviation 
  x<-gsub(" and $","",unlist(strsplit2(x,"[A-Z][-0-9A-Z_] and ",type="after")))
  
  # escape
  if(length(x)==0) return(list(abbreviation=NULL,label=NULL))
  
  # unify letters
  x<-letter.convert(x)
  # remove dot at end
  x<-gsub("\\.$","",x)
  x
  # TYPE A: if has "ABB[,:] full;"
  pattern<-"^[A-Z][0-9]*[,:] |[A-Z][-0-9a-z_]*[A-Z][,:] [A-z][a-z][^;]|[A-Z][-0-9A-Z_][-0-9A-Z_]*[,:] [A-z][a-z][^;]*;|[A-Z][-0-9A-Z_][-0-9A-Z_]*[,:] [A-z][a-z][^;]*$"
  typeA<-length(grep(pattern,x))>0
  typeA
  if(typeA){
    y<-unlist(strsplit(x,"; "))
    # parse splittet bracket content
    if(sum(grepl("\\(",x) & !grepl("\\)",x))>0){
      i<-which(grepl("\\(",x) & !grepl("\\)",x))
      i<-i[i<length(x)]
      j<-!grepl("\\(",x[i+1]) & grepl("\\)",x[i+1]) >0
      i<-i[j]
      x[i]<-paste(x[i],x[i+1],sep="; ")
      x<-x[-(i+1)]
    }
    
    y<-grep(pattern,y,value=TRUE)
    y<-gsub(".* ([A-Z][-0-9A-Z_][-0-9A-Z_]*[,:] )","\\1",y)
    y<-gsub(".* ([A-Z][-0-9a-z_]*[A-Z0-9][,:] )","\\1",y)
    
    #abb<-c(abb,gsub("[^-0-9A-Z_]*([A-Z][-0-9A-Z_]*)[^-0-9A-Z_]*.*","\\1",y))
    # extract abbreviation
    abb<-c(abb,gsub("([^,:]*)[,:] .*","\\1",y))
    # remove lines with stars
    i<-grep("\\*",abb,invert=TRUE)
    abb<-abb[i]
    y<-y[i]
    
    abb<-gsub("\\(.*","",abb)
    
    if(length(abb)>0) for(i in 1:length(abb)) full<-c(full,gsub(paste0(".*[,:]* *",abb[i],"[,:] ([^,;]*).*"),"\\1",y[i]))
    if(length(abb)==0) full<-NULL
    if(length(full)==0) abb<-NULL
    # remove abb and full from x
    for(i in abb) x<-gsub(paste0(i,"[,:] "),"",x)
    for(i in full) x<-gsub(i,"",x)
  }
  x
  
  # TYPE B: if has "ABB = full;"
  typeB<-length(grep("^[A-Z][0-9]* *[=] *|[A-Z][-0-9A-z_]* *[=] *[A-z]",x))>0
  typeB
  if(typeB){
    y<-unlist(strsplit(x,"[;,] "))
    # parse splittet bracket content
    if(sum(grepl("\\(",x) & !grepl("\\)",x))>0){
      i<-which(grepl("\\(",x) & !grepl("\\)",x))
      i<-i[i<length(x)]
      j<-!grepl("\\(",x[i+1]) & grepl("\\)",x[i+1]) >0
      i<-i[j]
      x[i]<-paste(x[i],x[i+1],sep="; ")
      x<-x[-(i+1)]
    }
    
    y<-grep("[A-Z][-0-9A-z_]* *[=] *[A-z]",y,value=T)
    y<-gsub(".* ([A-Z][-0-9A-z_]* *[=] *[A-z])","\\1",y)
    abb<-c(abb,gsub("[^-0-9A-Z_]*([A-Z][-0-9A-z_]*)[^-0-9A-z_]*.*","\\1",y))
    full<-c(full,gsub(".*[=] *","",y))
    if(length(abb)==0) full<-NULL
    if(length(full)==0) abb<-NULL
    # remove abb and full
    for(i in abb) x<-gsub(paste0(i," *[=] "),"",x)
    #for(i in full) x<-gsub(paste0(i,"[,:] "),"",x)
  }

  
  # TYPE C: if has "full (ABB)"
  typeC<-length(grep("\\([A-Z][-0-9A-Z_]*\\)",x))>0
  typeC
  #[-0-9A-Z_]
  if(typeC){
    brack_label<-NULL
    bag_post<-NULL
    bag_pre<-NULL
    
    y<-grep("\\([A-Z][-0-9A-Z_]*\\)",x,value=T)
    y<-unlist(strsplit(y,"[,;:]| and "))
    brack<-grep("\\([a-z]*[A-Z][-0-9A-Z_a-z/]*\\)",y,value=T)
    brack_abb<-gsub(".*\\(([a-z]*[A-Z][-0-9A-Z_a-z/]*)\\).*","\\1",brack)
    pre_brack<-gsub("^ | $","",gsub(".*[[:punct:]]","",gsub("(.*) \\([a-z]*[A-Z][-0-9A-Z_a-z/]*\\).*","\\1",brack)))
    post_brack<-gsub(".*\\([a-z]*[A-Z][-0-9A-Z_a-z/]*\\)(.*)","\\1",brack)
    post_brack<-gsub("[[:punct:]] .*|^ ","",post_brack)
    
    # remove duplications
    i<-!duplicated(brack_abb)
    pre_brack<-pre_brack[i]
    post_brack<-post_brack[i]
    brack_abb<-brack_abb[i]
    
    # try find the full text of abbreviation
    for(i in 1:length(brack_abb)){
      chars<-nchar(brack_abb[i])
      firstChar<-substr(brack_abb[i],1,1)
      if(length(chars)>0) if(!is.na(chars)){
        # extract nchars+(1-2) words
        if(chars==1) bag_pre<-ngram2(brack[i],brack_abb[i],c(-chars-2,-1))
        if(chars==2) bag_pre<-ngram2(brack[i],brack_abb[i],c(-chars-2,-1))
        if(chars>2) bag_pre<-ngram2(brack[i],brack_abb[i],c(-chars-4,-1))
        if(length(bag_pre)==0) bag_pre<-""
        if(is.na(bag_pre)) bag_pre <-  ""
        
        if(chars==2) bag_post<-ngram2(brack[i],brack_abb[i],c(1,chars+1))
        if(chars>2) bag_post<-ngram2(brack[i],brack_abb[i],c(1,chars+2))
        if(length(bag_post)==0) bag_post<-""
        if(is.na(bag_post)) bag_post <-  ""
        
        # remove till first char as start of word
        reduced<-gsub(paste0(".* (",firstChar,")|^(",firstChar,")" ),"\\1\\2",bag_pre)
        reduced_post<-gsub(paste0(".* (",firstChar,")|^(",firstChar,")" ),"\\1\\2",bag_post)
        if(length(reduced)==0) reduced<-""
        if(is.na(reduced)) reduced <-  ""
        if(length(reduced_post)==0) reduced_post<-""
        if(is.na(reduced_post)) reduced_post <-  ""
        
        # remove till first lowerized char as start of word
        if(length(reduced)>0){
          if(nchar(reduced)==nchar(bag_pre)){
            reduced<-gsub(paste0(".* (",tolower(firstChar),")|^(",tolower(firstChar),")" ),"\\1\\2",bag_pre)
          }}
        
        if(length(reduced_post)>0){
          if(nchar(reduced_post)==nchar(bag_post)){
            reduced_post<-gsub(paste0(".* (",tolower(firstChar),")|^(",tolower(firstChar),")" ),"\\1\\2",bag_post)
          }}
        
        if(length(reduced)==0) reduced<-""
        if(length(reduced_post)==0) reduced_post<-""
        
        # check if extraction starts with starting character of abb
        temp<-paste(grep(paste0("^[",firstChar,tolower(firstChar),"]|-[",firstChar,tolower(firstChar),"]"),reduced,value=TRUE),
                    grep(paste0("^[",firstChar,tolower(firstChar),"]|-[",firstChar,tolower(firstChar),"]"),reduced_post,value=TRUE))
        
        if(length(temp)==0) temp<-""
        brack_label[i]<-gsub(" $|^ ","",temp)
        
      }
    }
    # remove emty bracket labels
    i<-which(nchar(brack_label)>0&nchar(brack_abb)>0)
    brack_label<-brack_label[i]
    brack_abb<-brack_abb[i]
    # add to already extracted
    abb<-c(abb,brack_abb)
    full<-c(full,brack_label)
  }
  
  
  # TYPE D: if has "Abc. [=,] full;"
  typeD<-length(grep("[A-Z][a-z]*\\. *[=,] *[A-z]",x))>0
  typeD
  if(typeD){
    y<-unlist(strsplit(x,"[;] "))
    # parse splittet bracket content
    if(sum(grepl("\\(",x) & !grepl("\\)",x))>0){
      i<-which(grepl("\\(",x) & !grepl("\\)",x))
      i<-i[i<length(x)]
      j<-!grepl("\\(",x[i+1]) & grepl("\\)",x[i+1]) >0
      i<-i[j]
      x[i]<-paste(x[i],x[i+1],sep="; ")
      x<-x[-(i+1)]
    }
    
    y<-grep("[A-Z][a-z]*\\. *[=,] *[A-z]",y,value=T)
    y<-gsub(".* ([A-Z][a-z]*\\. *[=,] *[A-z])","\\1",y)
    abb<-c(abb,gsub("[^-0-9A-Z_]*([A-Z][a-z]*\\.)[^a-z\\.]*.*","\\1",y))
    full<-c(full,gsub(".*[=,] *","",y))
    if(length(abb)==0) full<-NULL
    if(length(full)==0) abb<-NULL
    # remove abb and full
    for(i in abb) x<-gsub(paste0(i," *[=,] "),"",x)
    #for(i in full) x<-gsub(paste0(i,"[,:] "),"",x)
  }
  
  
  brack_abb<-NULL
  brack_label<-NULL
  
#  if(length(footer)==0&length(text)!=0) footer<-text
  
  # all other cases only in footer text
  if(length(footer)>0)
  if(!typeA&!typeB&!typeC&!typeD){
  x<-footer
  x<-unlist(strsplit(x,"[,;] "))
  
  # reduce to lines with Capital =/: letter
  if(length(grep("[A-Z] *[=:] *[A-z]",x))>0) x<-grep("[A-Z] *[=:] *[A-z]",x,value=TRUE)
  
  ######## at start of line/segments
  start<-grep("^[a-z]*[A-Z][-0-9A-Z_]",x,value=T)
  #start<-start[1:20]
  start<-gsub("^ ","",unlist(strsplit2(start," [A-Z][-0-9A-Z_]",type="before")))
  # extract first abbreviation
  abb<-gsub("^([a-z]*[A-Z][-0-9A-Z_/][-0-9A-Z_a-z/]*)[^-0-9A-Z_/].*","\\1",start)
  # text without abbreviation
  abbRem<-gsub("^[a-z]*[A-Z][-0-9A-Z_/][-0-9A-Z_a-z/]*[^-0-9A-Z_/] *[,;:= ]*(.*)","\\1",start)
  full<-gsub(" $","",gsub("[,;\\.\\(].*","",abbRem))
  
  # full text with two or more small letters
  i<-grep("[a-z][a-z]",full)
  abb<-abb[i]
  full<-full[i]
  

  # remove text till first match of first character of abb, else remove
  if(FALSE) # deactivated"
  if(length(abb)>0){
    for(i in 1:length(abb)){
     char1<-substr(abb[i],1,1)
    if(length(grep(paste0(".* ",tolower(char1)),tolower(full[i])))==0){ 
      full[i]<-""} else {
        words<-tolower(unlist(strsplit(full[i]," ")))
        words<-words[min(grep(paste0("^",tolower(char1)),words)[1]):length(words)]
        full[i]<-paste(words,collapse=" ")
      }
    }}
  
  # remove tailoring '' or brackets
  full<-gsub("^[']([^'][^']*)[']*$","\\1",full)
  full<-gsub("\\[(.*)\\]*$","\\1",full)
  full<-gsub("^\\(([^\\)][^\\)]*)\\)*$","\\1",full)
  full<-gsub("(.*)\\[.*\\]$","\\1",full)
  full<-gsub("(.*)\\(.*\\)$","\\1",full)
  
  # remove cases where abb==full
  i<-abb!=full
  abb<-abb[i]
  full<-full[i]
  
  
  # remove cases whith http
  i<-grep("http:",full,invert=TRUE)
  abb<-abb[i]
  full<-full[i]
  
  # remove bad captures
  i<-grep("^for$|^[Tt]he$|^etc$",full,invert=TRUE)
  abb<-abb[i]
  full<-full[i]

  # remove cases where nchar(full)<=2
  i<-nchar(full)>2
  #sort(table(full[!i]))
  abb<-abb[i]
  full<-full[i]
  
}
  
  i<-which(!is.na(abb)&!is.na(full))
  full<-full[i]
  abb<-abb[i]
  
  # remove text behind round brackets 
  #full<-gsub("(.*) \\(.*","\\1",full)
  
  # remove less letters in full than abb
  i<-nchar(full)>nchar(abb)
  abb<-abb[i]
  full<-full[i]
  
  # escape
  if(length(full)==0|length(abb)==0) return(list(abbreviation=NULL,label=NULL))

  # clean up
  # remove text till abb
  for(i in 1:length(full)) full[i]<-gsub(paste0(".*",abb[i],"[^A-z0-1]"),"",full[i])
  # remove text
  full<-gsub("^[Ss]tands for |^means |^is the ","",full)
  full<-gsub("^,* ","",full)
  
  # remove cases where the initial "abb" letter doesn't match and "full" is only one word
  #i<-grep("[- ]",full,invert=T) # one words
  #j<-tolower(substr(abb[i],1,1))==tolower(substr(full[i],1,1))
  #abb<-abb[i][j]
  #full<-full[i][j]
  
  # combine
  abbreviation<-c(abb,brack_abb)
  label<-c(full,brack_label)
  # remove empty
  i<-which(abbreviation!=""&label!="")
  abbreviation<-abbreviation[i]
  label<-label[i]
  
  # remove cases where the label has less than three times as many characters as the abbr
  i<-which((nchar(label)/nchar(abbreviation))>3)
  abbreviation<-abbreviation[i]
  label<-label[i]
  
  
# output
    return(list(abbreviation=abbreviation,label=label))

  }



ngram2<-function(x,pattern,ngram=c(-3,3),tolower=FALSE,split=FALSE,exact=FALSE){
  temp<-NA
  if(length(x)>0){
    text<-unlist(x)
    #text<-text2sentences(x)
    if(split==TRUE) text<-unlist(strsplit(text,"[:;,.] "))
    if(tolower==TRUE){ pattern<-tolower(pattern); text<-tolower(text)}
    if(length(grep(pattern,text))>0){
      if(sum(!is.na(text))>0){
        # select lines
        text<-text[grep(pattern,text)]
        # vectorize lines and extract ngram
        temp<-vectorize.text(text)
        # get positions of pattern in lines
        if(exact==TRUE) ind<-lapply(lapply(temp,function(y) grep(paste0("^",pattern,"$"),y)),max,warn=F)
        if(exact==FALSE) ind<-lapply(lapply(temp,function(y) grep(pattern,y)),max,warn=F)
        # get index of ngram in lines
        # For several pattern matchess in one line, take max of index
        ind<-lapply(ind,function(y) y+(ngram[1]:ngram[2]))
        len<-lapply(temp,length)
        # remove bad indices
        for(i in 1:length(ind)){
          ind[[i]]<-unlist(ind[i])[unlist(ind[i])>0&unlist(ind[i])<=length(unlist(temp[i]))]
          temp[[i]]<-temp[[i]][unlist(ind[i])]
          temp[[i]]<-paste(unlist(temp[[i]]),collapse=" ")
        }
      }
    }
  }
  temp<-unlist(temp)
  # only select cells with characters
  temp<-temp[nchar(temp)>0]
  return(temp)
}


# Function to convert text to vector of words
vectorize.text<-function(x){
  helper<-function(x){
    x<-unlist(x)
    # split at spaces
    x<-unlist(strsplit(x," "))
    # clean up front and back
    x<-gsub("[.,;(]([^0-9])","\\1",x)
    x<-gsub("[.,;)!?]*$","",x)
    # select lines with letters or numbers
    x<-grep("[a-zA-Z0-9]",x,value=T)
    x<-x[which(nchar(x)>0)]
  }
  x<-lapply(x,helper)
  return(x)
}

