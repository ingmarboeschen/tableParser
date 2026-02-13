#' unifyMatrixContent
#' 
#' Unifies textual and numerical content of character matrices. Unifies hyphens, spaces, hexadecimal and Greek letters, and performs space and comma corrections. Big marks in numbers are removed. HTML tags <sup> and <sub> are converted to '^' and '_' respectively. All other HTML tags are removed. 
#' @param x A character matrix or list of character matrices.
#' @param letter.convert Logical. If TRUE, hexadecimal- and html-encoded letters will be unified and converted to Unicode with JATSdecoder::letter.convert().
#' @param greek2text Logical. If TRUE and 'letter.convert=TRUE', converts and unifies various Greek letters to a text-based form (e.g., 'alpha', 'beta'). 
#' @param text2num Logical. If TRUE, textual representations of numbers (words, exponents, fractions) are converted to digit numbers. 
#' @param correctComma Logical. If TRUE, commas used as decimal are converted to dots, big mark commas are removed. 
#' @param na.rm Logical. If TRUE, cells with NA, or only minus, hyphen, slash, or dot are set to empty cells.
#' @returns A unified character matrix or list of character matrices.
#' @examples
#' # Example matrix
#' m<-rbind(c("","ß","Standard Error","Pr(>|t|)"),
#'          c("(Intercept)","1,234.5","123.4","1.3e-4"),
#'          c("Variable 1","1,2",".04","2.4*10^-5"),
#'          c("R^2",".23","*","-"))
#' m
#' 
#' # apply function
#' unifyMatrixContent(m, correctComma = TRUE)
#' @importFrom JATSdecoder letter.convert
#' @importFrom JATSdecoder text2num
#' @export
unifyMatrixContent<-function(x,letter.convert=TRUE,
                             greek2text=TRUE,text2num=TRUE,
                             correctComma=FALSE,na.rm=TRUE){
  
  if(sum(nchar(unlist(x)))==0) return(x)
  
  fun<-function(x,letter.convert=TRUE,greek2text=TRUE,text2num=TRUE,correctComma=FALSE,na.rm=TRUE){
    if(!is.matrix(x)) return(x)
    # get attributes
    attribs<-attributes(x)
    
    # number of columns
    nCol<-ncol(x)
    
    # remove comma from big numbers except in columns with df if only one column with df
  #  hasDF<-colSums(matrix(grepl("^[dD][Ff]|[dD][Ff]$|[dD]eg[res\\.]* [ of]*[Ff]re",x),ncol=nCol))
  #  if(sum(hasDF)>1) hasDF[hasDF>0]<-0
  #  i<-which(hasDF==0)
    
    # add space to num[,;]num in columns with df1 and df2
    i<-which(colSums(matrix(grepl("df1[,;] *df2",x),ncol=nCol))>0)
    if(length(i)>0)
      x[,i]<-gsub("(\\(*[1-9]\\.*[0-9]*[,;])([1-9][\\.0-9]*\\)*)","\\1 \\2",x[,i])
    
    # add space to num[,;]num in columns with "df" and dot as decimal at least once 
    i<-which(colSums(matrix(grepl("^[dD]\\.*[fF]\\.*$|^[dD]egree[sof ]*[fF]reedom*$",x),ncol=nCol))>0 & 
               colSums(matrix(grepl("[0-9]\\.[0.9]",x),ncol=nCol))>0)
    if(length(i)>0)
      x[,i]<-gsub("([1-9]\\.*[0-9]*[,;])([1-9][\\.0-9]*)","\\1 \\2",x[,i])
    
    # add space to num[,;]num in columns with "df" and one or two digits behind "," at least once
    i<-which(colSums(matrix(grepl("^[dD]\\.*[fF]\\.*$|^[dD]egree[sof ]*[fF]reedom*$",x),ncol=nCol))>0 & 
               colSums(matrix(grepl("[0-9],[1-9][^0-9]*$|[0-9],[1-9][0-9][^0-9]*$",x),ncol=nCol))>0)
    if(length(i)>0)
      x[,i]<-gsub("([1-9]\\.*[0-9]*[,;])([1-9][\\.0-9]*)","\\1 \\2",x[,i])
    
    # convert DF1/DF2->df1, df2 in columns with df1 and df2
    i<-which(colSums(matrix(grepl("[dD][fF]1 */ *[dD][fF]2",x),ncol=nCol))>0)
    if(length(i)>0){
      x[,i]<-gsub("(\\(*[1-9][\\.0-9]*) */ *([1-9][\\.0-9]*\\)*)","\\1, \\2",x[,i])
      x[,i]<-gsub("([dD][fF]1) */ *([dD][fF]2)","\\1, \\2",x[,i])
    }
    # remove brackets around content in columns with only bracketed content in header
    i<-grep("^\\(.*\\)$",x[1,])
    if(length(i)>0)
      x[,i]<-gsub("^[\\(](.*)[\\)]$","\\1",x[,i])
      
    # add space in "num.num,num.num" -> "num.num, num.num"
    ind<-grep("([0-9]\\.[0-9][0-9]*),(-*[0-9][0-9]*\\.[0-9])",x)
    if(length(ind)>0) {
      warning(paste("Spaces were added to the following listed numbers:",paste(x[ind],collapse="; ")),call.=FALSE)
      x[ind]<-gsub("([0-9]\\.[0-9][0-9]*),(-*[0-9][0-9]*\\.[0-9])","\\1, \\2",x[ind])
    }
    
    # correct dot-space-number->dot-number in front
    x<-gsub("^(-*) *\\. *([0-9])","\\1.\\2",x)
    
    # if has potential big mark Comma remove it
    if(isTRUE(correctComma)){
    patBM<-"[0-9],[0-9]{3}$|[0-9],[0-9]{3}[^0-9]"
    if(length(grep(patBM,x))>0){
      # except df in F-values
      patDF<-"^F[ _]*\\([1-9][\\.0-9]*,[1-9][\\.0-9,]*\\)|[- ]F[_ ]*\\([1-9][\\.0-9]*,[1-9][\\.0-9,]*\\)"
      i<-grep(patDF,x,invert=TRUE)
      if(length(i)>0){
        warning("One or more detected big mark commas were removed from big numbers (e.g. '10,000' -> '10000').",call.=FALSE)
        x[i]<-gsub("([0-9]),([0-9]{3})","\\1\\2",x[i])
      }
    }
    }
    ## comma to dot correction
    # in "F(num, num,3num)" -> "F(num, num.3num)"
    if(isTRUE(correctComma)){
      i<-grep("^F[ _]*\\([1-9][\\.0-9]*, *[1-9][0-9]*,[0-9][0-9][0-9]\\)|[- ]F[_ ]*\\([1-9][\\.0-9]*, *[1-9][0-9]*,[0-9][0-9][0-9]\\)",x)
      if(length(i)>0){
        x[i]<-gsub("(F[ _]*\\([1-9][0-9]*,) *([1-9][0-9]*),([1-9][0-9][0-9][0-9]\\))","\\1 \\2.\\3",x[i])
        warning(paste0("Detected comma as decimal in: '",
                       paste(x[i],collapse="; "),
                       "' were converted to dots. This may infere with numeric values above 999, that have comma as big mark (e.g.: 1,000)."),call.=FALSE)
        }
    
    # space adding in F(num,num)
      i<-grep("^F[ _]*\\([1-9][\\.0-9]*,[1-9][\\.0-9]*\\)|[- ]F[_ ]*\\([1-9][\\.0-9]*,[1-9][\\.0-9]*\\)",x)
      if(length(i)>0){
        x[i]<-gsub("^(F[ _]*\\([1-9][\\.0-9]*,)([1-9][\\.0-9]*\\))","\\1 \\2",x[i])
        x[i]<-gsub("([- ]F[ _]*\\([1-9][\\.0-9]*,)([1-9][\\.0-9]*\\))","\\1 \\2",x[i])
        warning(paste0("A space was added to F(number,number), assuming that degrees of freedom 1 and 2 are reported. This may infere with numeric values above 999, that have comma as big mark (e.g.: 1,000)."),call.=FALSE)
      }
    }
    
    # has Comma as decimal (obviously cases: 1,2,4 or more decimals, 0,number)
    patComma<-"[0-9],[0-9][^0-9]|[0-9],[0-9]$|[0-9],[0-9]{2}[^0-9]|[0-9],[0-9]{2}$|[0-9],[0-9][0-9][0-9][0-9][^0-9][0-9]*|[0-9],[0-9][0-9][0-9][0-9][0-9]*$|^[^0-9]*0*,[0-9]{3}|[^0-9,]0*,[0-9]{3}$"
    # has dot as decimal (obviously cases: 1,2,4 or more decimals, 0,number)
    patDot<-"[0-9]\\.[0-9][^0-9]|[0-9]\\.[0-9]$|[0-9]\\.[0-9]{2}[^0-9]|[0-9]\\.[0-9]{2}$|[0-9]\\.[0-9][0-9][0-9][0-9][^0-9][0-9]*|[0-9]\\.[0-9][0-9][0-9][0-9]|^[^0-9]*0*\\.[0-9]{3}|[^0-9]0*\\.[0-9]{3}$"
    # has F value with df
    patF<-"F *\\([1-9][0-9]*,[1-9][0-9]*\\)"
    
    if(length(grep(patComma,grep(patF,x,invert=TRUE,value=TRUE)))>0 & isTRUE(correctComma)) {
      warning(paste0("Detected comma as decimal in: '",
                     paste(grep(patComma,grep(patF,x,invert=TRUE,value=TRUE),value=TRUE),collapse="; "),
                     "' were converted to dots. This may infere with numeric values above 999, that have comma as big mark (e.g.: 1,000)."),call.=FALSE)
      x[grep(patF,x,invert=TRUE)]<-gsub(",([0-9])",".\\1",x[grep(patF,x,invert=TRUE)])
    }
    
    # warning message
    if(length(grep(patComma,grep(patF,x,invert=TRUE,value=TRUE)))>0&length(grep(patDot,x))>0){
      warning(paste0(paste0("There is an inconsistent use of decimal signs. Found for numeric value/s: '",
                     paste(grep(patComma,grep(patF,x,invert=TRUE,value=TRUE),value=TRUE),collapse="; ")),
      ifelse(isFALSE(correctComma), "'\nYou may consider to set the argument correctComma=TRUE to unify the decimal sign to dots.","")),call.=FALSE)
    }

    # x as vector
    x<-as.vector(x)
    
    # super and subscript
    x<-gsub("<sup>","^",x)
    x<-gsub("<sub>","_",x) 
    # lower asterix to *
    x<-gsub("\u204e","*",x) 
    
    ## space removal
    # " ^ "
    x<-gsub(" *\\^ *","^",x)
    # " / "
    x<-gsub(" */ *","/",x)
    # remove all other html brackets
    x<-gsub("</*[a-z][^>]*/*>","",x)
    # "*^*" to "**"
    x<-gsub("\\*\\^\\*","**",gsub("\\*\\^\\*","**",x))
    # unify minus/hyphen sign
    x<-gsub("\u2212|\u02D7|\u002D|\u2013","-",x)
    # superscripted 2
    x<-gsub("\u00b3","^2",x)
    
    ## clean up empty cells
    # set only dot, minus, or slash to empty
    if(isTRUE(na.rm)){
      x<-gsub("^[-\\./ ]*$","",x)
    }
    # clean up and unify
    x<-gsub("\\+ */* *- *","+-",x)
    x<-gsub("^- ([0-9\\.])","-\\1",x)
    x<-gsub("^\\+ ([0-9\\.])","\\1",x)

    x<-gsub("^([0-9\\.-][0-9\\.\\%]*)\\(","\\1 (",x)
    x<-gsub("([0-9])  *\\%","\\1%",x)
    x<-gsub("  *"," ",x)
    x<-gsub("df1([,;])df2","df1\\1 df2",x)
    
    x<-gsub(":$","",x)
    
    # remove space around operator number
    x<-gsub("([A-z2\\(\\[]) *([<=>][<=>]*) *(-*[0-9\\.-])","\\1\\2\\3",x)
    # remove space between operator number at start
    x<-gsub("^ *([<=>][<=>]*) *(-*[0-9\\.-])","\\1\\2",x)
    # add Comma between number/star-letter-operator
    x<-gsub("([0-9\\*])( [A-z][A-z]*[<=>][<=>]*[-0-9\\.])","\\1,\\2",x)
    if(isTRUE(letter.convert)) {
      x<-html2unicode(x)
      x<-JATSdecoder::letter.convert(x,greek2text=greek2text)
      }
    # convert exponents with *10^-num and products
    i<-grep("[0-9] *\\* *10\\^[- ]*[0-9]",x)
    x[i]<-JATSdecoder::text2num(x[i],exponent = TRUE,percentage = FALSE,fraction = FALSE, product = TRUE, words=FALSE)
    # remove space in front of ^
    x<-gsub(" \\^","^",x)
    
    # remove html @ references
    x<-gsub("@[a-z][^\\{]*\\{[^\\]*\\} *","",x)

    # remove space behind/in front brackets
    x<-gsub("\\(  *","(",x)
    x<-gsub("\\[  *","[",x)
    x<-gsub("  *\\)",")",x)
    x<-gsub("  *\\]","]",x)
    
    # remove tailoring spaces
    x<-gsub("  *$","",x)
    x<-gsub("^  *","",x)
    x<-gsub("  *"," ",x)
    
    # vector to matrix
    m<-matrix(x,ncol=nCol)
    
    # remove NA from inner matrix
    if(isTRUE(na.rm)){
      m[-1,-1]<-gsub("^[:punct:]*[Nn][:punct:]*[Aa][:punct:]*$","",m[-1,-1])
    }

    
    # remove empty rows/cols
    row.rm<-which(rowSums(m=="",na.rm=TRUE)==ncol(m))
    if(length(row.rm)>0) m<-m[-row.rm,]
    if(!is.matrix(m)){
      if(length(m)==0) return(NULL)
      return(matrix(m,nrow=1))
    }
    
    col.rm<-which(colSums(m=="",na.rm=TRUE)==nrow(m))
    if(length(col.rm)>0) m<-m[,-col.rm]
    if(!is.matrix(m)){
      if(length(m)==0) return(NULL)
      return(matrix(m,ncol=1))
    }
    
    # add attributes
    if(length(grep("caption|footer",names(unlist(attribs))))==2){
      attributes(m)$caption<-attribs$caption
      attributes(m)$footer<-attribs$footer
      attributes(m)$class<-attribs$class
    }
    
    
    # output
    return(m)
  }
  
  # apply function
  if(!is.list(x)){
    out<-fun(x,letter.convert=letter.convert,greek2text=greek2text,text2num=text2num,correctComma=correctComma,na.rm=na.rm)
    return(out)
    }
  
  if(is.list(x)){
    out<-lapply(x,fun,letter.convert=letter.convert,greek2text=greek2text,text2num=text2num,correctComma=correctComma,na.rm=na.rm)
    return(out)
    }
}
