#' parseMatrixContent
#' 
#' Parses character matrix content into a text vector. This is the basic function of tableParser, which is implemented in matrix2text(), table2text() and table2stats(). Row and column names are parsed to cell content with operators, that depend on the cell content. Numeric cells are parsed with "=", text cells with ":". Cells that start with an operator ('<', '=' or '>') are parsed without a seperator. Detected codings for (e.g. p-values, abbreviations) from tables legend text can be used to extend the tabled content to a fully written out form.
#' @param x A character matrix or list with a character matrix as first and only element.
#' @param legend The tables caption/footer notes as character vector.
#' @param standardPcoding Logical. If TRUE, and no other detection of p-value coding is detected, standard coding of p-values is assumed to be: * p<.05, ** p<.01 and *** p<.001.
#' @param expandAbbreviations Logical. If TRUE, detected abbreviations are expanded to label detected in table caption/footer with tableParser::legendCodings().
#' @param superscript2bracket Logical. If TRUE, detected superscript codings are inserted inside parentheses.
#' @param addDF Logical. If TRUE, detected sample size N in caption/footer is inserted as degrees of freedom (N-2) to r- and t-values that are reported without degrees of freedom. 
#' @returns A text vector with the parsed matrix content.
#' @export

parseMatrixContent<-function(x,legend=NULL,
                             standardPcoding=TRUE,
                             expandAbbreviations=TRUE,
                             superscript2bracket=FALSE,
                             addDF=TRUE){
  # escapes and preparation
  if(length(x)==0) return(NULL)
  if(is.list(x)&length(x)==1) x<-x[[1]]
  if(!is.matrix(x)) stop("Input must be a character matrix or list with a character matrix as first and only element. You may consider lapply(x,parseMatrixContent) to parse more than one matrix strored in a list.")
  
   
  # prepare codesFromLegend
  if(is.list(legend)) codesFromLegend<-legend
  if(!is.null(legend)&!is.list(legend)) codesFromLegend<-legendCodings(legend)
  if(is.null(legend)) codesFromLegend<-NULL
  
  # prepare legend codings
  parentheses<-NULL;brackets<-NULL;psign<-NULL;pval<-NULL;abbr<-NULL;label<-NULL
  sup<-NULL;sup_label<-NULL;italic<-NULL;bold<-NULL;N<-NULL;alpha<-NULL
  # get legend codings
  if(is.list(codesFromLegend)){
    parentheses<-codesFromLegend$parentheses
    brackets<-codesFromLegend$brackets
    psign<-codesFromLegend$psign
    pval<-codesFromLegend$pval
    abbr<-codesFromLegend$abbreviation
    label<-codesFromLegend$label
    italic<-codesFromLegend$italicP
    bold<-codesFromLegend$boldP
    alpha<-codesFromLegend$alpha
    N<-codesFromLegend$N
    sup<-codesFromLegend$superscript
    sup_label<-codesFromLegend$sup_label
  }
  
  # add coding for significant p in bold to pval
  if(length(bold)==1) 
    if(length(grep("p[<=>]",bold))==1)
      if(!is.element(bold,pval)){
        pval<-c(pval,bold)
        psign<-c(psign,"^bold")
     }
  if(length(italic)==1) 
    if(length(grep("p[<=>]",italic))==1)
      if(!is.element(italic,pval)){
        pval<-c(pval,italic)
        psign<-c(psign,"^italic")
      }
  
  ##############################################
  # remove duplicated 2nd column
  if(ncol(x)>1){
    if(sum(x[,1]==x[,2])==nrow(x)) x<-x[,-2]
    if(!is.matrix(x)) x<-as.matrix(x)
  }
  
  # extract first cell for later correction
  firstCell<-x[1,1]
  
  if(ncol(x)==1|nrow(x)==1){
    x<-parseContent(x)
    # add degrees of freedom
    if(isTRUE(addDF)){
      # add df=max(n)-2 for t and r values if has N= in legend
      if(!is.null(N)){
        # get highest N
        maxN<-suppressWarnings(max(suppressWarnings(as.numeric(gsub(".*[<=>]","",N))),na.rm=T))
        # add maxN-2 to r= and t= if no df is found
        i<-(grep(" df[12]*=|degrees* of freed",x,invert=TRUE))
        if(maxN!=-Inf) x[i]<-gsub(" r=",paste0(" r(",maxN-2,")="),x[i])
        if(maxN!=-Inf) x[i]<-gsub(" t=",paste0(" t(",maxN-2,")="),x[i])
        if(maxN!=-Inf) x[i]<-gsub(" T=",paste0(" T(",maxN-2,")="),x[i])
      }
    }
    return(x)
  } 
  
  # take a copy
  m<-x
  
  # parse columns with only punctuation to columns in front
  # if colname is duplicated
  i<-which(duplicated(m[1,]))
  i<-i[i>2]
  if(length(i)>0){
    sel<-NULL
    for(j in i) sel<-c(sel, length(grep("^$|^[[:punct:]][[:punct:]]*$",m[-1,j]))==nrow(m)-1)
    i<-i[sel]
    if(length(i)>0){
      for(j in i) m[-1,j-1]<-paste0(m[-1,j-1],m[-1,j])
      m<-m[,-i]
    }
  }
  
  
  # convert Cronbachs alpha values
  if(length(alpha)>0)
    m<-enter.CrAlpha(m,alpha)
  
  
  # remove empty lines/cols
  #row.rm<-which(rowSums(m=="",na.rm=TRUE)==ncol(m))
  #col.rm<-which(colSums(m=="",na.rm=TRUE)==nrow(m))
  #if(length(row.rm)>0) m<-m[-row.rm,]
  #if(!is.matrix(m)) return(m)
  #if(length(col.rm)>0) m<-m[,-col.rm]
  #if(!is.matrix(m)) return(m)
  
  # escape if is not matrix with at least 2 rows and 2 cols
  if(ncol(m)==1) return(paste0(m[,1],collapse=", "))
  if(nrow(m)==1) return(paste0(m[1,],collapse=", "))
  
  # Classify table  
  class<-tableClass(m,legend=legend)
  
  # return parsed vector 
  if(class=="vector"){
    m<-parseContent(m)
    # add degrees of freedom
    if(isTRUE(addDF)){
      # add df=max(n)-2 for t and r values if has N= in legend
      if(!is.null(N)){
        # get highest N
        maxN<-suppressWarnings(max(suppressWarnings(as.numeric(gsub(".*[<=>]","",N))),na.rm=T))
        # add maxN-2 to r= and t= if no df is found
        i<-(grep(" df[12]*=|degrees* of freed",m,invert=TRUE))
        if(maxN!=-Inf) m[i]<-gsub(" r=",paste0(" r(",maxN-2,")="),m[i])
        if(maxN!=-Inf) m[i]<-gsub(" t=",paste0(" t(",maxN-2,")="),m[i])
        if(maxN!=-Inf) m[i]<-gsub(" T=",paste0(" T(",maxN-2,")="),m[i])
      }
    }
    return(m) 
  }
  
  ########################################
  # remove lines with no values in cor matrices
#  if(class=="correlation"){
#    i<-(rowSums(m[,]=="")!=(ncol(m)-1)&!grepl("^[1-9]",m[,1]))
#    m<-m[i,]
#  }
  
  # remove categorizing lines in correlation table
  if(class=="correlation"){ 
    if(nrow(m)>2&ncol(m)>2){
      i<-which(rowSums(m[-1,-1]=="")==(ncol(m)-1)&!grepl("^[1-9]",m[-1,1]))
      if(length(i)>0) m<-m[-(i+1),]
    }
  }
  
  
  if(class!="text"&class!="vector"){
    if(nrow(m)>1&ncol(m)>1){
    # paste text from lines with only the same value to first cells
    m<-multiHeaderSplit(m,split=FALSE,class=class)
    # handle empty cells by row
    m<-rowHandling(m)
    # collapse header rows
    m<-headerHandling(m)
    # paste first text columns to one column
    #m<-textColHandling(m)
   }
  }
  
  if(class!="text"&class!="vector"){
    # create new columns for brackets and percent
    m<-newColumnBracket(m)
    m<-newColumnCI(m)
    m<-percentHandler(m)
    
    # Re-classify table
    #class<-tableClass(m,legend=legend)
  }
  

  if(class=="correlation"){
    # parse first and second column, if first col has no letters and second column starts with letter in every cell
    if(length(grep("[A-z]",m[-1,1]))==0&length(grep("^[A-z]",m[-1,2]))==(nrow(m)-1)){
      m[,1]<-paste(m[,1],m[,2])
      m<-m[,-2]
    }
    # parse first and second row, if first row has no letters and second row starts with letter in every cell
    if(length(grep("[A-z]",m[1,-1]))==0&length(grep("^[A-z]",m[2,-1]))==(ncol(m)-1)){
      m[1,]<-paste(m[1,],m[2,])
      m<-m[-2,]
    }
  }
  
  if(!is.matrix(m)) m<-as.matrix(m)
  
  # convert numbers in brackets in inner matrix
  if(length(parentheses)>0) m[-1,-1]<-bracket2value(m[-1,-1],parentheses,"parentheses",sep=";;")
  if(length(brackets)>0) m[-1,-1]<-bracket2value(m[-1,-1],brackets,"brackets",sep=";;")

  # add standard p-coding
  if(standardPcoding==TRUE&length(pval)==0){
    psign<-c("***","**","*")
    pval<-c("p<.001","p<.01","p<.05")
  }
  
  # convert p signs in inner matrix
  if(length(pval)>0) 
    m[-1,-1]<-sign2p(m[-1,-1],psign,pval,sep=";;")
  
  # convert p stars in outer matrix with result^**
  if(length(pval)>0) {
    # first column
    i<-grep("[A-z][<=>][<=>]-*[\\.0-9]*[0-9]\\^*\\*",m[,1])
    if(length(i)>0) m[i,1]<-sign2p(m[i,1],psign,pval,sep=";;")
    # first row
    i<-grep("[A-z][<=>][<=>]-*[\\.0-9]*[0-9]\\^*\\*",m[1,])
    if(length(i)>0) m[1,i]<-sign2p(m[1,i],psign,pval,sep=";;")
    }
  # bold p
  if(length(grep("p[<=>]",bold))>0){
    m[-1,-1]<-gsub("(-*[\\.0-9][\\.0-9]*)\\^bold",paste("\\1;;",bold),m[-1,-1])
  }
  # italic p
  if(length(grep("p[<=>]",italic))>0){
      m[-1,-1]<-gsub("(-*[\\.0-9][\\.0-9]*)\\^italic",paste("\\1;;",italic),m[-1,-1])
    }
  
  # remove superscripted bold and italic text
  m<-gsub("\\^italic","",m)
  m<-gsub("\\^bold","",m)
  
  # convert abbreviations in first row and col
  if(expandAbbreviations=="TRUE"){
    if(is.matrix(m)){
      m[1,]<-abb2text(m[1,],abbr=abbr,label=label)
      m[,1]<-abb2text(m[,1],abbr=abbr,label=label)
    }
  }
  
  # repeat cells in first column if there are empty cells
  if(class!="vector"){
    if(sum(m[-1,1]=="",na.rm=TRUE)>0&length(grep("^[1-9]",m[-1,1]))==0) for(i in which(m[-1,1]==""))  m[i+1,1]<-m[i,1]
    # handle empty cells by column
    m<-colHandling(m)
  }
  
  # reclassify table  
  class<-tableClass(m,legend=legend)
#  print(class)

  # expand detected abbreviations
  if(expandAbbreviations=="TRUE"){
    if(is.matrix(m)){
      m<-abb2text(m,abbr=abbr,label=label)
    }
  }
  # expand detected superscripts
  if(superscript2bracket=="TRUE"){
    if(is.matrix(m)){
      m<-sup2text(m,sup=sup,sup_label=sup_label)
    }
  }
  
  ####################################################################
  ## return parsed content if is text matrix
  if(class=="text"|class=="vector"){
    m<-parseContent(m)
    # add degrees of freedom
    if(isTRUE(addDF)){
      # add df=max(n)-2 for t and r values if has N= in legend
      if(!is.null(N)){
        # get highest N
        maxN<-suppressWarnings(max(suppressWarnings(as.numeric(gsub(".*[<=>]","",N))),na.rm=T))
        # add maxN-2 to r= and t= if no df is found
        i<-(grep(" df[12]*=|degrees* of freed",m,invert=TRUE))
        if(maxN!=-Inf) m[i]<-gsub(" r=",paste0(" r(",maxN-2,")="),m[i])
        if(maxN!=-Inf) m[i]<-gsub(" t=",paste0(" t(",maxN-2,")="),m[i])
        if(maxN!=-Inf) m[i]<-gsub(" T=",paste0(" T(",maxN-2,")="),m[i])
      }
    }
  
    
    return(m)
    } 
  
  ## else go on:
  # as long as multiple fields in second row contain only text, paste to cells in first row and remove row
    while(
    sum(grepl("[0-9]",gsub("[1-9][0-9\\.][- ]*%","",m[2,]))) == 0 & 
    sum(grepl("[A-z]",m[2,])) > 1 & 
    nrow(m) > 2 & 
    # no line with only the same values
    sum(duplicated(m[2,]))!= ncol(m) & 
    # and at least 30% of all other fields contain numbers 
    sum(grepl("[0-9]|^$",m[-1:-2,-1]))/length(m[-1:-2,-1])>.3
  ){
    m[2,][m[1,]==m[2,]]<-""
    m[1,]<-gsub("^  *|  *$","",paste(m[1,],m[2,]))
    m<-m[-2,]
  }

  ###################################################################
  ## specific correlation table handling
  correlations<-NULL
  if(class=="correlation"){
    
    # extract correlations as vector
    correlations<-extractCorrelations(m,legendCodes=legend,remove=FALSE)
    # table without correlations
    m<-extractCorrelations(m,legendCodes=legend,remove=TRUE)
    # set first cell to ""
    if(length(m)>0) m[1,1]<-""
    
    # rotate if nrow<ncol to better read out descriptive  stats
    if(length(m)>0) 
      if(nrow(m)<(ncol(m)/2))
        m<-t(m)
    
    
    # add df to extracted correlations
    if(isTRUE(addDF)){
    # add df=max(n)-2 if has N in legend
    if(!is.null(N)){
      # get highest N
      maxN<-suppressWarnings(max(suppressWarnings(as.numeric(gsub(".*[<=>]","",N))),na.rm=T))
      # add maxN-2 to r=
      if(maxN!=-Inf) correlations<-gsub("r=",paste0("r(",maxN-2,")="),correlations)
    }
      }
    
  } # results are in updated "m" and object "correlations"

  
  # if first cell contains result create new line and move result to first cell in new line
  if(is.matrix(m)) {
    if(nrow(m)>2){
      if(length(grep("[<=>]-*[\\.0-9]",m[1,1]))==1){
        m<-rbind(m[1,],rep("",ncol(m)),m[2:nrow(m),])
        m[2,1]<-m[1,1]
        m[1,1]<-""
      }}
  }
  
  
  i<-NULL;j<-NULL
  if(length(m)>0&is.matrix(m)){
  # convert ns -> p>.05
  if(standardPcoding==TRUE) m<-gsub(" [Nn]\\.*[Ss]\\.*$",";; p>.05",m)
  if(standardPcoding==TRUE) m<-gsub("^[Nn]\\.*[Ss]\\.*$",";; p>.05",m)
  # add non significant p-values in columns that have cells with coded p-values,
  # but exclude lines from model statistics/residuals
  i<-grep("^R2$| R2|R\\^2|R[- ][Ss]q|[Rr]esidual|AIC|BIC|[Ii]nformation [Cr]iter|chi\\^2|degrees* of freedom|^df$|^DF$|[Ll]ikelihood",m[,1])
  if(length(i)>0) i<-1:(min(i[1])-1)
  if(length(i)==0) i<-1:nrow(m)
  
  # which of these lines have imputed p-values
  j<-max(i)+which(rowSums(matrix(grepl(";; p[<=>]",m[-i,]),ncol=ncol(m)))>0)
  # impute colwise p>max(p) 
  m[i,]<-noSign2p(m[i,],pval=pval)
  # row wise in model stats
  if(length(j)>0) m[j,]<-t(noSign2p(t(as.matrix(m[c(1,j),])),pval))[-1,]
  # remove inserted grouping from Model stats
  m[-i,1]<-gsub("^[^,;]*: ","",m[-i,1])
  }
  
  # which line has number-star in model stats
  #if(is.matrix(m[-i,]))
  #j<-rowSums(matrix(grepl("[0-9]\\*|[0-9]\\^[^0-9]",m[-i,]),ncol=ncol(m)))>0
  # no sign to p-value in these lines
  #if(sum(j)>0) m[i,][j,]<-noSign2p(m[i,][j,],pval=pval)
  
  # expand detected superscripts
  if(superscript2bracket=="TRUE"){
    if(is.matrix(m)){
      m<-sup2text(m,sup=sup,sup_label=sup_label)
    }
  }
  
  # paste model and standard statistics in first col to numeric field behind
  m<-modelStatsHandler(m)
  
  ## create vector with parsed results
  parsed<-NULL
  parsed<-parseContent(m)
  
  # combine parsed results and correlations 
  output<-c(parsed,correlations)
  
  # remove dummy text
  output<-gsub("TEMP_TEXT, ","",output)
  firstCell<-"Variable"
  output<-gsub(paste0("^(",specialChars(firstCell),": [^:]*: )",specialChars(firstCell),": "),"\\1",output)
  
  # convert abbreviations in first row and col
  #output<-abb2text(output,abbr=abbr,label=label)
  
  # escape if no stats are detected
  if(length(output)==0) return(NULL)
  # remove "[:;=,] " at start
  output<-gsub("^ *[:;=,] *","",output)
  # remove second sign
  output<-gsub(", [:;=,] *",", ",output)
  output<-gsub("  *"," ",output)
  
  #output<-unifyStats(output)
  
  # add degrees of freedom
  if(isTRUE(addDF)){
    # add df=max(n)-2 for t and r values if has N= in legend
    if(!is.null(N)){
      # get highest N
      maxN<-suppressWarnings(max(suppressWarnings(as.numeric(gsub(".*[<=>]","",N))),na.rm=T))
      # add maxN-2 to r= and t= if no df is found
      i<-(grep(" df[12]*=|degrees* of freed",output,invert=TRUE))
      if(maxN!=-Inf) output[i]<-gsub(" r=",paste0(" r(",maxN-2,")="),output[i])
      if(maxN!=-Inf) output[i]<-gsub(" t=",paste0(" t(",maxN-2,")="),output[i])
      if(maxN!=-Inf) output[i]<-gsub(" T=",paste0(" T(",maxN-2,")="),output[i])
    }
  }
  
  return(output)
} # end parseMatrixContent()

# paste name with content if number is present and does not contain = num
parseContent<-function(x){
  # escapes
  if(length(x)==0) return(NULL)
  if(length(unlist(x))==0) return(NULL)
  
  if(is.vector(x)) return(paste(x,collapse="; "))
  if(!is.matrix(x)) return(x)
  
    class<-tableClass(x)
  # take a copy
  m<-x
  out<-NULL
  if(nrow(m)>1){
    for(j in 2:nrow(m)){
      empty<-grepl("^$",m[j,])
      # paste with "=", ",", ":" or no sign
      out[j]<-paste0(m[1,!empty],
                     # if starts with number and not "number-letter" or "number-dot-space -> "="
                     ifelse(grepl("^[-\\.0-9][\\.0-9]*",m[j,!empty]) &
                              !grepl("^[0-9][\\.0-9]* *- *[A-z]|^[0-9][0-9]*[\\.] ",m[j,!empty])
                            ,"=",
                            # if starts with operator-number -> "no sign"
                            ifelse(grepl("^[<>=][<>=]* *[-\\.0-9]",m[j,!empty]),"",
                                   #else ":"
                                   ": ")),
                     #m[j,!empty])),"=","="), # always paste with "="
                     m[j,!empty],
                     # collapse with "," or "; if class=="text"
                     collapse=ifelse(class=="text","; ",", "))
    }
    
  }else{
    out<-paste0(m[1,],collapse=", ")
  }
  out<-out[which(!is.na(out))]
  # clean up
  # remove "[:, ] " at start
  out<-gsub("^ *[:,;=] *","",out)
  # further clean up ", : "
  out<-gsub(", : ",", ",out)
  out<-gsub("^  *","",out)
  out<-gsub(" : ",": ",out)
  out<-gsub(": [,;=] ",": ",out)
  out<-gsub("([^0-9]) %=([0-9\\.][0-9\\.]*)[^%]*","\\1 \\2%\\3",out)
  out<-gsub("% *%","%",out)
  
  return(out)
  
}
