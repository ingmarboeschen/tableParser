## Matrix processing functions
#############################

# convert special characters for replacement
specialChars<-function(x){
  i<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",x))
  i<-gsub("\\]","\\\\]",gsub("\\[","\\\\[",i))
  i<-gsub("\\+","\\\\+",gsub("\\$","\\\\$",i))
  i<-gsub("\\?","\\\\?",gsub("\\&","\\\\&",i))
  i<-gsub("\\|","\\\\|",gsub("\\*","\\\\*",i))
  i<-gsub("\\^","\\\\^",gsub("\\.","\\\\.",i))
  return(i)
}

# convert coding of Cronbachs alpha
enter.CrAlpha<-function(x,coding){
  if(length(grep("italic",coding))==1) 
    x<-gsub("(-*[0-9\\.]*[0-9])\\^italic","Cronb. alpha=\\1",x)  
  if(length(grep("bold",coding))==1) 
    x<-gsub("(-*[0-9\\.]*[0-9])\\^bold","Cronb. alpha=\\1",x)  
 # if(length(grep("diagonal",coding))==1) 
    # to be done;:
    x
  return(x)
}


# split at lines that are text between numeric lines
multiTextRowSplit<-function(x){
  temp<-matrix(grepl("^[0-9][0-9]*$",gsub("\\^.*|[[:punct:]]| ","",x[,-1])),ncol=ncol(x)-1)
  i<-which(rowSums(temp)==0)
  i<-i[i>2]
  i<-i[i<nrow(x)]
  if(length(i)>0) x<-rowSplit(x,i-1)
  return(x)
}

# split matrix by row
rowSplit<-function(x,split=NULL,headerRows=1){
  if(length(split)==0) return(x)
  if(max(headerRows)>= min(split)){
    #warning("split cannot be performed within headerRows.")
    return(x)
  }
  
  fun<-function(x,headerRows=1,split=NULL){
    if(!is.matrix(x)) return(x)
    # extract header and prepare sequences for line selection
    header<-x[headerRows,]
    start<-c(max(headerRows)+1,split+1)
    end<-c(split,nrow(x))
    
    out<-list()
    for(i in 1:(length(split)+1)){
      part<-x[start[i]:end[i],]
      out[[i]]<-rbind(header,part,deparse.level = 0)
    }
    return(out)
  }
  
  if(is.matrix(x)) return(fun(x,headerRows=headerRows,split=split))
  if(is.list(x)) return(lapply(x,fun,headerRows=headerRows,split=split))
  
}

# split matrix by column
colSplit<-function(x,split=NULL,headerCols=1){
  if(length(split)==0) return(x)
  if(max(headerCols)>= min(split)){
   # warning("split cannot be performed within headerCols.")
    return(x)
  }
  
  fun<-function(x,headerCols=1,split=NULL){
    if(!is.matrix(x)) return(x)
    # extract cols to reapeat and prepare sequences for col selection
    header<-x[,headerCols]
    start<-c(max(headerCols)+1,split+1)
    end<-c(split,ncol(x))
    
    out<-list()
    for(i in 1:(length(split)+1)){
      part<-x[,start[i]:end[i]]
      out[[i]]<-cbind(header,part,deparse.level = 0)
    }
    return(out)}
  
  if(is.matrix(x)) return(fun(x,headerCols=headerCols,split=split))
  if(is.list(x)) return(lapply(x,fun,headerCols=headerCols,split=split))
  
}

# split matrix at dublicated values in row x
duplicatedColSplit<-function(x,row=1){
  row<-x[row,]
  i<-which(duplicated(row)&c(FALSE,x[2:length(row)]!=x[1:(length(row)-1)]))
  i<-i[i>2]
  i<-i[1]
  if(length(i)>0) if(!is.na(i)) {
    i<-which(row==row[i])
    x<-colSplit(x,i-1)}
  return(x)
}

# handling of multiple tables within a table-wrap, splits input at <table>-tag and repeat <table-wrap> for every table
multiTable<-function(x){
  # function for single cell with table
  temp<-function(x){
    # if has <table-wrap>
    if(length(grep("<table-wrap",x))>0){
      # split at <table.
      y<-strsplit2(x,"<table[- >]",type="before")
      # if has <table-wrap> with  more than 1 <table>
      if(length(grep("<table-wrap",y[[1]][1]))==1 &
         length(grep("<table[ >]",y[[1]]))>1){
        wrapHead<-grep("<table-wrap[ >]",y[[1]],value=TRUE)
        wrapFoot<-grep("<table-wrap-foot",y[[1]],value=TRUE)
        tables<-gsub("</table-wrap>|<table-wrap/>","",grep("<table[ >]",y[[1]][-1],value=TRUE))
        x<-paste0(wrapHead,tables,wrapFoot)
      }}
    return(x)
  }  
  # convert every cell and paste results, input=output if no conversion was performed
  out<-NULL
  for(i in 1:length(x)) out<-c(out,temp(x[i]))
  return(out)
}


# split matrix at rows that have only the same value
multiHeaderSplit<-function(x,split=FALSE,class=NULL){
 if(!is.matrix(x)) return(x)
  at<-NULL
 if(length(nrow(x))>0){
 if(ncol(x)<2) return(x)
  # detect lines with only the same value
  for(i in 1:nrow(x)) at[i]<-sum(x[i,]==x[i,1])==length(x[i,])
  # and remove those that are also part in col names
  # only proceed if has line to split at
  if(sum(at)>0){
  # convert all except first cell to ""
  x[at,-1]<-"" 
  # detect lines with only an entry in first cell 
  for(i in 1:nrow(x)) at[i]<-sum(x[i,-1]=="")==(ncol(x)-1)
  # and set to FALSE if TRUE follows TRUE
  if(length(at)>1){ 
    for(k in 1:(length(at)-1))
      at[k+1]<-ifelse(isTRUE(at[k])&isTRUE(at[k+1]),FALSE,at[k+1])
  }
  at
  # collapse lines in case of a sequence of lines 
  s<-NULL
  for(i in 1:(length(at)-1)) s[i]<-at[i]&at[i+1]
  # paste to first cells in following lines
  if(sum(s)>0){
    stop<-FALSE
    while(stop==FALSE){
      # paste cells
      x[which(s)[1],1]<-paste0(x[which(s)[1],1],": ",x[which(s)[1]+1,1])
      # reduce matrix
      x<-x[-(which(s)[1]+1),]
      # detect lines with only an entry in first cell 
      at<-NULL
      for(i in 1:nrow(x)) at[i]<-sum(x[i,-1]=="")==(ncol(x)-1)
      s<-NULL
      for(i in 1:(length(at)-1)) s[i]<-at[i]&at[i+1]
      if(sum(s)==0) stop<-TRUE
    }
  }
  }
  
  at<-NULL
  # detect lines with only an entry in first cell 
  for(i in 1:nrow(x)) at[i]<-sum(x[i,-1]=="")==(ncol(x)-1)
  at
  # set first cell in second row to FALSE if cell and following cells start with a number
  if(class=="correlation") if(length(at)>2) if(length(grep("^.*[123]",x[2:min(c(4,nrow((x)))),1]))>1) at[2]<-FALSE
  at
  # remove cases where the label also exists in col names
  at[is.element(
    gsub("^([0-9][0-9]*)\\.*.*","\\1",x[,1]),
    gsub("^([0-9][0-9]*)\\.*.*","\\1",x[1,-1]))]<-FALSE
  
  # set cells with numeric results to FALSE
  at<-!grepl("[<=>]-*[\\.0-9]",x[,1])&at
  
  # set first and last row to FALSE
  at[1]<-FALSE
  at[length(at)]<-FALSE
  # and set to FALSE if TRUE follows TRUE
  if(length(at)>1){ 
    for(k in 1:(length(at)-1))
      at[k+1]<-ifelse(isTRUE(at[k])&isTRUE(at[k+1]),FALSE,at[k+1])
  }
  at
  
  
  
  # paste first cell to lines below
  if(sum(at>0)){
  index<-which(at)
  for(i in 1:length(index)){
  if(i!=length(index)) x[(index[i]+1):(index[i+1]-1),1]<- paste0(x[index[i],1],": ",x[(index[i]+1):(index[i+1]-1),1])
  if(i==length(index)) x[(index[i]+1):nrow(x),1]<- paste0(x[index[i],1],": ",x[(index[i]+1):nrow(x),1])
  }
  nRem<-sum(at)
  
  splits<-which(at)-seq(1:nRem)
  splits<-splits[splits>1]
  # remove lines
  x<-x[-which(at),]
  
  # split at splits
  if(split==TRUE&length(splits)>0) x<-rowSplit(x,  splits)
   }
 }
  return(x)

}


#########################
## function to unify textual output
unifyOutput<-function(x){
  # unify minus/hyphen sign
  x<-gsub("\u2212|\u02D7|\u002D|\u2013","-",x)
  # unify beta 
  x<-gsub("\u00DF|\u03b2","b",x)
  x<-gsub("\u0392","B",x)
  # unify eta/omega 
  x<-gsub("\u03b7","eta",x)
  x<-gsub("\u03c9","omega",x)
  
  # Standard error -> SE
  out<-gsub("[Ss]t\\.*a*n*d*\\.*a*r*d* [Ee]rr\\.*o*r*s*","SE",x)
  out<-gsub("[Ss]td\\. [Ee]rr\\.*o*r*s*","SE",out)
  out<-gsub("S\\.E\\.([^A-Z]*)","SE\\1",out)
  #  out<-gsub("([^A-Z]) SE[_ ]*[^0-9]*([<=>][<=>]*[-\\.0-9]*)","\\1 SE\\2",out)
  # Standard deviation -> SD
  out<-gsub("[Ss]t\\.*a*n*d*\\.*a*r*d* [Dd]eviations*","SD",out)
  out<-gsub("[Ss]td\\. [Dd]eviations*|STDEV","SD",out)
  out<-gsub("S\\.D\\.([^A-Z]*)","SD\\1",out)
  #  out<-gsub("([^A-Z]) SD[_ ]*[^0-9]*([<=>][<=>]*[-\\.0-9]*)","\\1 SD\\2",out)

  # degrees of freedom -> df
  out<-gsub(" D[Ff] *[=]| degrees* of freedom *[=]"," df=",out)
  
  # unify lower/upper bound CI
  out<-gsub("([^%]) CI ([1-9][0-9]%) ","\\1 \\2 CI ",out)
  out<-gsub("([0-9]%)(CI)","\\1 \\2",out)
  out<-gsub("[Ll]ower bound[,;:=]* *[Uu]pper bound[,;:=]* *([0-9\\.-][0-9\\.]*) ([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  out<-gsub("[Ll]ower[,;:=]* *[Uu]pper[,;]* ([0-9\\.-][0-9\\.]*) ([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  out<-gsub("[Uu]pper bound[,;:=]* *[Ll]ower bound[,;:=]* *([0-9\\.-][0-9\\.]*) ([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  out<-gsub("[Uu]pper[,;:=]* *[Ll]ower[,;]* ([0-9\\.-][0-9\\.]*) ([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  
  out<-gsub("[Ll]ower bound[,;:=]* *([0-9\\.-][0-9\\.]*)[,;:=]* *[Uu]pper bound[,;:=]* *([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  out<-gsub("[Ll]ower[,;:=]* *([0-9\\.-][0-9\\.]*)[,;:=]* *[Uu]pper[,;:=]* *([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  out<-gsub("[Uu]per bound[,;:=]* *([0-9\\.-][0-9\\.]*)[,;:=]* *[Ll]ower bound[,;:=]* *([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  out<-gsub("[Uu]pper[,;:=]* *([0-9\\.-][0-9\\.]*)[,;:=]* *[Ll]ower[,;:=]* *([0-9\\.-][0-9\\.]*)","[\\1; \\2]",out)
  
  # unify to bracket with num; num
  out<-gsub("CI[:,; =]*([-\\.0-9][\\.0-9]*) *[,;-] *([-\\.0-9][\\.0-9]*)","CI: [\\1; \\2]",out)
  
  #convert "letter=letter" -> "letter: letter"  
  out<-gsub("([A-z])=([A-z])","\\1: \\2",out)
  # remove "=" at beginning
  out<-gsub("^=","",out)
  
  # capital p to p
  out<-gsub(" P( *[<=>])"," p\\1",out)
  # unify p
  out<-gsub(" [pP]: (p[<=>])"," \\1",out)
  out<-gsub(" [pP][- ][Vv]alue: (p[<=>])"," \\1",out)
  # remove 's
#  out<-gsub("  *"," ",gsub("'s","",out))
  # remove stars
#  out<-gsub("[\\*][\\*]*","",out)
  # correct %%
  out<-gsub("[\\%][\\%]*","%",out)
  
  return(out)
}


# header handling
headerHandling<-function(m){
  if(!is.matrix(m)) return(m)
  if(nrow(m)==1) return(m)
  if(ncol(m)==1) return(m)
  #collapse first two lines if first two cells in first column are empty 
  loop<-TRUE
  while(loop==TRUE & m[1,1]==""&m[2,1]==""){
  if(nrow(m)>2) if(m[1,1]==""&m[2,1]==""){
    m[2,][m[1,]==m[2,]]<-""
    m[1,-1]<-paste0(m[1,-1]," ",m[2,-1])
    m<-m[-2,]
  }
  if(nrow(m)==2) loop<-FALSE
  }
  
  #collapse first two lines if second cell in first column is empty 
  loop<-TRUE
  while(loop==TRUE & m[1,1]!=""&m[2,1]==""){
    if(nrow(m)>2) if(m[1,1]!=""&m[2,1]==""){
      m[2,][m[1,]==m[2,]]<-""
      m[1,-1]<-gsub("  "," ",paste0(m[1,-1]," ",m[2,-1]))
      m<-m[-2,]
    }
    if(nrow(m)==2) loop<-FALSE
  }
  
  #collapse first two lines if first two rows have character and last row numeric and second row does not only contain the same value 
  while(nrow(m)>2 & ncol(m)>1 & 
        length(grep("[A-z]|^$",gsub("\\^[A-z]*","",m[1,])))==ncol(m) &
        length(grep("[A-z]|^$",gsub("\\^[A-z]*","",m[2,])))==ncol(m) &
        sum(is.element( m[2,-1],m[2,1]))!=length(m[2,-1]) &
        sum(is.element( m[2,-1],""))!=length(m[2,-1]) &
        length(grep("[0-9]",m[nrow(m),-1])>0) ){
      m[1,]<-gsub("  "," ",paste0(m[1,]," ",m[2,]))
      m<-m[-2,]
    }

  # if second row has enumeration only parse to first row
  if(nrow(m)>2 & 
        length(grep("[A-z]|^$",m[1,]))==ncol(m) &
        length(grep("^1\\.*$",m[2,]))==1 & length(grep("^2\\.*$",m[2,]))==1 &
        length(grep("[0-9]",m[nrow(m),-1])>0) ){
    if((grep("^1\\.*$",m[2,])[1]+1)==grep("^2\\.*$",m[2,])[1]){
      m[1,]<-gsub("  "," ",paste0(m[1,]," ",m[2,]))
      m<-m[-2,]
    }
  }
  
  #collapse first two lines if second row has %-CI
  while(nrow(m)>2 & 
        length(grep("[A-z]|^$",m[1,]))==ncol(m) &
        length(grep("[89][059]\\.*9*%[- ]CI|[89][059]\\.*9*%[- ]confidence",m[2,]))>=1){
    m[1,]<-gsub("  "," ",paste0(m[1,]," ",m[2,]))
    m<-m[-2,]
  }
  # remove tailoring white spaces
  m<-gsub("^  *|  *$","",m)
  return(m)
  }

textColHandling<-function(x){
  if(!is.matrix(x)) return(x)
  # paste first and second column with ", "
  # if second column doesn't contain any number or letters in every cell
  loop<-TRUE
  while((loop==TRUE & length(grep("[0-9]",x[,2]))==0)|
        (loop==TRUE & length(grep("[A-z]",x[,2]))==nrow(x))
        ){
    if(ncol(x)>2){ 
      x[,2][x[,1]==x[,2]]<-""
      x[,1]<-gsub(" ( )|,( )$|^, ","\\1\\2",paste0(x[,1],", ",x[,2]))
      x<-x[,-2]}
    if(ncol(x)<=2) loop<-FALSE
  }
  return(x)
}

##################################################################################################
# handle rows with only the same values (except in correlation tables)
# from:    to:
# X Y Z    X Y Z
# A A A    A 1 2
#   1 2    A 3 4
#   3 4    b 5 6
# b b b 
#   5 6
rowHandling<-function(x){ 
  m<-x
  if(!is.matrix(m)) return(m)
  if(nrow(m)>1 & ncol(m)>1){
    ind<-rep(TRUE,nrow(m))
    for(i in 1:nrow(m)) ind[i]<-sum(duplicated(m[i,]))<(length(m[i,])-1)
    # paste content to cells below row with only the same value, if is not a table with only correlations
    isCor<-suppressWarnings(as.numeric(m[-1,-1]))
    isCor<-isCor>=(-1)|isCor<=1
    isCor[is.na(isCor)]<-FALSE
    if(sum(!ind)>0&length(which(!isCor))>0){
      for(j in 1:length(which(!ind))){
        if(j<sum(!ind)) m[(which(!ind)[j]+1):(which(!ind)[j+1]-1),1]<-paste(m[which(!ind)[j],1],m[(which(!ind)[j]+1):(which(!ind)[j+1]-1),1],sep=": ")
        if(j==sum(!ind)& which(!ind)[j]<length(ind)) 
          m[(which(!ind)[j]+1):length(!ind),1]<-
            paste(m[which(!ind)[j],1],m[(which(!ind)[j]+1):(length(!ind)),1],sep=": ")
      }
      # and remove rows with only the same value
      m<-m[ind,]
    }
    m[,1]<-gsub("^: |: $","",m[,1])
    
    
  }
  return(m)
}

###########################################################################################
# collapse empty cells in row with cell in next row:
# from:    to:
# A B C    A B C
# 1   2    1 3 2
#   3
colHandling<-function(m){
  if(nrow(m)>2){
    nCollapsed<-0
    empty<-m==""
    if(sum(empty,na.rm=TRUE)>0)
      for(i in 2:(nrow(m)-1)){
        row<-empty[i-nCollapsed,]!=empty[i-nCollapsed+1,]
        if(sum(row,na.rm=TRUE)==length(row)){
          m[i-nCollapsed,]<-paste0(m[i-nCollapsed,],m[i-nCollapsed+1,])
          m<-m[-(i-nCollapsed+1),]
          empty<-m==""
          nCollapsed<-nCollapsed+1
        }
      }
  }
  return(m)
}

## convert numeric codings of variables in correlation matrices
coding2variable<-function(m){
  # if coding is in first row
  if(sum(nchar(m[1,]),na.rm=TRUE)>sum(nchar(m[,1]),na.rm=TRUE)){
    # remove brackets around enumeration
    m[1,]<-gsub("^\\(([0-9][0-9]*\\.*)\\)","\\1",m[1,])
    m[,1]<-gsub("^\\(([0-9][0-9]*\\.*)\\)","\\1",m[,1])
    # move number in brackets at end to front
    m[1,]<-gsub("(.*) \\(([0-9][0-9]*\\.*)\\)$","\\2 \\1",m[1,])
    m[,1]<-gsub("(.*) \\(([0-9][0-9]*\\.*)\\)$","\\2 \\1",m[,1])
    
    fullNames<-m[1,] 
    # get numeric coding and varNames
    coding<-gsub("^([0-9][0-9]*).*|^[^:]*: ([0-9][0-9]*).*","\\1\\2",fullNames)
    coding[grep("^[0-9]",coding,invert=TRUE)]<-""
    varNames<-gsub("^[0-9][^A-z]*([A-z])|^[^:]*: [0-9][^A-z]*([A-z])","\\1\\2",fullNames)
    
    codes<-varNames[coding!=""]
    coding<-coding[coding!=""]
    rowname<-m[,1]
    
    if(sum(is.element(c(1,2,3),gsub("^0","",coding)))==3&
       sum(nchar(coding))!=0){
      #colname<-gsub("[^0-9]*","",colname)
      for(i in 1:length(coding)){
        rowname<-gsub(coding[i],codes[i],rowname)
      }
      m[,1]<-rowname
      m[1,]<-varNames  
    }
  }
  
  # if coding is in first column
  if(sum(nchar(m[1,]),na.rm=TRUE)<sum(nchar(m[,1]),na.rm=TRUE)){
    # remove brackets around enumeration
    m[1,]<-gsub("^\\(([0-9][0-9]*\\.*)\\)","\\1",m[1,])
    m[,1]<-gsub("^\\(([0-9][0-9]*\\.*)\\)","\\1",m[,1])
    
    fullNames<-m[,1] 
    # get numeric coding and varNames
    coding<-gsub("^([0-9][0-9]*).*|^[^:]*: ([0-9][0-9]*).*","\\1\\2",fullNames)
    coding[grep("^[0-9]",coding,invert=TRUE)]<-""
    varNames<-gsub("^[0-9][^A-z]*([A-z])|^[^:]*: [0-9][^A-z]*([A-z])","\\1\\2",fullNames)
    
    codes<-varNames[coding!=""]
    coding<-coding[coding!=""]
    
    colname<-m[1,]
    
    if((sum(is.element(c(1,2,3),gsub("^0","",coding)))==3|sum(is.element(c(2,3,4),gsub("^0","",coding)))==3) &
       sum(nchar(coding))!=0){
      for(i in 1:length(coding)){
        colname<-gsub(paste0("^",coding[i],"[^0-9]*$"),codes[i],colname)
      }
      m[1,]<-colname
      m[,1]<-varNames  
    }
  } 

# if first row still contains numbers and nrow=ncol
  if(sum(is.element(c(1,2,3),gsub("^0","",m[1,])))==3){
    if(nrow(m)==ncol(m))
      m[1,]<-m[,1]
  }
  
  
      
return(m)
}# end function coding2variable

## Function to handle correlation tables
extractCorrelations<-function(x,
                              legendCodes=NULL,
                              remove=FALSE,
                              standardPcoding=TRUE){
  # prepare legend codings
  parentheses<-NULL;brackets<-NULL;psign<-NULL;pval<-NULL;italic<-NULL;bold<-NULL;N<-NULL
  # get legend codings
  if(!is.list(legendCodes)) legendCodes<-legendCodings(legendCodes)
  if(is.list(legendCodes)){
    psign<-legendCodes$psign
    pval<-legendCodes$pval
    italic<-legendCodes$italic
    bold<-legendCodes$bold
    N<-gsub("[Nn]=","",legendCodes$N)
  }
  
  # take a copy of unified matrix
  x<-unifyMatrixContent(x)
  m<-x
  # unify letters
  m<-letter.convert(m,greek2text=TRUE)
  if(ncol(m)<=2|nrow(m)<=2) return(m)
  # remove grouping text 
  preText<-rep("",length(m[,1]))
  i<-grep("(.*: )",m[,1])
  preText[i]<-gsub("(.*: ).*","\\1",m[i,1])
  m[,1]<-gsub(".*: ","",m[,1])
  
  # move number in bracket at end in first row and col to front
  m[1,]<-gsub("^([A-z][-A-z _\\^\\.\\*]*) (\\([1-9][0-9]*\\))$","\\2 \\1",m[1,])
  m[,1]<-gsub("^([A-z][-A-z _\\^\\.\\*]*) (\\([1-9][0-9]*\\))$","\\2 \\1",m[,1])
  m[1,]<-gsub(": ([A-z][-A-z _\\^\\.\\*]*) (\\([1-9][0-9]*\\))$",": \\2 \\1",m[1,])
  m[,1]<-gsub(": ([A-z][-A-z _\\^\\.\\*]*) (\\([1-9][0-9]*\\))$",": \\2 \\1",m[,1])
  m[1,]<-gsub("^([A-z][-A-z _\\^\\.\\*]*) (\\([1-9][0-9]*\\)): ","\\2 \\1: ",m[1,])
  m[,1]<-gsub("^([A-z][-A-z _\\^\\.\\*]*) (\\([1-9][0-9]*\\)): ","\\2 \\1: ",m[,1])
  
  # remove brackets around numbers from first row and col
  m[1,]<-gsub("^ *\\(([1-9][0-9]*\\.*)\\)","\\1",m[1,])
  m[,1]<-gsub("^ *\\(([1-9][0-9]*\\.*)\\)","\\1",m[,1])
  
  # paste first and second column, if first column has enumeration only
  col1<-m[,1]
  if(length(col1)>1&sum(gsub("[0-9\\[:punct:]]","",col1[-1])=="",na.rm=TRUE)==(length(col1[-1]))){
    col2<-m[,2]
    m[,1]<-gsub("^ | $","",paste(col1,col2))
    m<-m[,-2]
  }
  
  if(ncol(m)<2|nrow(m)<2) return(m)
  
  
  nCol<-ncol(m)     
  # convert signs to p-values
  m[-1,-1]<-sign2p(m[-1,-1],sign=psign,val=pval)
  # unify numbering at beginning of first column/row to "number."
  m[,1]<-gsub("^\\(*0*([1-9][0-9]*)[\\),;:] ","\\1. ",m[,1])
  m[1,]<-gsub("^\\(*0*([1-9][0-9]*)[\\),;:] ","\\1. ",m[1,])
  
  cors<-suppressWarnings(as.numeric(gsub("[^0-9\\.-]|;;.*|\\^.*","",m)))
  # prepare removal matrix of correlations
  rem<-cors<=1&cors>=-1
  
  # select correlations except -< and 1
  cors<-cors<=1&cors>=-1
  cors<-matrix(cors,ncol=nCol)
  
  rem<-matrix(rem,ncol=nCol)
  

  # if first row has increasing numbers but first column has not add detected numbers to text in first column
  if(sum(is.element(c("1","2","3"),gsub("[\\.\\(\\) ]*","",m[1,-1])))==3 & 
     sum(is.element(c("1","2","3"),gsub("^([1-9]).*","\\1",m[-1,1])))==0){
    i<-grep("^[0-9][0-9]*$",gsub("[\\.\\(\\) ]*","",m[1,-1]))
    if(length(i)==max(i)&max(i)<=(nrow(m)-1)){
      m[1,i+1]<-m[2:(max(i)+1),1]
    }
  }
  
  # if matrix has enumeration in rows and cols, 
  if((length(grep("^[0-9][0-9\\.]*$",m[1,-1]))>2&length(grep("^[0-9].*[A-z]",m[-1,1]))>2)|
      (length(grep("^[0-9].*[A-z]",m[1,-1]))>2&length(grep("^[0-9][0-9\\.]*$",m[-1,1]))>2)){
    # deselect lines for extraction 
  cols<-grep("^[1-9]| [1-9]*[0-9]$",m[1,],invert=TRUE)
  rows<-grep("^[1-9]| [1-9]*[0-9]$",m[,1],invert=TRUE)
  cors[,cols]<-FALSE
  cors[rows,]<-FALSE
  rem[rows,]<-FALSE
  rem[,cols]<-FALSE
  }
  
  # set rows/cols with mean/sd/etc to FALSE
  cors[grep("Cronbach|^[Mm]ean$|^M$|[Ss]tandard [Dd]eviation|^SDS*$|^alpha$| alpha$|^AVE$|^MSV$|^ASV$|Skewnes|[KC]urtosis|[Rr]eliabilit|^[Vv]ariance| [Vv]ariance|[sS]quared",m[,1]),-1]<-FALSE
  cors[-1,grep("Cronbach|^[Mm]ean$|^M$|[Ss]tandard [Dd]eviation|^SDS*$|^alpha$| alpha$|^AVE$|^MSV|^ASV$$|Skewnes|[KC]urtosis|[Rr]eliabilit|^[Vv]ariance| [Vv]ariance|[sS]quared",m[1,])]<-FALSE
  
  #i<-which(rowSums(cors[-1,-1]==FALSE,na.rm=T)==0)+1
  #j<-which(colSums(cors[-1,-1]==FALSE,na.rm=T)==0)+1
  
  # convert numbered variable name to full name label 
  m<-coding2variable(m)  
  
  # add pretext again
  m[,1]<-paste0(preText,m[,1])
  
  # select correlation matrix and replicate var names
  corTab<-m
  corTab[-1,-1][-which(cors[-1,-1])]<-""
  # set first row and column to FALSE
  cors[1,]<-FALSE
  cors[,1]<-FALSE
  # add r= to cells with correlations
  corTab[which(cors)]<-gsub("^([-\\.]*[\\.0-9][\\.0-9[:punct:]]*)","r=\\1",corTab[which(cors)])
#  corTab[which(cors)]<-gsub("^([-\\.0-9][\\.0-9][\\.0-9[:punct:]]*)","r=\\1",corTab[which(cors)])

  # stars2p if still has star and standardPcoding==TRUE
  if(standardPcoding==TRUE){
  if(length(grep("\\*$",corTab))>0){
    #corTab[which(cors)]<-gsub("([0-9])$","\\1;; p>.05",corTab[which(cors)])
    corTab[which(cors)]<-gsub("\\^*\\*\\*\\*$",";; p<.001",corTab[which(cors)])
    corTab[which(cors)]<-gsub("\\^*\\*\\*$",";; p<.01",corTab[which(cors)])
    corTab[which(cors)]<-gsub("\\^*\\*$",";; p<.05",corTab[which(cors)])
  }}
  
  corTab<-gsub("r=([<>])","r\\1",corTab)

  # add "p > max(p<x)" if has added p-value or coding in legend
  if(length(grep(";; p<",corTab))>0|length(grep("p<",pval))>0){
  if(length(grep("p<",pval))>0) 
    Pmax<-suppressWarnings(max(as.numeric(
              gsub("p<=*","",grep("p<=*",gsub("p=0*\\.0([51])","p<.0\\1",grep("p<",pval,value=TRUE)),value=T)))))
  if(length(grep("p<",pval))==0) 
    Pmax<-suppressWarnings(max(as.numeric(
              gsub("^(0*.*[0-9][0-9]*).*","\\1",gsub(".*;; p<=*","",grep(";; p<",corTab,value=TRUE))))))

  corTab<-gsub("^(r=-*0*\\.[0-9][0-9]*)$",paste0("\\1;; p>",Pmax),corTab)
  }
  
  # add "p < min(p>x)" if has added p-value or coding in legend
    if(length(grep(";; p>",corTab))>0|length(grep("p>",pval))>0){
    if(length(grep("p>",pval))>0) 
      Pmin<-suppressWarnings(min(as.numeric(
        gsub("p>=*","",grep("p<|p>",gsub("p=0*\\.0([51])","p>.0\\1",grep("p>",pval,value=TRUE)),value=T)))))
    if(length(grep("p>",pval))==0) 
      Pmin<-suppressWarnings(min(as.numeric(
        gsub("^(0*.*[0-9][0-9]*).*","\\1",gsub(".*;; p>=*","",grep(";; p>",corTab,value=TRUE))))))
    
  corTab<-gsub("^(r=-*0*\\.[0-9][0-9]*)$",paste0("\\1;; p<",Pmin),corTab)
  }

  # extract correlations as vector
  correlations<-NULL
  for(i in 2:nrow(corTab))
    for(j in 2:ncol(corTab))
      if(corTab[i,j]!="")
        correlations<-c(correlations,paste0(corTab[i,1]," <<~>> ",corTab[1,j],": ",corTab[i,j]))
  
  # set extracted correlations to "" in matrix
  m[-1,-1][corTab[-1,-1]!=""]<-""
  #m<-gsub("^-$","",m)
  
  ### remove empty lines/columns
  nonempty<-m!=""
  # lines
  m<-m[c(1,which(rowSums(matrix(nonempty[-1,-1],ncol=nCol-1))!=0)+1),]
  if(is.vector(m)) m<-matrix(m,ncol=1)
  # columns
  m<-m[,c(1,which(colSums(matrix(nonempty[-1,-1],ncol=nCol-1))!=0)+1)]
  if(is.vector(m)) m<-matrix(m,ncol=length(m))

  # empty matrix if only one row or column is left
  if(nrow(m)==1|ncol(m)==1) m<-NULL
  
  # add (Nmax-2) to r=
  if(length(N)>0){
    Nmax<-suppressWarnings(max(as.numeric(N),na.rm=T))
    if(!is.na(Nmax)) correlations<-gsub(" r=",paste0(" r(",Nmax-2,")="),correlations)
  }
  # output
  if(remove==FALSE) return(correlations)
  if(remove==TRUE){
    return(m)
  }
} # end extractCorrelations


extractMatrixWise<-function(x,remove=FALSE){
  if(!is.matrix(x)&remove==FALSE) return(NULL)
  if(!is.matrix(x)&remove==TRUE) return(x)
  # extend numeric variable codings
  x<-coding2variable(x)  
  # find variables that exist in 1st row and col
  
  row<-x[1,]
  col<-x[,1]
  i_row<-which(is.element(row,col))
  i_col<-which(is.element(col,row))
  
  #####....
  

  if(remove==FALSE) return(x)
  if(remove==TRUE) return(x)
}

 
# convert signs to p-values
sign2p<-function(x,sign,val,sep=";;"){
  #sign<-leg$psign
  #val<-leg$pval
  if(length(x)==0) return(x)
  if(length(sign)==0) return(x)
  # reorder by nChar of sign
  i<-order(nchar(sign),decreasing=TRUE)
  sign<-sign[i]
  val<-val[i]
  # special sign handling (replaces * to \\*)
  sign<-gsub("\\*","\\\\*",sign)
  sign<-gsub("\\^","\\\\^",sign)
  sign<-gsub("\\+","\\\\+",sign)
  sign<-gsub("\\$","\\\\$",sign)
  sign<-gsub("\\.","\\\\.",sign)
  
  for(i in 1:length(sign)){
    #convert sign to pval if at end 
    x<-gsub(paste0(sign[i],"$"),
             # p value with seperator     
             paste(sep,val[i]),gsub("^n\\.*s\\.*$|( )n\\.*s\\.*$","\\1ns",x))
    # convert sign to pval if NOT at end 
    incl<-grep(paste0("[A-z] *",sign[i]," *[A-z]"),x,invert=TRUE)
    x[incl]<-gsub(paste0(sign[i],"[,; ]*"),
            # p value with seperator     
            paste0(sep," ",val[i],", "),x[incl])
    
    # clean up if cell has ^;; p-val
    x<-gsub(paste0("\\^(",sep," )"),"\\1",x)
    # clean up if cell starts with seperator
    x<-gsub(paste0("^",sep," "),"",x)
    # clean up before bracket
    x<-gsub(", \\)",")",x)
    x<-gsub(", \\]",")",x)
  }
 return(x)
}

## convert no p sign to p>max(p)
noSign2p<-function(x,pval){
  if(!is.matrix(x)) return(x)
  # remove pvalues with ">" in coding
  i<-grep(">=*|[^<]=",pval,invert=TRUE)
  pval<-pval[i]
  # escape
  if(length(pval)==0) return(x)
  nCol<-ncol(x)
  # find cloumns with already inserted p-values
  cols<-which(colSums(matrix(grepl(";; p<",x),ncol=nCol))>0)
  # escape
  if(length(cols)==0) return(x)
  # get max of coded p
  if(length(pval)>0) Pmax<-max(as.numeric(gsub("p<=*","",pval)))
  if(length(pval)==0) Pmax<-max(as.numeric(gsub("^(0*.*[0-9][0-9]*).*","\\1",gsub(".*;; p<=*","",grep(";; p<",x[,cols],value=TRUE)))))
  # go through columns and add p>Pmax
  for(j in cols){
    cells<-grep(";; p<",x[,j],invert=TRUE)
    # remove cell=1 if exists
    cells<-cells[cells!=1]
    x[cells,j]<-gsub("([0-9])$",paste0("\\1;; p>",Pmax),x[cells,j])
    }
  return(x)
}

# convert brackets to values
bracket2value<-function(x,value,type=c("parentheses","brackets")[1],sep=";"){
  if(length(x)==0) return(x)
  if(length(value)==0) return(x)
#  if(ncol(x)<2|nrow(x)<2) return(x)
  # is confidence/highest density interval
  i<-grep("CI|HDI",value)
  # for CI's or HDI's keep bracket/parentheses
  if(length(i)>0){
    if(type=="parentheses") 
      x<-gsub("(\\(-*[\\.0-9][\\.0-9]*[^\\.0-9][^\\]]*-*[0-9\\.][\\.0-9]*\\))",paste0(paste(sep,value),"=","\\1"),x)    
    if(type=="brackets") 
      x<-gsub("(\\[-*[\\.0-9][\\.0-9]*[^\\.0-9][^\\]]*-*[0-9\\.][\\.0-9]*\\])",paste0(paste(sep,value),"=","\\1"),x)    
  }
  # for non CI's or HDI's
  if(length(i)==0){
  if(type=="parentheses") 
    x<-gsub("\\(([^\\)]*[0-9][^\\)]*)\\)",paste0(paste(sep,value),"=","\\1"),x)    
  if(type=="bracket") 
    x<-gsub("\\[([^\\]*[0-9][^\\]*)\\]",paste0(paste(sep,value),"=","\\1"),x)    
  }
  
  
  # cleanup
  x<-gsub(paste0(" (",sep," )"),"\\1",x)
  x<-gsub(paste0("^",sep," "),"",x)
  return(x)
}

abb2text<-function(x,abbr,label){
  if(length(abbr)==0) return(x)
  # reorder by length
  i<-order(abbr,decreasing=TRUE)
  abbr<-abbr[i]
  label<-label[i]
  abbr<-specialChars(abbr)
  
  # expand each abbreviation
  for(i in 1:length(abbr))  
    x<-gsub(
           paste0("([^A-z]|[[:punct:]])",abbr[i],"([^A-z]|[[:punct:]])|","^",abbr[i],"([^A-z]|[[:punct:]])|",abbr[i],"$"),
           paste0("\\1",label[i],"\\2\\3"),x)
  return(x)
}

sup2text<-function(x,sup=NULL,sup_label=NULL){
  if(length(sup)==0) return(x)
  i<-order(sup,decreasing=TRUE)
  sup<-sup[i]
  sup_label<-sup_label[i]
  for(i in 1:length(sup))
    x<- gsub(specialChars(sup[i]),paste0(" (",sup_label[i],")"),
             gsub("([^\\^])\\*","\\1^*",x))
  return(x)
}

percentHandler<-function(x){
      # escapes
      if(length(x)==0) return(x)
      if(!is.matrix(x)) return(x)
      if(nrow(x)<1) return(x)
      # extract and insert (pattern) as column
      
      # columns with " (%)" in header
      ind<-grepl(" *\\(\\%\\)| *\\[\\%\\]", x[1,])
      # remove brackets if only numbers are below (%)
      if(sum(ind)>0) 
        for(j in which(ind)){
         if(length(grep("^[-0-9\\.][0-9\\.]*$",x[-1,j]))==length(x[-1,j]))
           x[1,j]<-gsub("\\(%\\)","%",x[1,j])
         }
      
      # columns with " (%)" in header
      ind<-grepl(" *\\(\\%\\)| *\\[\\%\\]", x[1,])
      # colums with only numbers below the header 
      #      ind2<-colSums(matrix(grepl("^[-0-9\\.][0-9\\.]*$",as.vector(x[-1,])),ncol=ncol(x)))>1
      #ind<-ind#&ind2
      
      # remove (%) from 1st line and paste to cells with only one number
      for(j in which(ind)){
        x[1,j]<-gsub(" *\\(\\%\\)| *\\[\\%\\]","",x[1,j])
        # add % if only one number in bracket and remove bracket
        x[-1,j]<-gsub("^\\((-*[0-9\\.][0-9\\.]*)\\)$","\\1%",x[-1,j])
        # add % to number in brackets
        #x[-1,j]<-gsub("^([0-9\\.][0-9\\.]* *\\([0-9\\.][0-9\\.]*)\\)$","\\1%)",x[-1,j])
        # convert to "coma value%"
        x[-1,j]<-gsub("^(-*[0-9\\.][0-9\\.]*) *\\(([0-9\\.][0-9\\.]*)%*\\)$","\\1, \\2%",x[-1,j])
        
        }
      x<-unname(x)
      return(x)
    }

## split lines with multiple results 
# 2 CIs
splitCIs<-function(x){
      x<-as.list(x)
      i<-nchar(gsub("\\[[-\\.0-9]","",x))<(nchar(x)-2)
      x[i]<-lapply(x[i],function(y) strsplit2(y,"[0-9]\\][,;] ","after"))
      x<-unlist(x)
      x<-gsub("[,;] *$","",x)
      return(x)
    }
    
splitLastStat<-function(x){
      # define functions
      # split at last detected stat
      fun1<-function(x){
      lastStat<-gsub(".*,[^<=>]* ([-A-z0-9\\^_][-A-z0-9\\^_]*)[<=>][<=>]*-*[0-9\\.][-0-9\\.\\^]*$","\\1",x)
      
      # remove till standard stat
      lastStat<-gsub(".* ([StTzZpPQIHdbBF][DFEdfe]*)$","\\1",lastStat)
      lastStat<-gsub(".* ([Cce][ht][ai]\\^*2*)$","\\1",lastStat)
      lastStat<-gsub(".* (omega\\^*2*)$","\\1",lastStat)
      lastStat<-gsub(".* (R\\^*2*)$","\\1",lastStat)
  #    lastStat<-gsub("\\^","\\\\^",lastStat)
  #    lastStat<-gsub(".*([^A-z])\\\\\\^","\\\\\\1\\\\^",lastStat)
      
      if(lastStat!=x){
      x<-gsub(paste0("(",specialChars(lastStat),"[<=>][<=>]*-*[0-9\\.]*)[,;]* "),"\\1SPLITHERE",x)
      x<-unlist(strsplit(x,"SPLITHERE"))
      if(length(x)>1)
        # add first cell content to front of new lines
        #  x[-1]<-paste0(gsub("^([^,]*), .*","\\1, ",x[1]),x[-1])
        # add stats in table num:
        x[-1]<-paste0(gsub("^([^:]*): .*","\\1: ",x[1]),x[-1])
      
      }
      return(x)
      }
      
      # split at last detected imputed p-value
      fun2<-function(x){
        lastPcode<-gsub(".*(;; p)[<=>][<=>]*[0-9\\.]*$","\\1",x)
      if(sum(lastPcode!=x)>0){
        x<-gsub(paste0("(;; p[<=>][<=>]*[0-9\\.]*)[,;] "),"\\1SPLITHERE",x)
        x<-unlist(strsplit(x,"SPLITHERE"))
        # add first cell content to front of new lines
#        if(length(x)>1)
          #  x[-1]<-paste0(gsub("^([^,]*), .*","\\1, ",x[1]),x[-1])
          # add stats in table num:
#          x[-1]<-paste0(gsub("^([^,:]*)[:,] .*","\\1: ",x[1]),x[-1])
      }
    return(x)
      }
      
      # apply function cell wise
      x<-unlist(lapply(x,fun1))
      x<-unlist(lapply(x,fun2))
      return(x)
    }

# 2 t/F/Z-values
splitTFZB<-function(x){
      for(j in c(" t[<=>][-\\.0-9]| t\\([1-9]|^t[<=>][-\\.0-9]|^t\\([1-9]"," T[<=>][-\\.0-9]| T\\([1-9]|^T[<=>][-\\.0-9]|^T\\([1-9]",
                 " f[<=>][-\\.0-9]| f\\([1-9]|^f[<=>][-\\.0-9]|^f\\([1-9]"," f[<=>][-\\.0-9]| F\\([1-9]|^F[<=>][-\\.0-9]|^F\\([1-9]",
                 " z[<=>][-\\.0-9]| z\\([1-9]|^z[<=>][-\\.0-9]|^z\\([1-9]"," z[<=>][-\\.0-9]| Z\\([1-9]|^Z[<=>][-\\.0-9]|^Z\\([1-9]",
                 " b[<=>][-\\.0-9]| b\\([1-9]|^b[<=>][-\\.0-9]|^b\\([1-9]"," B[<=>][-\\.0-9]| B\\([1-9]|^b[<=>][-\\.0-9]|^B\\([1-9]",
                 " beta[<=>][-\\.0-9]| beta\\([1-9]|^beta[<=>][-\\.0-9]|^beta\\([1-9]")){
        i<-nchar(gsub(j,"",x))<(nchar(x)-4)
        x<-as.list(x)
        x[i]<-lapply(x[i],function(y) strsplit2(y,substr(j,1,25),"before"))
        x<-unlist(x)
        x<-gsub("^ |[,;]$","",x)
      }
      return(x)
    }
    
# 2 p-values??? (to do!!)
splitP<-function(x){
      return(x)
    }
    

# split at duplicatated header cells
splitter<-function(x){
  # collapse header rows
  x<-headerHandling(x)
  
  # try split by first line
  # x<-duplicatedColSplit(x,1)
  # if not splitted try split by second line
  #if(!is.list(x)) x<-duplicatedColSplit(x,2)
  # if not splitted try split by third line
  #if(!is.list(x)) x<-duplicatedColSplit(x,3)
  
  # split by model num
  if(length(grep("Mode*l* *[1-9]|^M [1-9]|[Ee]quation *[1-9]",x[1,-1]))>0){
    mods<-gsub(".*Mode*l* *([1-9][0-9]*).*|^M ([1-9][0-9]*).*|.*[Ee]quation *([1-9][0-9]*)","\\1\\2\\3",x[1,])
    mods<-unique(suppressWarnings(as.numeric(mods)))
    mods<-mods[!is.na(mods)]
    mods<-mods[mods!=1]
    if(length(mods)>0){
      at<-NULL
      for(i in 1:(length(mods)))
        at[i]<-min(grep(paste0(c("Mode*l* *","^M ","Equation *"),mods[i],collapse="|"),x[1,]))
      at <- at[at>2]
      if(length(at)>0) x<-colSplit(x,at-1)
    }
  }
  return(x)
}


# Function to parse content from brackets to a new column
newColumnBracket<-function(x){
  # escapes
  if(length(x)==0) return(x)
  if(!is.matrix(x)) return(x)
  if(nrow(x)<1) return(x)
  if(ncol(x)<2) return(x)
  
  # extract and insert number in round brackets as column
  ind<-which(grepl("\\([-0-9\\.][-,;0-9\\. ]*\\)", as.vector(x[-1,]))&grepl("\\([-0-9\\.A-z][-,;0-9\\. A-z]*\\)", as.vector(x[1,])))
  if(length(ind)>0){
    nCols<-ncol(x)
    ind<-which((colSums(matrix(unlist(
      grepl("\\([-0-9\\.][-,;0-9\\. ]*\\)",as.vector(x[-1,]))),ncol=nCols,byrow=FALSE))>0) 
      &
      grepl("\\([A-z].*\\)|\\(.*[A-z]\\)|\\(.*%\\)",x[1,])  
        )
    Nadded<-0
    
    if(length(ind)>0) for(j in ind){
      noBracket<-gsub("^ | $","",gsub(" \\([-0-9\\.A][-,;0-9\\. A-z]*\\)","",x[,j+Nadded]))
      noBracket[1]<-gsub("  *"," ",gsub(" *\\(([^\\)]*)\\)","",noBracket[1]))
      Bracket<-gsub("  *"," ",gsub(".*\\(([-0-9\\.][-,;0-9\\. A-z]*)\\).*","\\1",x[,j+Nadded]))
      # set cells that don't have bracket to ""
      k<-grep("\\(([-0-9\\.][-,;0-9\\. A-z]*)\\)",x[,j+Nadded],invert=TRUE)
      Bracket[k]<-""
      # add code in brackets to first cell
      
      Bracket[1]<-gsub("  *"," ",gsub(".*\\(([^\\)]*)\\).*","\\1",x[1,j+Nadded]))
      
      Bracket[1]<-gsub("\\+","\\\\+",Bracket[1])
      Bracket[1]<-gsub("\\^","\\\\^",Bracket[1])
      Bracket[1]<-gsub("\\$","\\\\$",Bracket[1])
      Bracket[1]<-gsub("\\*","\\\\*",Bracket[1])
      
      # and remove from all other entries
      noBracket<-gsub(paste0(" *\\(",Bracket[1],"\\)"),"",noBracket)
      Bracket[1]<-gsub("\\\\","",Bracket[1])
      
      # add column
      if(j==1) x<-cbind(noBracket,Bracket,x[,(j+1):ncol(x)])
      if(j>1&(j+Nadded)<ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket,x[,(j+1+Nadded):ncol(x)])
      if(j>1&(j+Nadded)==ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket)
      Nadded<-Nadded+1
    }
  }
  
  # extract and insert number in squared brackets as column
  ind<-which(grepl("\\[[-0-9\\.][-,;0-9\\. ]*\\]", as.vector(x[-1,]))&grepl("\\[[-0-9\\.A-z][-,;0-9\\. A-z]*\\]", as.vector(x[1,])))
  if(length(ind)>0){
    nCols<-ncol(x)
    ind<-which(colSums(matrix(unlist(
      grepl("\\[[-0-9\\.][-,;0-9\\. ]*\\]",as.vector(x[-1,]))),ncol=nCols,byrow=FALSE))>0 
      &
        grepl("\\[[A-z].*\\]|\\[.*[A-z]\\]|\\[.*%\\]",x[1,])  
    )
    Nadded<-0
    
    if(length(ind)>0) for(j in ind){
      noBracket<-gsub("^ | $","",gsub(" \\[[-0-9\\.A-z][-,;0-9\\. A-z]*\\]","",x[,j+Nadded]))
      noBracket[1]<-gsub("  *"," ",gsub(" *\\[([^\\]]*)\\]","",noBracket[1]))
      Bracket<-gsub("  *"," ",gsub(".*\\[([-0-9\\.][-,;0-9\\. A-z]*)\\].*","\\1",x[,j+Nadded]))
      # set cells that don't have bracket to ""
      k<-grep("\\[([-0-9\\.][-,;0-9\\. A-z]*)\\]",x[,j+Nadded],invert=TRUE)
      Bracket[k]<-""
      # add code in brackets to first cell
      Bracket[1]<-gsub("  *"," ",gsub(".*\\[([^\\]]*)\\].*","\\1",x[1,j+Nadded]))
      # add empty column
      if(j==1) x<-cbind(noBracket,Bracket,x[,(j+1):ncol(x)])
      if(j>1&(j+Nadded)<ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket,x[,(j+1+Nadded):ncol(x)])
      if(j>1&(j+Nadded)==ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket)
      Nadded<-Nadded+1
    }
  }
  
  x<-unname(x)
  return(x)
}

# Function to split and create new columns with (95%CI)
newColumnCI<-function(x){
  # escapes
  if(length(x)==0) return(x)
  if(!is.matrix(x)) return(x)
  if(nrow(x)<1) return(x)
  if(ncol(x)<2) return(x)
  # extract and insert (pattern) as column
  x<-gsub("[Cc]onfidence[ -][Ii]nterval","CI",x)
  header<- as.vector(x[1,] )
  ind<-grep("\\([0-9][0-9] *%*[- ]*[CcK][Ii]\\)|\\[[0-9][0-9] *%*[- ]*[CKc][Ii]\\]", header)
  if(length(ind)>0){
    Nadded<-0
    for(j in ind){
      noCI<-gsub("^ | $","",gsub("  *"," ",
                                 gsub("\\([0-9][0-9] *%*[- ]*[CKc][Ii]\\)|\\([-\\.0-9][\\.0-9 ]*[^0-9]*[-\\.0-9]*\\)|\\[[0-9][0-9] *%*[- ]*[CcK][Ii]\\]|\\[[-\\.0-9][\\.0-9 ]*[^0-9]*[-\\.0-9]*\\]","",x[,j+Nadded])))
      CI<-gsub(".*\\(([0-9][0-9] *%*[- ]*[CcK][Ii])\\).*|.*(\\([-\\.0-9][\\.0-9 ]*[^0-9]*[-\\.0-9]*\\)).*|.*\\[([0-9][0-9] *%*[- ]*[CcK][Ii])\\].*|.*(\\[[-\\.0-9][\\.0-9 ]*[^0-9]*[-\\.0-9]*\\]).*","\\1\\2\\3\\4",x[,j+Nadded])
      # add empty column
      if(j==1) x<-cbind(noCI,CI,x[,(j+1+Nadded):ncol(x)])
      if(j>1&(j+Nadded)<ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noCI,CI,x[,(j+Nadded+1):ncol(x)])
      if(j>1&(j+Nadded)==ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noCI,CI)
      Nadded<-Nadded+1
    }
  }
  x<-unname(x)
  return(x)
}


# flatten list of lists to simple list
flatten<-function(x){
  j<-unlist(lapply(x,is.list))
  j
  out<-list()
  n<-names(x)
  names<-NULL
  
  for(i in 1:length(x)){
   if(j[i]){
     for(k in 1:length(x[[i]])) out<-c(out,x[[i]][k])  
     names<-c(names,rep(n[i],length(x[[i]])))
   }
   if(!j[i]){
     out[[length(out)+1]]<-x[[i]]  
     names<-c(names,n[i])
   }
   }
  out
  names(out)<-names
  return(out)
}



R2handler<-function(x){
  i<-grep("R\\^2",x[,1])
  # if is in lower half
  if(length(i)>0) if(i[1]>nrow(x)/2){
    # paste content to numeric cells by row
    x1<-x[1:(i[1]-1),]
    x2<-cbind(x[1,],t(x[i[1]:nrow(x),]))
    x2[1,1]<-""
    return(list(x1,x2))
  }       
  return(x)   
}

##################################################################
anovaHandler<-function(x){
  if(length(x)==0) return(x)
  # take a copy
  y<-x
  # add df2
  if(sum(grepl(" F=",x)&grepl(" df=",x))>0 & 
     sum(!grepl(" F=",x)&grepl(" df=",x))>0){
    i<-grep(" F=",x)
    j<-grep(" F=|[Tt]otal",x,invert=TRUE)
    if(length(j)>0&length(grep("df=",x[j]))>0){
      x[i]<-gsub("df=","df1=",x[i])
      x[j]<-gsub("df=","df2=",x[j])
      # extract sum of df2
      df2<-sum(as.numeric(unique(gsub(".*df2=([0-9][0-9\\.]*).*","\\1",x[j]))),na.rm=T)
      # add df2 to df 1
      x[i]<-gsub("(df1=[0-9][0-9\\.]*)",paste0("\\1",", df2=", df2),x[i])
      # remove df2 from j
#      x[j]<-gsub(", df2=[0-9][0-9\\.]","",x[j])  
    
      }
    }
  #
  # warning
  if(sum(x!=y)>1) warning("Special handling for degrees of freedom in ANOVA tables was performed. This may result in erroneous df2-values, which where imputet in order to check the reported p-values.")
  
  # in Lines with F-values
  i<-grep(" F=",x)
  if(length(i)>0){
   # convert df1 and df2 in brackets to text
   x[i]<-gsub(" [Dd][Ff]: \\(([0-9][0-9\\.]*)[,;] ([0-9][0-9\\.]*)\\)"," df1=\\1, df2=\\2",x[i])
   # unify df1 and df2 in text
#   x[i]<-gsub(",[^,]*[Dd][Ff][=: ]*([0-9][0-9\\.]*),[^,]*[Dd][Ff]=([0-9][0-9\\.]*)",", df1=\\1, df2=\\2",x[i])
  }

  # unify df1 and df2 in text
  x<-gsub(",[^,]*[Dd][Ff][=: ]*([0-9][0-9\\.]*)[,;] *([0-9][0-9\\.]*)",", df1=\\1, df2=\\2",x)
  
  return(x)
}



#####################################

# split stats at first duplicated row name
dupSplit<-function(x){
 
  out<-list()
  temp<-x
  where<-which(duplicated(gsub("^[^:]*:* *([^,]*), .*","\\1",temp)))[1]
  if(length(where)==0|is.na(where)){
    out[[1]]<-x
    return(out)}
  while(length(where)>0&!is.na(where)){
    out[[length(out)+1]]<-temp[1:(where-1)]
    temp<-temp[-1:-(where-1)]
    where<-which(duplicated(gsub("^[^:]*:* *([^,]*), .*","\\1",temp)))[1]
  }
  # add remaining lines
  out[[length(out)+1]]<-temp
  return(out)
  }
  


## paste model and standard statistics in first col to numeric field behind
modelStatsHandler<-function(x){
  if(!is.matrix(x)) return(x)
  if(nrow(x)<3) return(x)
  if(ncol(x)<2) return(x)
  
  ## paste first cell to all cells with numbers in rows starting from search term 
  
  ## for standard results
  ind1<-grep("^[A-z]$|^[RO]R$|^[Ss][Ee]$",gsub("[- ]val*u*e*s*","",x[,1]))
  ## for model statistics
  ind2<-grep("^R2$|[- ]R2|R\\^2|R[- ][Ss]q|[Rr]esidual|AIC|BIC|[Ii]nformation [Cr]iter|chi\\^2|degrees* of freedom|^df$|^DF$|[Ll]ikelihood",x[,1])
  # model index if second cell (constant/first variable) only exists once
  if(length(ind2)>0&!is.element(x[2,1],x[c(-1,-2),1])) ind2<-ind2[1]:nrow(x)
  # combined index
  ind<-sort(unique(c(ind1,ind2)))
  ind
  # paste first cell to numeric cells in same row
  if(length(ind)>0){
    for(h in ind){
      x[h,grep("^-*[\\.0-9]",x[h,])]<-paste0(x[h,1],"=",x[h,grep("^-*[\\.0-9]",x[h,])])
      x[h,grep("^[<=>][<=>]*-*[\\.0-9]",x[h,])]<-paste0(x[h,1],"",x[h,grep("^[<=>][<=>]*-*[\\.0-9]",x[h,])])
    }
    # remove first cell content
    if(length(ind>0)) x[ind,1]<-"TEMP_TEXT"
  }
  
  ## collapse all cells with pasted statistics per col 
  hasSequence<-function(x){
    if(length(x)<2) return(FALSE)
    temp<-NULL
    listSeq<-list()
    listInd<-1
    for(i in 1:(length(x)-1)){
      temp<-c(temp,(x[i]+1)==x[i+1])
      if((x[i]+1)==x[i+1]){
        listSeq
      } 
    }
    out<-sum(temp,na.rm=TRUE)>0
    return(out)
  }

  firstSeq<-function(x){
    seq<-NULL
  for(i in 1:(length(x)-1)){
    if((x[i]+1)==x[1+i]) seq<-unique(c(seq,x[i],x[i+1]))
    if((x[i]+1)!=x[1+i]&length(seq)>1) break()
  }
  return(seq)
  }
  
  # apply function to parse cells
  while(hasSequence(ind)){
    i<-firstSeq(ind)
       for(j in 2:ncol(x)) x[i[1],j]<-paste0(x[i,j],collapse=", ")
       x[i[-1],]<-"removeLine"
     ind<-ind[!is.element(ind,i)]
   }
   x<-x[grep("removeLine",x[,1],invert=TRUE),]
   x<-gsub("(, )(, )*|^, |, $","\\1",x)
   return(x)
}



# handling of model statistics. Use with: unlist(lapply(dupSplit(stats),modelhandler))
modelHandler<-function(x){
  fun<-function(x){
    i<-grep("R\\^2|R[- ][Ss]q|[Rr]esidual|AIC|BIC|[Ii]nformation [Cr]iter|chi\\^2|degrees* of freedom|^df$|^DF$|^F$",x)
  stats<-x
  if(length(i)>0){
    i<-min(i):length(stats)
    # switch position
    preValue<-gsub("^[^:]*:* *([^,][^,]*), ([^=]*)=.*","\\1",stats[i])
    postValue<-gsub("(.*): .*","\\1",gsub("^[^:]*:* *([^,][^,]*), ([^=]*)=.*","\\2",stats[i]))
    
    preValue<-gsub("\\*","\\\\*",gsub("\\^","\\\\^",preValue))
    preValue<-gsub("\\(","\\\\(",gsub("\\)","\\\\)",preValue))
    preValue<-gsub("\\[","\\\\[",gsub("\\]","\\\\]",preValue))
    preValue<-gsub("\\.","\\\\.",preValue)
    
    postValue<-gsub("\\*","\\\\*",gsub("\\^","\\\\^",postValue))
    postValue<-gsub("\\(","\\\\(",gsub("\\)","\\\\)",postValue))
    postValue<-gsub("\\[","\\\\[",gsub("\\]","\\\\]",postValue))
    postValue<-gsub("\\.","\\\\.",postValue)
    
    for(j in 1:length(postValue)){
      stats[i][j]<-gsub(postValue[j],"TEMPTEXT",stats[i][j])
      stats[i][j]<-gsub(preValue[j],postValue[j],stats[i][j])
      stats[i][j]<-gsub("TEMPTEXT",preValue[j],stats[i][j])
    }
    
    # remove standard model stats
    stats[i]<-gsub(" *standardized beta[,:] | *beta[,:] | *SE[,:] | [tTzZFpPORb][R]*[,:] |^[tTzZFpPORb][R]*[,:] | *[8-9][0-9]\\%[ -]CI[,:]* *"," ",stats[i])
    stats[i]<-gsub("^  *","",gsub("  "," ",stats[i]))
    # add model stats:
    out<-c("- Extracted model table:", stats[1:(i[1]-1)],"- Extracted and processed model statistics:",
           stats[i])
    return(out)
  }else
    return(x)
}
return(unlist(lapply(dupSplit(x),fun)))
}

#################################################
# model stats handler within matrix
collapseModelMatrix<-function(m){
  # escapes
  if(!is.matrix(m)) return(m)
  i<-grep("R\\^2|R[- ][Ss]q|[Rr]esidual|AIC|BIC|[Ii]nformation [Cr]iter|[cC]hi\\^2|degrees* of freedom|^df$|^DF$|^F$",m[,1])
  if(length(i)==0) return(m)
  # remove empty rows
  if(ncol(m)>2) m<-m[!rowSums(m[,-1]=="")==ncol(m[,-1]),]
  if(!is.matrix(m)) return(m)
  i<-grep("R\\^2|R[- ][Ss]q|[Rr]esidual|AIC|BIC|[Ii]nformation [Cr]iter|[cC]hi\\^2|degrees* of freedom|^df$|^DF$|^F$",m[,1])
  if(length(i)==0) return(m)
  # index for model stats
  ModRow<-min(i):nrow(m)
  if(length(ModRow)>0)
    for(k in ModRow){
      # to numeric cells in non numeric rows
      w<-grep("^[^-0-9\\.].",m[k,-1])
      m[k,-1][w]<-
        paste0(m[k,1],": ",m[k,-1][w])
      # to numeric cells in row
      w<-grep("^-*[0-9\\.]",m[k,-1])
      m[k,-1][w]<-
        paste0(m[k,1],"=",m[k,-1][w])
      # remove pasted entries
      m[k,1]<-""
    }
  
  # collapse colum-wise
  if(length(ModRow)>1){
    for(k in 2:ncol(m))
      for(n in 1:(length(ModRow)-1))
        m[ModRow[1],k]<-paste0(m[ModRow[1],k],
                               ifelse(m[ModRow[n],k]=="","",", "),
                               m[ModRow[n+1],k]
        )
    # remove parsed lines
    m<-m[-ModRow[-1],]
    # rename first cell
    m[ModRow[1],1]<-"collapsed model statistics"
  }
  return(m)
}


## flatten two level list to one level list
flattenList<-function(x){
  if(length(x)<=0) return(x)
  if(!is.list(x)) return(x)
  j<-unlist(lapply(x,is.list))
  if(sum(j)>0){
    n<-names(x)
    n<-rep(n,times=ifelse(j==TRUE,lapply(x[j],length),1))
    for(i in 1:length(j)){
      if(i==1){ 
        if(j[i]==FALSE) y<-x[[i]]
        if(j[i]==TRUE) y<-unlist(x[[i]],recursive=FALSE)
      }
      if(i>1){ 
        if(j[i]==FALSE) y<-c(y,list(x[[i]]))
        if(j[i]==TRUE) y<-c(y,x[[i]])
      }
    }
    x<-y
    #    names(x)<-n
  }
  return(x)
}


