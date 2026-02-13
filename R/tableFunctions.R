##########################################
## some useful matrix processing functions
##########################################

# convert special characters for replacement
specialChars<-function(x){
  i<-gsub("\\)","\\\\)",gsub("\\(","\\\\(",x))
  i<-gsub("\\]","\\\\]",gsub("\\[","\\\\[",i))
  i<-gsub("\\}","\\\\}",gsub("\\{","\\\\{",i))
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
 if(ncol(x)==nrow(x) & class == "correlation") return(x)
 if(ncol(x)==(nrow(x)-1) & class == "correlation") return(x)
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
  if(isTRUE(split) & length(splits)>0) x<-rowSplit(x,  splits)
   }
 }
  return(x)
}

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
  # if has many characters, skip processing headers
  if(sum(unlist(lapply(m,nchar))>100)>0) return(m)
  
  # collapse first two lines if first two cells in first column are empty 
  # and second line has no numbers with digit
  loop<-TRUE
  while(isTRUE(loop) & m[1,1]==""&m[2,1]=="" & length(grep("\\.[0-9]",m[2,-1]))==0){
  if(nrow(m)>2) 
    if(m[1,1]==""&m[2,1]==""){
    m[1,m[1,]!=m[2,]]<-paste0(m[1,m[1,]!=m[2,]]," ",m[2,m[1,]!=m[2,]])
    # remove duplicated text in first row
    for(i in 1:length(m[1,])) m[1,i]<-gsub(paste0("(",specialChars(m[2,i]),") ",specialChars(m[2,i]),"$"),"",m[1,i])
    # remove second row
    m<-m[-2,]
  }
  if(nrow(m)==2) loop<-FALSE
  }
  
  #collapse first two lines if second cell in first column is empty and no digit number in second row
  loop<-TRUE
  while(isTRUE(loop) & m[1,1]!=""&m[2,1]==""& length(grep("\\.[0-9]|[Nn]\\.[Ss]\\.|^ns$",m[2,-1]))==0){
    if(nrow(m)>2) 
      if(m[1,1]!=""&m[2,1]==""){
      m[1,m[1,]!=m[2,]]<-paste0(m[1,m[1,]!=m[2,]]," ",m[2,m[1,]!=m[2,]])
      # remove duplicated text in first row
      for(i in 1:length(m[1,])) m[1,i]<-gsub(paste0("(",specialChars(m[2,i]),") ",specialChars(m[2,i]),"$"),"\\1",m[1,i])
      # remove second row
      m<-m[-2,]
    }
    if(nrow(m)==2) loop<-FALSE
  }
  
  # collapse first two lines if first two rows have character, 
  # and second row has no result and last row numeric and second row does not only contain the same value 
  while(nrow(m)>2 & ncol(m)>1 & 
        length(grep("[A-z]|^$",gsub("\\^[A-z]*|<su[pb].*","",m[1,])))==ncol(m) &
        length(grep("[A-z]|^$",gsub("\\^[A-z]*|<su[pb].*","",m[2,])))==ncol(m) &
        length(grep("[A-z]\\^*2* *[<=>][<=>]* *[-0-9\\.]",m[2,]))==0 &
        length(grep("[Nn]\\.[Ss]\\.|^n\\.*s\\.*$",m[2,]))==0 &
        sum(is.element( m[2,-1],m[2,1]))!=length(m[2,-1]) &
        sum(is.element( m[2,-1],""))!=length(m[2,-1]) &
        length(grep("[0-9]",m[nrow(m),-1])>0) ){
          m[1,m[1,]!=m[2,]]<-paste0(m[1,m[1,]!=m[2,]]," ",m[2,m[1,]!=m[2,]])
          # remove duplicated text in first row
          for(i in 1:length(m[1,])) m[1,i]<-gsub(paste0("(",specialChars(m[2,i]),") ",specialChars(m[2,i]),"$"),"\\1",m[1,i])
          # remove second row
          m<-m[-2,]
    }
  
  # if second row has enumeration only parse to first row
  if(nrow(m)>2 & 
        length(grep("[A-z]|^$",m[1,]))==ncol(m) &
        length(grep("^1\\.*$",m[2,]))==1 & length(grep("^2\\.*$",m[2,]))==1 &
        length(grep("[0-9]",m[nrow(m),-1])>0) ){
    if((grep("^1\\.*$",m[2,])[1]+1)==grep("^2\\.*$",m[2,])[1]){
      m[1,m[1,]!=m[2,]]<-paste0(m[1,m[1,]!=m[2,]]," ",m[2,m[1,]!=m[2,]])
      # remove duplicated text in first row
      for(i in 1:length(m[1,])) m[1,i]<-gsub(paste0("(",specialChars(m[2,i]),") ",specialChars(m[2,i]),"$"),"\\1",m[1,i])
      # remove second row
      m<-m[-2,]
    }
  }
  
  #collapse first two lines if second row has %-CI
  while(nrow(m)>2 & 
        length(grep("[A-z]|^$",m[1,]))==ncol(m) &
        length(grep("[89][059]\\.*9*%[- ]CI|[89][059]\\.*9*%[- ]confidence",m[2,]))>=1){
    m[1,m[1,]!=m[2,]]<-paste0(m[1,m[1,]!=m[2,]]," ",m[2,m[1,]!=m[2,]])
    # remove duplicated text in first row
    for(i in 1:length(m[1,])) m[1,i]<-gsub(paste0("(",specialChars(m[2,i]),") ",specialChars(m[2,i]),"$"),"\\1",m[1,i])
    # remove second row
    m<-m[-2,]
  }
  
  # remove tailoring white spaces
  m<-gsub("^  *|  *$","",m)
  
  return(m)
  }

# paste first and second column with ", "
# if second column doesn't contain any number or letters in every cell
textColHandling<-function(x){
  if(!is.matrix(x)) return(x)
  loop<-TRUE
  while((isTRUE(loop) & length(grep("[0-9]",x[,2]))==0)|
        (isTRUE(loop) & length(grep("[A-z]",x[,2]))==nrow(x))
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
  
  # repeat first cell if all other cells in same row are empty
  if(nrow(m)>2 & ncol(m)>2){ 
    j<-rowSums(!m[,-1]=="")==0
    if(sum(j)==0){
    # except lines that start with a number and following line as well
    id<-which(j)
    if(length(id)>0)
    for(k in 1:length(id))
      if(k<nrow(m))
        if(grepl("^[1-9]",m[id[k],1]) & 
            grepl("^[1-9]",m[id[k]+1,1]))
          j[id[k]]<-FALSE
    
    # only if there is enough (>.5) lines with content
    if(sum(j)>0 & sum(j)/length(j)<.5) m[j,]<-m[j,1]
    }
    }
  
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
  
  # remove brackets around enumeration
  m[1,]<-gsub("^\\(([0-9][0-9]*\\.*)\\)","\\1",m[1,])
  m[,1]<-gsub("^\\(([0-9][0-9]*\\.*)\\)","\\1",m[,1])
  # move number in brackets at end to front
  m[1,]<-gsub("(.*) \\(([0-9][0-9]*\\.*)\\)$","\\2 \\1",m[1,])
  m[,1]<-gsub("(.*) \\(([0-9][0-9]*\\.*)\\)$","\\2 \\1",m[,1])
  
  # remove bold/italic from first row and col, and stars and coded p-values at end behind letter
  m[1,]<-gsub("\\^bold|\\^italic|([A-z]) *\\^*\\*\\**","\\1",m[1,])
  m[,1]<-gsub("\\^bold|\\^italic|([A-z]) *\\^*\\*\\**","\\1",m[,1])
  m
  # if coding is in first row
  if(sum(nchar(m[1,]),na.rm=TRUE)>sum(nchar(m[,1]),na.rm=TRUE)){
    fullNames<-m[1,] 
    # get numeric coding and varNames
    coding<-gsub("^([0-9][0-9]*).*|^[^:]*: ([0-9][0-9]*).*","\\1\\2",fullNames)
    coding[grep("^[0-9]",coding,invert=TRUE)]<-""
    varNames<-gsub("^[0-9][^A-z]*([A-z])|^[^:]*: [0-9][^A-z]*([A-z])","\\1\\2",fullNames)
    
    codes<-varNames[coding!=""]
    coding<-coding[coding!=""]
    rowname<-gsub("\\.$","",m[,1])
    
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
    
    # add numbers from first row to first column if has no numbers
    # and matrix is quadratic
    if(length(m[1,])==length(m[,1]) & 
       length(grep("^[1-9]",m[,1]))==0 &
       length(grep("^[1-9][0-9]*\\.*$",m[1,]))==(ncol(m)-1)
    )
      m[-1,1] <-  paste0(gsub("\\.","",m[1,-1]),". ",m[-1,1])
    
    # if nrow-1==ncol
    if(ncol(m)==(nrow(m)-1) & 
       length(grep("^[1-9]",m[,1]))==0 &
       grepl("^$|^1[\\.0]*$",m[2,2]) &
       length(grep("^[1-9][0-9]*\\.*$",m[1,]))==(ncol(m)-1)
    )
      m[-1,1]<-paste0(gsub("\\.","",m[1,-1][1]:(nrow(m)-1)),". ",m[-1,1])
      
    # get numeric coding and varNames
    fullNames<-m[,1] 
    coding<-gsub("^([0-9][0-9]*).*|^[^:]*: ([0-9][0-9]*).*","\\1\\2",fullNames)
    coding[grep("^[0-9]",coding,invert=TRUE)]<-""
    varNames<-gsub("^[0-9][^A-z]*([A-z])|^[^:]*: [0-9][^A-z]*([A-z])","\\1\\2",fullNames)
    
    codes<-varNames[coding!=""]
    coding<-coding[coding!=""]
    
    colname<-m[1,]
    
    if((sum(is.element(c(1,2,3),gsub("^0","",coding)))==3|sum(is.element(c(2,3,4),gsub("^0","",coding)))==3) &
       sum(nchar(coding))!=0){
          for(i in 1:length(coding)) colname<-gsub(paste0("^",coding[i],"[^0-9]*$"),codes[i],colname)
      # override names
      m[1,]<-colname
      m[,1]<-varNames  
    }
  } 
  
  # if first row still contains numbers and nrow=ncol
  if(sum(is.element(c(1,2,3),gsub("^0","",m[1,])))==3){
    if(nrow(m)==ncol(m))
      m[1,]<-m[,1]
  }
  
  # add space between letter-bracket-letters
  m<-gsub("([A-z])(\\([A-z][-A-z _]*\\))","\\1 \\2",m)
  
  
return(m)
}# end function coding2variable

## Function to handle correlation tables
extractCorrelations<-function(x,
                              legendCodes=NULL,
                              remove=FALSE,decodeP=TRUE,noSign2p=TRUE,
                              standardPcoding=TRUE,addNasDF=TRUE){
  if(isFALSE(decodeP)) noSign2p<-FALSE
  # prepare legend codings
  parentheses<-NULL;brackets<-NULL;psign<-NULL;pval<-NULL;italic<-NULL;bold<-NULL;N<-NULL;diagonal<-NULL;boldValue<-NULL;italicValue<-NULL
  # get legend codings
  if(!is.list(legendCodes)) legendCodes<-legendCodings(legendCodes)
  if(is.list(legendCodes)){
    psign<-legendCodes$psign
    pval<-legendCodes$pval
    italic<-legendCodes$italicP
    bold<-legendCodes$boldP
    italicValue<-legendCodes$italicValue
    boldValue<-legendCodes$boldValue
    N<-gsub("[Nn]=","",legendCodes$N)
    diagonal<-legendCodes$diagonal
  }
  if(isFALSE(addNasDF)) N<-NULL
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
  m
  # remove brackets around numbers from first row and col
  m[1,]<-gsub("^ *\\(([1-9][0-9]*\\.*)\\)","\\1",m[1,])
  m[,1]<-gsub("^ *\\(([1-9][0-9]*\\.*)\\)","\\1",m[,1])
  
  # paste first and second column, if first column has enumeration only and all cells in second column start with letters
  col1<-m[,1]
  if(length(col1)>1&
     sum(gsub("[0-9\\[:punct:]]","",col1[-1])=="",na.rm=TRUE) == length(col1[-1]) &
     sum(grepl("^[A-z]",m[-1,2])) == length(col1[-1])
     ){
    col2<-m[,2]
    m[,1]<-gsub("^ | $","",paste(col1,col2))
    m<-m[,-2]
  }
  
  if(ncol(m)<2|nrow(m)<2) return(m)
  
  nCol<-ncol(m)     
  # convert signs to p-values
  if(isTRUE(decodeP)) m[-1,-1]<-sign2p(m[-1,-1],sign=psign,val=pval)
  
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
  m
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
  m
  
  # set rows/cols with mean/sd/etc to FALSE
  cors[grep("eta\\^*2|omega\\^2|^rho\\^*2*$|^ICC|intraclass correlation|Cronbach|^[Mm]eans* *[Mm]*$| [Mm]eans*$|^M$|[Ss]ta*n*da*r*d*\\.* [Dd]ev\\.*i*a*t*i*o*n*|^[aA]lpha$| [aA]lpha$",m[,1]),-1]<-FALSE
  cors[grep("^SD[Ss]*$| SD[Ss]*$|^AVE[^A-z]|[^A-z]AVES*$|^CR |^CR$|^AVE$|^MSV$|^ASV$|Skewnes|[KC]urtosis|[Rr]eliabilit|^[Vv]ariance| [Vv]ariance|[sS]quared",m[,1]),-1]<-FALSE
  cors[-1,grep("eta\\^*2|omega\\^2|^rho\\^*2*$|^ICC|intraclass correlation|Cronbach|^[Mm]eans* *[Mm]*$| [Mm]eans*$|^M$|[Ss]ta*n*da*r*d*\\.* [Dd]ev\\.*i*a*t*i*o*n*|^[aA]lpha$| [aA]lpha$",m[1,])]<-FALSE
  cors[-1,grep("^SD[Ss]*$| SD[Ss]*$|^[aA]lpha$| [aA]lpha$|^CR |^CR$|^AVE[^A-z]|[^A-z]AVES*$|^AVE$|^MSV|^ASV$$|Skewnes|[KC]urtosis|[Rr]eliabilit|^[Vv]ariance| [Vv]ariance|[sS]quared",m[1,])]<-FALSE
  
  #i<-which(rowSums(cors[-1,-1]==FALSE,na.rm=T)==0)+1
  #j<-which(colSums(cors[-1,-1]==FALSE,na.rm=T)==0)+1
  
  # convert numbered variable name to full name label 
  m<-coding2variable(m)  

  cors[grep("^S\\.*D\\.*$|^sd$|^Var\\.$|^[Mm]in\\.*$|^[Mm]ax\\.*$|^[Rr]ange$|^RMSEA*|^SRMR|^p$|delta [A-z][^a-z]|delta [A-z]$",m[,1]),-1]<-FALSE
  cors[-1,grep("^S\\.*D\\.*$|^sd$|^Var\\.$|^[Mm]in\\.*$|^[Mm]ax\\.*$|^[Rr]ange$|^RMSEA*|^SRMR|^p$|delta [A-z][^a-z]|delta [A-z]$",m[1,])]<-FALSE
  cors[grep("^[Rr]\\^2| [Rr]\\^2|$[Rr]2$|[Cc]hi\\^*2|^[tpdr]$",m[,1]),-1]<-FALSE
  cors[-1,grep("^[Rr]\\^2| [Rr]\\^2|$[Rr]2$|[Cc]hi\\^*2|^[tpdr]$",m[1,])]<-FALSE
  
  # if col/row has name N and numbers>1 set cells to false
  i<-grepl("^[Nn]$",m[1,])
  if(sum(i)>0)
     m[-1,i][m[-1,i]>=1] <- FALSE
  
  i<-grepl("^[Nn]$",m[,1])
  if(sum(i)>0)
    m[i,-1][m[i,-1]>=1] <- FALSE
  
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
  if(isTRUE(standardPcoding)){
  if(length(grep("\\*$",corTab))>0){
    #corTab[which(cors)]<-gsub("([0-9])$","\\1;; p>.05",corTab[which(cors)])
    corTab[which(cors)]<-gsub("\\^*\\*\\*\\*$",";; p<.001",corTab[which(cors)])
    corTab[which(cors)]<-gsub("\\^*\\*\\*$",";; p<.01",corTab[which(cors)])
    corTab[which(cors)]<-gsub("\\^*\\*$",";; p<.05",corTab[which(cors)])
     }
    }
  
  corTab<-gsub("r=([<>])","r\\1",corTab)

  # add "p > max(p<x)" if has added p-value or coding in legend
  if(isTRUE(noSign2p)){
  if(length(grep(";; p<",corTab))>0|length(grep("p<",pval))>0){
  if(length(grep("p<",pval))>0) 
    Pmax<-suppressWarnings(max(as.numeric(
              gsub("p<=*","",grep("p<=*",gsub("p=0*\\.0([51])","p<.0\\1",grep("p<",pval,value=TRUE)),value=T)))))
  if(length(grep("p<",pval))==0) 
    Pmax<-suppressWarnings(max(as.numeric(
              gsub("^(0*.*[0-9][0-9]*).*","\\1",gsub(".*;; p<=*","",grep(";; p<",corTab,value=TRUE))))))

  corTab<-gsub("^(r=-*0*\\.[0-9][0-9]*)$",paste0("\\1;; p>",Pmax),corTab)
  }
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
  
  # insert diaginal statistic instead of r=
  if(length(diagonal)>0){
    x<-gsub("^(.*) <<~>>.*","\\1",correlations)
    y<-gsub(".*<<~>> (.*): r[=\\(].*","\\1",correlations)
    i<-which(tolower(x)==tolower(y))
    correlations[i]<-gsub("(<<~>>[^:]*: )r\\(*[0-9]*\\)*=",paste0("\\1",diagonal,"="),correlations[i])
    correlations[i]<-gsub(paste0("(",diagonal,"=-*[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*"),"\\1",correlations[i])
  }
  
  # insert bolded statistic instead of r=
  if(length(boldValue)>0){
    x<-gsub("^(.*) <<~>>.*","\\1",correlations)
    y<-gsub(".*<<~>> (.*): r[=\\(].*","\\1",correlations)
    i<-which(tolower(x)==tolower(y))
    correlations[i]<-gsub("(<<~>>[^:]*: )r\\(*[0-9]*\\)*=",paste0("\\1",boldValue,"="),correlations[i])
    correlations[i]<-gsub(paste0("(",boldValue,"=-*[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*"),"\\1",correlations[i])
  }
  
  # insert italic statistic instead of r=
  if(length(italicValue)>0){
    x<-gsub("^(.*) <<~>>.*","\\1",correlations)
    y<-gsub(".*<<~>> (.*): r[=\\(].*","\\1",correlations)
    i<-which(tolower(x)==tolower(y))
    correlations[i]<-gsub("(<<~>>[^:]*: )r\\(*[0-9]*\\)*=",paste0("\\1",italicValue,"="),correlations[i])
    correlations[i]<-gsub(paste0("(",italicValue,"=-*[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*"),"\\1",correlations[i])
  }
  
  # insert "alpha" for r if var x and var y are the same and 0<r<1
  x<-gsub("^(.*)<<~>>.*","\\1",correlations)
  y<-gsub(".*<<~>>(.*): r[=\\(].*","\\1",correlations)
  i<-which(tolower(x)==tolower(y))
  correlations[i]<-gsub("(<<~>>[^:]*: )r\\(*[0-9]*\\)*=([0\\.])",paste0("\\1alpha=\\2"),correlations[i])
  
  # remove p values behind alpha
  correlations[i]<-gsub("(alpha=[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*","\\1",correlations[i])
  
  # convert square root sign and AVE
  correlations<-gsub("\u221a","sqrt",correlations)
  correlations<-gsub("[Aa]verage [Vv]ariance [Ee]xtracted","AVE",correlations)

  # replace alpha with AVE if is followed by AVE
  correlations[i]<-gsub("alpha(=[0-9\\.][0-9\\.]*)[ ,;].*[Ss]quare.*AVE.*","sqrt AVE\\1",correlations[i])
  correlations[i]<-gsub("alpha(=[0-9\\.][0-9\\.]*)[ ,;].*sqrt *AVE*.*","sqrt AVE\\1",correlations[i])
  correlations[i]<-gsub("alpha(=[0-9\\.][0-9\\.]*)[ ,;].*AVE.*|alpha(=[0-9\\.][0-9\\.]*) .*[ \\(]AVE\\)*.*","AVE\\1\\2",correlations[i])
  
  # add (Nmax-2) to "r=" if Nmax>1
  N<-N[N>1]
  if(length(N)>0){
    Nmax<-suppressWarnings(max(as.numeric(N),na.rm=T))
    if(!is.na(Nmax)){
      correlations<-gsub(" r=",paste0(" r(",Nmax-2,")="),correlations)
      # if line has [Nn]=num replace number in brackets with N-2 
      df<-suppressWarnings(as.numeric(gsub(".*([^A-z][Nn]=)([1-9][0-9]*)(.* r\\()[1-9][0-9]*\\).*","\\2",correlations))-2)
      j<-which(!is.na(df))
      if(length(j)>0) 
        for(i in j)
        correlations[i]<-(gsub("([^A-z][Nn]=)([1-9][0-9]*)(.* r\\()[1-9][0-9]*\\)",paste0("\\1\\2\\3",df[i],")"),correlations[i]))
      }
    }
  
  
  # output
  if(isFALSE(remove)) return(correlations)
  if(isTRUE(remove)) return(m)
} # end extractCorrelations


## Function to handle correlation tables
extractMatrix<-function(x,
                        legendCodes=NULL,
                        statistic="VALUE",      
                        range=NULL,
                        remove=FALSE,
                        decodeP=TRUE,
                        noSign2p=TRUE,
                        standardPcoding=TRUE){
  if(isFALSE(decodeP)) noSign2p<-FALSE
  # prepare legend codings
  parentheses<-NULL;brackets<-NULL;psign<-NULL;pval<-NULL;italic<-NULL;bold<-NULL;N<-NULL;diagonal<-NULL;boldValue<-NULL;italicValue<-NULL
  # get legend codings
  if(!is.list(legendCodes)) legendCodes<-legendCodings(legendCodes)
  if(is.list(legendCodes)){
    psign<-legendCodes$psign
    pval<-legendCodes$pval
    italic<-legendCodes$italicP
    bold<-legendCodes$boldP
    italicValue<-legendCodes$italicValue
    boldValue<-legendCodes$boldValue
    N<-gsub("[Nn]=","",legendCodes$N)
    diagonal<-legendCodes$diagonal
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
  m
  # remove brackets around numbers from first row and col
  m[1,]<-gsub("^ *\\(([1-9][0-9]*\\.*)\\)","\\1",m[1,])
  m[,1]<-gsub("^ *\\(([1-9][0-9]*\\.*)\\)","\\1",m[,1])
  
  # paste first and second column, if first column has enumeration only and all cells in second column start with letters
  col1<-m[,1]
  if(length(col1)>1&
     sum(gsub("[0-9\\[:punct:]]","",col1[-1])=="",na.rm=TRUE) == length(col1[-1]) &
     sum(grepl("^[A-z]",m[-1,2])) == length(col1[-1])
  ){
    col2<-m[,2]
    m[,1]<-gsub("^ | $","",paste(col1,col2))
    m<-m[,-2]
  }
  
  if(ncol(m)<2|nrow(m)<2) return(m)
  
  nCol<-ncol(m)     
  # convert signs to p-values
  if(isTRUE(decodeP)) m[-1,-1]<-sign2p(m[-1,-1],sign=psign,val=pval)
  
  m
  # unify numbering at beginning of first column/row to "number."
  m[,1]<-gsub("^\\(*0*([1-9][0-9]*)[\\),;:] ","\\1. ",m[,1])
  m[1,]<-gsub("^\\(*0*([1-9][0-9]*)[\\),;:] ","\\1. ",m[1,])
  
  ##############################################################
  # including range
  values<-suppressWarnings(as.numeric(gsub("[^0-9\\.-]|;;.*|\\^.*","",m)))
  # prepare removal matrix of correlations
  rem<-abs(values)>=0
  if(!is.null(range))
    rem<-values<=sort(range)[2]&values>=sort(range)[1]
  # select correlations 
  values<-abs(values)>=0
  if(!is.null(range))
    values<-values<=sort(range)[2]&values>=sort(range)[1]
  
  values<-matrix(values,ncol=nCol)
  rem<-matrix(rem,ncol=nCol)
  
  # if first row has increasing numbers but first column has not add detected numbers to text in first column
  if(sum(is.element(c("1","2","3"),gsub("[\\.\\(\\) ]*","",m[1,-1])))==3 & 
     sum(is.element(c("1","2","3"),gsub("^([1-9]).*","\\1",m[-1,1])))==0){
    i<-grep("^[0-9][0-9]*$",gsub("[\\.\\(\\) ]*","",m[1,-1]))
    if(length(i)==max(i)&max(i)<=(nrow(m)-1)){
      m[1,i+1]<-m[2:(max(i)+1),1]
    }
  }
  m
  # if matrix has enumeration in rows and cols, 
  if((length(grep("^[0-9][0-9\\.]*$",m[1,-1]))>2&length(grep("^[0-9].*[A-z]",m[-1,1]))>2)|
     (length(grep("^[0-9].*[A-z]",m[1,-1]))>2&length(grep("^[0-9][0-9\\.]*$",m[-1,1]))>2)){
    # deselect lines for extraction 
    cols<-grep("^[1-9]| [1-9]*[0-9]$",m[1,],invert=TRUE)
    rows<-grep("^[1-9]| [1-9]*[0-9]$",m[,1],invert=TRUE)
    values[,cols]<-FALSE
    values[rows,]<-FALSE
    rem[rows,]<-FALSE
    rem[,cols]<-FALSE
  }
  # set rows/cols with mean/sd/etc to FALSE
  values[grep("eta\\^*2|omega\\^2|^rho\\^*2*$|^ICC|intraclass correlation|Cronbach|^[Mm]eans* *[Mm]*$| [Mm]eans*$|^M$|[Ss]ta*n*da*r*d*\\.* [Dd]ev\\.*i*a*t*i*o*n*|^[aA]lpha$| [aA]lpha$",m[,1]),-1]<-FALSE
  values[grep("^SD[Ss]*$| SD[Ss]*$|^AVE[^A-z]|[^A-z]AVES*$|^CR |^CR$|^AVE$|^MSV$|^ASV$|Skewnes|[KC]urtosis|[Rr]eliabilit|^[Vv]ariance| [Vv]ariance|[sS]quared",m[,1]),-1]<-FALSE
  values[-1,grep("eta\\^*2|omega\\^2|^rho\\^*2*$|^ICC|intraclass correlation|Cronbach|^[Mm]eans* *[Mm]*$| [Mm]eans*$|^M$|[Ss]ta*n*da*r*d*\\.* [Dd]ev\\.*i*a*t*i*o*n*|^[aA]lpha$| [aA]lpha$",m[1,])]<-FALSE
  values[-1,grep("^SD[Ss]*$| SD[Ss]*$|^[aA]lpha$| [aA]lpha$|^CR |^CR$|^AVE[^A-z]|[^A-z]AVES*$|^AVE$|^MSV|^ASV$$|Skewnes|[KC]urtosis|[Rr]eliabilit|^[Vv]ariance| [Vv]ariance|[sS]quared",m[1,])]<-FALSE
  
  # convert numbered variable name to full name label 
  m<-coding2variable(m)  
  
  values[grep("^S\\.*D\\.*$|^sd$|^Var\\.$|^[Mm]in\\.*$|^[Mm]ax\\.*$|^[Rr]ange$|^RMSEA*|^SRMR|^p$|delta [A-z][^a-z]|delta [A-z]$",m[,1]),-1]<-FALSE
  values[-1,grep("^S\\.*D\\.*$|^sd$|^Var\\.$|^[Mm]in\\.*$|^[Mm]ax\\.*$|^[Rr]ange$|^RMSEA*|^SRMR|^p$|delta [A-z][^a-z]|delta [A-z]$",m[1,])]<-FALSE
  values[grep("^[Rr]\\^2| [Rr]\\^2|$[Rr]2$|[Cc]hi\\^*2|^[tpdr]$",m[,1]),-1]<-FALSE
  values[-1,grep("^[Rr]\\^2| [Rr]\\^2|$[Rr]2$|[Cc]hi\\^*2|^[tpdr]$",m[1,])]<-FALSE
  
  # add pretext again
  m[,1]<-paste0(preText,m[,1])
  
  # select correlation matrix and replicate var names
  valueTab<-m
  valueTab[-1,-1][-which(values[-1,-1])]<-""
  # set first row and column to FALSE
  values[1,]<-FALSE
  values[,1]<-FALSE
  # add STATISTIC= to cells with correlations
  valueTab[which(values)]<-gsub("^([-\\.]*[\\.0-9][\\.0-9[:punct:]]*)","STATISTIC=\\1",valueTab[which(values)])
  # stars2p if still has star and standardPcoding==TRUE
  if(isTRUE(standardPcoding)&isTRUE(decodeP)){
    if(length(grep("\\*$",valueTab))>0){
      #valueTab[which(values)]<-gsub("([0-9])$","\\1;; p>.05",valueTab[which(values)])
      valueTab[which(values)]<-gsub("\\^*\\*\\*\\*$",";; p<.001",valueTab[which(values)])
      valueTab[which(values)]<-gsub("\\^*\\*\\*$",";; p<.01",valueTab[which(values)])
      valueTab[which(values)]<-gsub("\\^*\\*$",";; p<.05",valueTab[which(values)])
    }}
  
  valueTab<-gsub("STATISTIC=([<>])","STATISTIC\\1",valueTab)
  
  # add "p > max(p<x)" if has added p-value or coding in legend
  if(isTRUE(noSign2p)){
    if(length(grep(";; p<",valueTab))>0|length(grep("p<",pval))>0){
      if(length(grep("p<",pval))>0) 
        Pmax<-suppressWarnings(max(as.numeric(
          gsub("p<=*","",grep("p<=*",gsub("p=0*\\.0([51])","p<.0\\1",grep("p<",pval,value=TRUE)),value=T)))))
      if(length(grep("p<",pval))==0) 
        Pmax<-suppressWarnings(max(as.numeric(
          gsub("^(0*.*[0-9][0-9]*).*","\\1",gsub(".*;; p<=*","",grep(";; p<",valueTab,value=TRUE))))))
      
      valueTab<-gsub("^(STATISTIC=-*0*\\.[0-9][0-9]*)$",paste0("\\1;; p>",Pmax),valueTab)
    }
  }
  
  # add "p < min(p>x)" if has added p-value or coding in legend
  if(isTRUE(decodeP))
  if(length(grep(";; p>",valueTab))>0|length(grep("p>",pval))>0){
    if(length(grep("p>",pval))>0) 
      Pmin<-suppressWarnings(min(as.numeric(
        gsub("p>=*","",grep("p<|p>",gsub("p=0*\\.0([51])","p>.0\\1",grep("p>",pval,value=TRUE)),value=T)))))
    if(length(grep("p>",pval))==0) 
      Pmin<-suppressWarnings(min(as.numeric(
        gsub("^(0*.*[0-9][0-9]*).*","\\1",gsub(".*;; p>=*","",grep(";; p>",valueTab,value=TRUE))))))
    
    valueTab<-gsub("^(STATISTIC=-*0*\\.[0-9][0-9]*)$",paste0("\\1;; p<",Pmin),valueTab)
  }
  
  # extract correlations as vector
  correlations<-NULL
  for(i in 2:nrow(valueTab))
    for(j in 2:ncol(valueTab))
      if(valueTab[i,j]!="")
        correlations<-c(correlations,paste0(valueTab[i,1]," <<~>> ",valueTab[1,j],": ",valueTab[i,j]))
  
  # set extracted correlations to "" in matrix
  m[-1,-1][valueTab[-1,-1]!=""]<-""
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
  
  # insert diaginal statistic instead of r=
  if(length(diagonal)>0){
    x<-gsub("^(.*) <<~>>.*","\\1",correlations)
    y<-gsub(".*<<~>> (.*): r=.*","\\1",correlations)
    i<-which(x==y)
    correlations[i]<-gsub("(<<~>>[^:]*: )r\\(*[0-9]*\\)*=",paste0("\\1",diagonal,"="),correlations[i])
    correlations[i]<-gsub(paste0("(",diagonal,"=-*[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*"),"\\1",correlations[i])
  }
  
  # insert bolded statistic instead of r=
  if(length(boldValue)>0){
    x<-gsub("^(.*) <<~>>.*","\\1",correlations)
    y<-gsub(".*<<~>> (.*): r[=\\(].*","\\1",correlations)
    i<-which(tolower(x)==tolower(y))
    correlations[i]<-gsub("(<<~>>[^:]*: )r\\(*[0-9]*\\)*=",paste0("\\1",boldValue,"="),correlations[i])
    correlations[i]<-gsub(paste0("(",boldValue,"=-*[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*"),"\\1",correlations[i])
  }
  
  # insert italic statistic instead of r=
  if(length(italicValue)>0){
    x<-gsub("^(.*) <<~>>.*","\\1",correlations)
    y<-gsub(".*<<~>> (.*): r[=\\(].*","\\1",correlations)
    i<-which(tolower(x)==tolower(y))
    correlations[i]<-gsub("(<<~>>[^:]*: )r\\(*[0-9]*\\)*=",paste0("\\1",italicValue,"="),correlations[i])
    correlations[i]<-gsub(paste0("(",italicValue,"=-*[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*"),"\\1",correlations[i])
  }
   
  
  # insert "alpha" for r if var x and y are the same and 0<r<1
  x<-gsub("^(.*) <<~>>.*","\\1",correlations)
  y<-gsub(".*<<~>> (.*): STATISTIC=.*","\\1",correlations)
  i<-which(x==y)
  correlations[i]<-gsub("(<<~>>[^:]*: )STATISTIC\\(*[0-9]*\\)*=([0\\.])",paste0("\\1diagonal=\\2"),correlations[i])
  
  # remove p values behind alpha
  correlations[i]<-gsub("(diagonal=[0-9\\.][0-9\\.]*);; p[<=>][<=>]*[0-9\\.]*","\\1",correlations[i])
  
  # convert square root sign and AVE
  correlations<-gsub("\u221a","sqrt",correlations)
  correlations<-gsub("[Aa]verage [Vv]ariance [Ee]xtracted","AVE",correlations)
  
  # replace alpha with AVE if is followed by AVE
  correlations[i]<-gsub("alpha(=[0-9\\.][0-9\\.]*)[ ,;].*[Ss]quare.*AVE.*","sqrt AVE\\1",correlations[i])
  correlations[i]<-gsub("alpha(=[0-9\\.][0-9\\.]*)[ ,;].*sqrt *AVE*.*","sqrt AVE\\1",correlations[i])
  correlations[i]<-gsub("alpha(=[0-9\\.][0-9\\.]*)[ ,;].*AVE.*|alpha(=[0-9\\.][0-9\\.]*) .*[ \\(]AVE\\)*.*","AVE\\1\\2",correlations[i])
  
  
  correlations<-gsub("STATISTIC",statistic,correlations)
  
  # output
  if(isFALSE(remove)) return(correlations)
  if(isTRUE(remove))  return(m)
} # end extractMatrix

 
# convert signs to p-values
sign2p<-function(x,sign,val,sep=";;"){
  if(length(x)==0) return(x)
  if(length(sign)==0) return(x)
  # remove ^ in front of dagger
  x<-gsub("\\^([\u2020\u2021])","\\1",x)
  # reorder by nChar of signs
  i<-order(nchar(sign),decreasing=TRUE)
  sign<-sign[i]
  val<-val[i]
  # special sign handling (replaces * to \\*)
  sign<-specialChars(sign)

  # convert n.s. -> ;; p>Pmax
  if(length(grep("[nN]\\.*[sS]\\.*",sign))==0)
  Pmax<-gsub(".*<=*","",grep("p<=*[\\.0]",val,value=TRUE))
  if(length(grep("[nN]\\.*[sS]\\.*",sign))>0)
  Pmax<-gsub(".*[<>]=*","",val[grep("[nN]\\.*[sS]\\.*",sign)])
  if(length(Pmax)>0){
    Pmax<-max(as.numeric(Pmax))
    # num^n.s.
    x<-gsub("([0-9])[\\^ ]*[nN]\\.* *[sS]\\.*[[:punct:] ]*",paste0("\\1;; p>",Pmax,", "),x)
    # space n.s.
    x<-gsub(" \\^*[n]\\. *[s]\\.([ [:punct:]]*)",paste0(";; p>",Pmax,", \\1"),x)
    # no letter n.s.
    x<-gsub("([^a-z])[\\^]*[nN]\\. *[sS]\\.([ [:punct:]]*)",paste0("\\1;; p>",Pmax,", \\2"),x)
    # only n.s.
    x<-gsub("^ *[nN]\\. *[sS]\\. *$|^ns$",paste0(";; p>",Pmax,", \\2"),x)
    # clean up
    x<-gsub(",$|, $","",x)
  }
  
  # convert sign to pval if at end, behind number or bracket
  for(i in 1:length(sign)){
    x<-gsub(paste0(sign[i],"[\\[]"),paste0(sign[i]," ["),x)
    x<-gsub(paste0(sign[i],"[\\(]"),paste0(sign[i]," ("),x)
    # remove bold/italic behind letter/punctuation in outer matrix
    x[,1]<-gsub("([A-z[:punct:]] *)\\^bold|([A-z[:punct:]] *)\\^italic","\\1\\2",x[,1])
    x[1,]<-gsub("([A-z[:punct:]] *)\\^bold|([A-z[:punct:]] *)\\^italic","\\1\\2",x[1,])
    # and behind letter in full matrix
    x<-gsub("([A-z[:punct:]] *)\\^bold|([A-z] *)\\^italic","\\1\\2",x)
    # replace sign with p value,
    # if has no letter-num-sign and if at end
    incl<-!grepl(paste0("[A-z][-_]*[0-9][0-9]*",sign[i]),x)
    x[incl]<-gsub(paste0("[0-9\\)\\]] *\\^*",sign[i],"[[:punct:]]*$"),
              paste(sep,val[i]),x[incl])
    # and sign is not between a pair of letters or pair of numbers
    incl<-incl & !grepl(paste0("[A-z] *\\^*",sign[i]," *[[:alpha:]]",
                       "|[0-9] *\\^*",sign[i]," *[0-9]"),x)
    x[incl]<-gsub(paste0(" *\\^*",sign[i],"([,; 0-9])| *\\^*",sign[i],"[[:punct:]]*$"),
            # p value with seperator     
            paste0(sep," ",val[i],"\\1"),x[incl])
    x
    # clean up if cell has ^;; p-val
    x<-gsub(paste0(" *\\^(",sep," )"),"\\1",x)
    # clean up if cell starts with seperator
#    x<-gsub(paste0("^",sep," "),"",x)
    # clean up before bracket
    x<-gsub(", \\)",")",x)
    x<-gsub(", \\]",")",x)
  }
  
  x<-gsub("[:,;]* ;;",";;",x)
  x<-gsub("  *"," ",x)
  x<-gsub("[,;] $","",x)
  x<-gsub(" \\((;; p[<=>][<=>]*[\\.0][0-9\\.]*),* *\\)","\\1",x)

  return(x)
}

## convert non existant p sign to p>max(p)
noSign2p<-function(x,pval){
  if(!is.matrix(x)) return(x)
  # escape if has already non significant and significant imputed p-values 
  if(length(grep(";; p>=*[\\.0]",x))>0 & length(grep(";; p<=*[\\.0]",x))>0) return(x)
  
  ##########################################
  ## for cases with no insignificant p but significicant p inserted
  if(length(grep(";; p>=*[\\.0]",x))==0 & length(grep(";; p<=*[\\.0]",x))>0){
  # remove p-values with ">" in coding
  i<-grep(">=*|[^<]=",pval,invert=TRUE)
  pval<-pval[i]
  # escape
  if(length(pval)==0) return(x)
  nCol<-ncol(x)
  # find cloumns with already inserted p-values and no results in front
  cols<-which(colSums(matrix(grepl(";; p<",x),ncol=nCol))>0)
  # escape
  if(length(cols)==0) return(x)
  # get max of coded p
  if(length(pval)>0) Pmax<-max(as.numeric(gsub("p<=*","",pval)))
  if(length(pval)==0) Pmax<-max(as.numeric(gsub("^(0*.*[0-9][0-9]*).*","\\1",
                                                gsub(".*;; p<=*","",grep(";; p<",x[,cols],value=TRUE)))))
  
  ## find cloumns with already inserted p-values and only number in front
  cols<-which(colSums(matrix(grepl("^[<=>]*-*[\\.0-9][\\.0-9]*;; p<",x),ncol=nCol))>0)

  # go through columns and add p>Pmax in cells without ;; p>
  if(length(cols)>0)
  for(j in cols){
    cells<-!grepl(";; p<|;; p>",x[,j]) & !grepl("^$",x[,j])&
               grepl("^[<=>]*-*[\\.0-9][\\.0-9]*$",x[,j])
    # impute p>max(p<num)
    x[cells,j]<-gsub("([0-9])$",paste0("\\1;; p>",Pmax),x[cells,j])
    if(j>1)  x[cells,j][grepl("[0-9]$",x[cells,j-1])] <- 
               gsub("^$",paste0(";; p>",Pmax),x[cells,j][grepl("[0-9]$",x[cells,j-1])])
  }
  
  ## find cloumns with already inserted p-values and results in front
  cols<-which(colSums(matrix(grepl("[A-z\\)]\\^*2* *[<=>][<=>]*-*[\\.0-9][\\.0-9]*;; p<",x)|
                             grepl("[\\.0-9] *[;-] *-*[\\.0-9][\\.0-9]*[\\]\\)]*;; p<",x)
                             ,ncol=nCol))>0)
  
  # go through columns and add p>Pmax in cells without ;; p>
  if(length(cols)>0)
    for(j in cols){
      cells<-!grepl(";; p<|;; p>",x[,j]) & !grepl("^$",x[,j])&
        (grepl("[A-z\\)]\\^*2* *[<=>][<=>]*-*[\\.0-9][\\.0-9]*$",x[,j])|
           grepl("[\\.0-9] *[;-] *-*[\\.0-9][\\.0-9]*$",x[,j])|
           grepl("[\\.0-9] *[;-] *-*[\\.0-9][\\.0-9]*\\)*\\]*$",x[,j])
        )
      # set first cell to FALSE 
      #cells[1]<-FALSE
      # impute p>max(p<num)
      x[cells,j]<-gsub("([0-9]\\]*\\)*)$",paste0("\\1;; p>",Pmax),x[cells,j])
      if(j>1)  x[cells,j][grepl("[0-9]\\]*\\)*$",x[cells,j-1])] <- 
        gsub("^$",paste0(";; p>",Pmax),x[cells,j][grepl("[0-9]\\]*\\)*$",x[cells,j-1])])
    }
  
  ## find cloumns with already inserted p-values and nothing in front
  cols<-which(colSums(matrix(grepl("^;; p<",x)
                             ,ncol=nCol))>0)
  
  # go through columns and add p>Pmax in without ;; p>
  if(length(cols)>0)
    for(j in cols){
      cells<-!grepl("^;; p<|^;; p>",x[,j]) &
                 grepl("^$",x[,j])
      # set first cell to FALSE 
      #cells[1]<-FALSE
      # impute p>max(p<num)
      x[cells,j]<-paste0(";; p>",Pmax)
      if(j>1)  x[cells,j][grepl("[0-9]$",x[cells,j-1])] <- 
        gsub("^$",paste0(";; p>",Pmax),x[cells,j][grepl("[0-9]$",x[cells,j-1])])
    }
  
  # clean up brackets
  x<-gsub(" \\((;; p[<=>][<=>]*[\\.0][0-9\\.]*),* *\\)","\\1",x)
  x<-gsub(" \\[(;; p[<=>][<=>]*[\\.0][0-9\\.]*),* *\\]","\\1",x)
  
  }
  
  ###############################################################
  ## for cases with inserted insignificant but no significant values
  if(length(grep(";; p>=*[\\.0]",x))>0&length(grep(";; p<=*[\\.0]",x))==0){
    pval<-gsub(".*;; p>=*([\\.0][0-9\\.]*).*","\\1",grep(";; p>=*[\\.0]",x,value=TRUE)[1])
    # escape
    if(length(pval)==0) return(x)
    nCol<-ncol(x)
    ## find cloumns with already inserted p-values and only number in front
    cols<-which(colSums(matrix(grepl("^[<=>]*-*[\\.0-9][\\.0-9]*;; p>",x),ncol=nCol))>0)
    
    # go through columns and add p<pval in cells without ;; p<
    if(length(cols)>0)
      for(j in cols){ 
        cells<-!grepl(";; p<|;; p>",x[,j]) & !grepl("^$",x[,j])&
          grepl("^[<=>]*-*[\\.0-9][\\.0-9]*$",x[,j])
        # impute p<pval(p<num)
        x[cells,j]<-gsub("([0-9])$",paste0("\\1;; p<",pval),x[cells,j])
        if(j>1)  x[cells,j][grepl("[0-9]$",x[cells,j-1])] <- 
          gsub("^$",paste0(";; p<",pval),x[cells,j][grepl("[0-9]$",x[cells,j-1])])
      }
    
    ## find cloumns with already inserted p-values and results in front
    cols<-which(colSums(matrix(grepl("[A-z\\)]\\^*2* *[<=>][<=>]*-*[\\.0-9][\\.0-9]*;; p>",x)|
                                 grepl("[\\.0-9] *[;-] *-*[\\.0-9][\\.0-9]*[\\]\\)]*;; p>",x)
                               ,ncol=nCol))>0)
    
    # go through columns and add p<pval in cells without ;; p>
    if(length(cols)>0)
      for(j in cols){
        cells<-!grepl(";; p>|;; p<",x[,j]) & !grepl("^$",x[,j])&
          (grepl("[A-z\\)]\\^*2* *[<=>][<=>]*-*[\\.0-9][\\.0-9]*$",x[,j])|
             grepl("[\\.0-9] *[;-] *-*[\\.0-9][\\.0-9]*$",x[,j])|
             grepl("[\\.0-9] *[;-] *-*[\\.0-9][\\.0-9]*\\)*\\]*$",x[,j])
          )
        # set first cell to FALSE 
        #cells[1]<-FALSE
        # impute p<pval(p<num)
        x[cells,j]<-gsub("([0-9]\\]*\\)*)$",paste0("\\1;; p<",pval),x[cells,j])
        if(j>1)  x[cells,j][grepl("[0-9]\\]*\\)*$",x[cells,j-1])] <- 
          gsub("^$",paste0(";; p<",pval),x[cells,j][grepl("[0-9]\\]*\\)*$",x[cells,j-1])])
      }
    
    ## find cloumns with already inserted p-values and nothing in front
    cols<-which(colSums(matrix(grepl("^;; p>",x)
                               ,ncol=nCol))>0)
    
    # go through columns and add p<pval in without ;; p>
    if(length(cols)>0)
      for(j in cols){
        cells<-!grepl("^;; p>|^;; p<",x[,j]) &
          grepl("^$",x[,j])
        # set first cell to FALSE 
        #cells[1]<-FALSE
        # impute p<pval(p<num)
        x[cells,j]<-paste0(";; p<",pval)
        if(j>1)  x[cells,j][grepl("[0-9]$",x[cells,j-1])] <- 
          gsub("^$",paste0(";; p<",pval),x[cells,j][grepl("[0-9]$",x[cells,j-1])])
      }
    
    # clean up brackets
    x<-gsub(" \\((;; p[<=>][<=>]*[\\.0][0-9\\.]*),* *\\)","\\1",x)
    x<-gsub(" \\[(;; p[<=>][<=>]*[\\.0][0-9\\.]*),* *\\]","\\1",x)
  }
  
    return(x)
}

# convert brackets to values
bracket2value<-function(x,value,type=c("parentheses","brackets")[1],sep=","){
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
    x<-gsub("([^<=>])=([<>])","\\1\\2",x)
  if(type=="bracket") 
    x<-gsub("\\[([^\\]*[0-9][^\\]*)\\]",paste0(paste(sep,value),"=","\\1"),x)    
    x<-gsub("([^<=>])=([<>])","\\1\\2",x)
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
  abbr<-gsub(" "," *",abbr)
  abbr<-gsub("(\\.)([A-Z])","\\1 *\\2",abbr)
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
  # exclude cells with chi, eta, omega, R square
  j<-grep("[ -]R\\^2|[ /-]chi\\^2|[ -]eta\\^2|[ -]omega\\^2|^R\\^2|^[cC]hi\\^2|^eta\\^2|^omega\\^2",x,invert = TRUE)
  if(length(j)>0)
    temp<-x==""
    temp[j]<-TRUE
   for(i in 1:length(sup))
      x[temp]<-gsub(specialChars(sup[i]),paste0(" (",sup_label[i],")"),gsub("([^\\^])\\*","\\1^*",x[temp]))
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


## split matrix at textRow-numRows-SPLIT-textRow-NumRow
matrixSplit<-function(x){
  n<-names(x)
  if(is.matrix(x)) x<-list(x)
  if(!is.list(x)) return(x)
  # set name as attribute
  for(i in 1:length(x)) attributes(x)$name<-n[i]
  
  innerFun<-function(x){
    if(!is.matrix(x)) return(x)
    Ncol<-ncol(x)
    if(Ncol<3|nrow(x)<4) return(x)
    # find rows with second header line
    i<-which(rowSums(matrix(grepl("[[:alpha:]]|^%*$",gsub("^[Nn]\\.*[Ss]\\.*$","",x)) &
                              !grepl("[<=>]|^[Nn]\\.*[Ss]\\.*$",gsub("\\(.*\\)|\\^.*","",x)),ncol=Ncol))==Ncol &
               rowSums(matrix(grepl("^$",gsub("^[Nn]\\.*[Ss]\\.*$","",x)),ncol=Ncol)[,-1])<((Ncol-1)/2)
    )
    # correct i  
    i<-i[i>3&i<nrow(x)]
    if(length(i)>1) i<-i[c(TRUE,i[-length(i)]!=i[2:length(i)]-1)]
    if(length(i)==0) return(x)
    
    out<-list()
    att<-attributes(x)
    
    # escape if matrix is text, vector, correlation
    if(length(grep("vector|correlation|text",att))>0) {
      attributes(x)$name<-NULL
      out[[1]]<-x
      return(out)
    }
    # split matrix
      st<-c(1,i)
      en<-c(i-1,nrow(x))
      for(j in 1:length(st))
        out[[j]]<-x[st[j]:en[j],]

    # set attributes
    for(i in 1:length(out)){
      attributes(out[[i]])$caption<-att$caption
      attributes(out[[i]])$footer<-att$footer
      attributes(out[[i]])$class<-tableClass(out[[i]],legend=c(att$caption,att$footer))
      
    }
      
  # add name with letter
  names(out)<-paste0(attributes(x)$name,"",letters[1:i])
  # remove attribute name
  attributes(out)$name<-NULL
  
  return(out) 
  }
  
  out<-lapply(rapply(lapply(x,innerFun), enquote, how="unlist"), eval)
  return(out)
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

splitBeta<-function(x){
  fun<-function(y){
    y<-unlist(strsplit(gsub(", ([^,;:<=>]* ([bB]|[bB]eta)[<=>][<=>]*[^,;]*[,;];* [^<=>]*[<=>])","SPLITHERE\\1",y),"SPLITHERE"))
    if(length(y)>1){
      y[-1]<-paste0(y[1],", ",y[-1])
      y<-y[-1]}
    return(y)
  }
  return(unlist(lapply(x,fun)))
}

splitLastStat<-function(x){
      # define functions
      # split at last detected stat
      fun1<-function(x){
      lastStat<-gsub(".*, ([^<=>]* *[-A-z0-9\\^_][-A-z0-9\\^_]*)[<=>][<=>]*-*[0-9\\.][-0-9\\.\\^]*$","\\1",x)
      # except lines with coded p at end
      lastStat[grep(";; p[<>=][<>=]*[0-9\\.][0-9\\.]*$",x)]<-x
      # remove till standard stat
      lastStat<-gsub(".* ([SstTzZpPQIHdbBF][DFEdfe]*)$","\\1",lastStat)
      lastStat<-gsub(".* ([Cce][ht][ai]\\^*2*)$","\\1",lastStat)
      lastStat<-gsub(".* (omega\\^*2*)$","\\1",lastStat)
      #lastStat<-gsub(".* (R\\^*2*)$","\\1",lastStat)
      if(lastStat!=x){
      x<-gsub(paste0("([^;][^;] ",specialChars(lastStat),"[<=>][<=>]*-*[0-9\\.]*)[,:]* "),"\\1SPLITHERE",x)
      x<-unlist(strsplit(x,"SPLITHERE"))
      #if(length(x)>2){
        # add first cell content to front of new lines
        #  x[-1]<-paste0(gsub("^([^,]*), .*","\\1, ",x[1]),x[-1])
        # add stats in table num:
        #x[-1]<-paste0(gsub("^([^:]*): .*","\\1: ",x[1]),", ",x[-1])
      #}
      }
      return(x)
      }
      
      # split at imputed p-value
      fun2<-function(x){
        lastPcode<-gsub(".*(;; p)[<=>][<=>]*[0-9\\.]*$","\\1",x)
      if(sum(lastPcode!=x)>0){
        x<-gsub(paste0("(;; p[<=>][<=>]*[0-9\\.]*), "),"\\1SPLITHERE ",x)
        # remove SPLIT if in front of SE=
        x<-gsub("SPLITHERE([^,;<>=]* SE=)",",\\1",x)
        x<-unlist(strsplit(x,"SPLITHERE "))
      }
    return(x)
      }
      
      # split at stat in front of remaining coded p values
      # in lines with and without "delta"-anything
      fun3<-function(x){
        lastStat<-gsub(".*,[^<=>]* ([-A-z0-9\\^_][-A-z0-9\\^_]*)[<=>][<=>]*-*[0-9\\.][-0-9\\.\\^]*;; p[<=>\\.0-9]*$","\\1",x)
        # has delta
        hasDelta<-grep("[ Dd]elta",lastStat)>0
        if(isFALSE(hasDelta)){
        # remove till standard stat
        lastStat<-gsub(".* ([SstTzZpPQIHdbBF][DFEdfe]*[_01]*)$","\\1",lastStat)
        lastStat<-gsub(".* ([Cce][ht][ai]\\^*2*)$","\\1",lastStat)
        lastStat<-gsub(".* (omega\\^*2*)$","\\1",lastStat)
        lastStat<-gsub(".* (R\\^*2*)$","\\1",lastStat)
        # only standard stats
        lastStat<-grep("^[SstTzZpPQIHdbBF][DFEdfe]*$|^[Cce][ht][ai]\\^*2*$|^omega\\^*2*$|^R\\^*2*$",lastStat,value=TRUE)
        }
        if(isTRUE(hasDelta)){
          # remove till standard stat
          lastStat<-gsub(".*([dD]elta[ -][SstTzZpPQIHdbBF][DFEdfe]*)$","\\1",lastStat)
          lastStat<-gsub(".* ([dD]elta[ -][Cce][ht][ai]\\^*2*)$","\\1",lastStat)
          lastStat<-gsub(".* ([dD]elta[ -]omega\\^*2*)$","\\1",lastStat)
          lastStat<-gsub(".* ([dD]elta[ -]R\\^*2*)$","\\1",lastStat)
          # only standard stats
          lastStat<-grep("^[dD]elta[ -][SstTzZpPQIHdbBF][DFEdfe]*$|^[Cce][ht][ai]\\^*2*$|^omega\\^*2*$|^R\\^*2*$",lastStat,value=TRUE)
        }
        
        
        if(lastStat!=x){
          x<-gsub(paste0("( ",specialChars(lastStat),"[<=>][<=>]*-*[0-9\\.][0-9\\.]*), "),"\\1SPLITHERE",x)
          x<-unlist(strsplit(x,"SPLITHERE"))
          #if(length(x)>2){
            # add first cell content to front of new lines
            #  x[-1]<-paste0(gsub("^([^,]*), .*","\\1, ",x[1]),x[-1])
            # add stats in table num:
            #x[-1]<-paste0(gsub("^([^:]*): .*","\\1: ",x[1]),", ",x[-1])
          #}
        }
        return(x)
      }
      
      # apply function cell wise
      x<-unlist(lapply(x,fun1))
      x<-unlist(lapply(x,fun2))
  #    x<-unlist(lapply(x,fun3))
      x<-gsub(": ,",":",x)
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


correctDF<-function(x){
  fun<-function(x){
  # escape if has df=num
  if(length(grep(" [dD]\\.*[fF]\\.* *= *[1-9]|^[dD]\\.*[fF]\\.$",x))>0) return(x)
    
  # if has N=num and r/t-values
    if(length(grep("[ \\(][Nn] *= *[1-9][0-9]*|^[Nn] *= *[1-9][0-9]*",x))>0 &
       length(grep(" [rt]\\(*[1-9]*[0-9\\.]*\\)* *[<=>][<=>]* *(-*[\\.0-9][\\.0-9]*)|^[rt]\\(*[1-9]*[0-9\\.]*\\)* *[<=>][<=>]* *(-*[\\.0-9][\\.0-9]*)",x))>0){
    
     # replace df in [rt](num)= with N-2 behind in same row 
     isNbehind<-length(grep(" [rt]\\(*[1-9]*[0-9\\.]*\\)* *[<=>][<=>]* *(-*[0-9\\.][\\.0-9]*)|^[rt]\\(*[1-9]*[0-9\\.]*\\)* *[<=>][<=>]* *(-*[0-9\\.][\\.0-9]*)",
          unlist(strsplit(gsub("([ \\(][Nn] *= *[1-9][0-9]*)([^0-9])|^([Nn] *= *[1-9][0-9]*)([^0-9])","\\1\\3SPLITHERE\\2\\4",x),"SPLITHERE"))[1]))>0
     oneN<-length(grep("([ \\(][Nn] *= *[1-9][0-9]*)|^([Nn] *= *[1-9][0-9]*)",unlist(strsplit(gsub("([ \\(][Nn] *= *[1-9][0-9]*)([^0-9])|^([Nn] *= *[1-9][0-9]*)([^0-9])","\\1\\3SPLITHERE\\2\\4",x),"SPLITHERE"))))==1

     if(isNbehind){
       if(!oneN) x<-unlist(strsplit(gsub("([ \\(][Nn] *= *[1-9][0-9]*)([^0-9])","\\1SPLITHERE\\2",x),"SPLITHERE"))
       for(i in 1:length(x)){
         N<-suppressWarnings(as.numeric(gsub(".*[ \\(][Nn] *= *([1-9][0-9]*).*","\\1",x[i])))
         if(!is.na(N)) x[i]<-gsub("( [rt])\\(*[1-9]*[0-9\\.]*\\)* *([<=>][<=>]* *-*[\\.0-9])|^([rt])\\(*[1-9]*[0-9\\.]*\\)* *([<=>][<=>]* *-*[\\.0-9])",
                                  paste0("\\1\\3(",N-2,")\\2\\4"),x[i])
     }
       x<-paste0(x,collapse="")
      }
     else{
        # replace df in [rt](num)= with N-2, when N=num is in front in same row 
        # if N= exists twice split
        if(!oneN) x<-unlist(strsplit(gsub("( [rt]\\(*[1-9]*[0-9\\.]*\\)* *[<=>][<=>]* *-*[\\.0-9][\\.0-9]*)","\\1SPLITHERE",x),"SPLITHERE"))
        for(i in 1:length(x)){
        N<-suppressWarnings(as.numeric(gsub(".*[ \\(][Nn] *= *([1-9][0-9]*).*|^[Nn]=([1-9][0-9]*).*","\\1\\2",x[i])))
        if(!is.na(N)) x[i]<-gsub("( [rt])\\(*[1-9]*[0-9\\.]*\\)* *([<=>][<=>]* *-*[\\.0-9])|^([rt])\\(*[1-9]*[0-9\\.]*\\)* *([<=>][<=>]* *-*[\\.0-9])",
                                 paste0("\\1\\3(",N-2,")\\2\\4"),x[i])
        }
        x<-paste0(x,collapse="")
        } 
     }
    return(x)
    }
      
  x<-unlist(lapply(x,fun))
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
  if(nrow(x)<2) return(x)
  if(ncol(x)<2) return(x)
  
  # remove brackets in column with only brackets around anything
  i<-grep("^\\(.*\\)$",x[1,])
  if(length(i>0)) x[,i]<-gsub("^\\((.*)\\)","\\1",x[,i])   
  
  # extract and insert number in round brackets as column
  ind<-which(
    colSums(matrix(grepl("\\([<=> ]*[-0-9\\.][-,;0-9\\. ]*\\)", (x[-1,])),ncol=ncol(x)))>1 &
      grepl("\\([-0-9\\.A-z][-,;0-9\\. A-z]*\\)", as.vector(x[1,])))
    if(length(ind)>0){
    nCols<-ncol(x)
    ind<-which((colSums(matrix(unlist(
      grepl("\\([<=>]*[-0-9\\.][-,;0-9\\. ]*\\)",as.vector(x[-1,]))),ncol=nCols,byrow=FALSE))>0) 
      &
      grepl("\\([-A-z].*\\)|\\(.*[A-z]\\)|\\(.*%\\)",x[1,])
      & # no citation in header
      !grepl("\\([A-z].* et al\\..*\\)",x[1,])
        )
    
    # add columns with content in bracket and remove content within bracket 
    Nadded<-0
    if(length(ind)>0) for(j in ind){
      noBracket<-gsub("^ | $","",gsub(" \\([<=> ]*[-0-9\\.A][-,;0-9\\. A-z]*\\)","",x[,j+Nadded]))
      noBracket[1]<-gsub("  *"," ",gsub(" *\\(([^\\)]*)\\)","",noBracket[1]))
      Bracket<-gsub("  *"," ",gsub(".*\\(([<=> ]*[-0-9\\.][-,;0-9\\. A-z]*)\\).*","\\1",x[,j+Nadded]))
      # set cells that don't have bracket to ""
      k<-grep("\\(([<=> ]*[-0-9\\.][-,;0-9\\. A-z]*)\\)",x[,j+Nadded],invert=TRUE)
      Bracket[k]<-""
      # add code in brackets to first cell
      Bracket[1]<-gsub("  *"," ",gsub(".*\\(([^\\)]*)\\).*","\\1",x[1,j+Nadded]))

      # and remove from all other entries
      noBracket<-gsub(paste(paste0(" *\\(",specialChars(Bracket)[nchar(Bracket)>0],"\\)"),collapse="|"),"",noBracket)
      Bracket[1]<-gsub("\\\\","",Bracket[1])
      
      # add column
      if(j==1) x<-cbind(noBracket,Bracket,x[,(j+1):ncol(x)])
      if(j>1&(j+Nadded)<ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket,x[,(j+1+Nadded):ncol(x)])
      if(j>1&(j+Nadded)==ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket)
      Nadded<-Nadded+1
    }
    if(nrow(x)>2) x<-x[,colSums(x[-1,]=="")!=(nrow(x)-1)]
    if(nrow(x)==2) x<-x[,(x[-1,]=="")!=(nrow(x)-1)]
    }
  
  # extract and insert number in squared brackets as column
  ind<-which(
    colSums(matrix(grepl("\\[[<=> ]*[-0-9\\.][-,;0-9\\. ]*\\]", (x[-1,])),ncol=ncol(x)))>1 &
      grepl("\\[[-0-9\\.A-z][-,;0-9\\. A-z]*\\]", as.vector(x[1,])))
  if(length(ind)>0){
    nCols<-ncol(x)
    ind<-which(colSums(matrix(unlist(
      grepl("\\[[<=> ]*[-0-9\\.][-,;0-9\\. ]*\\]",as.vector(x[-1,]))),ncol=nCols,byrow=FALSE))>0 
      &
        grepl("\\[[A-z].*\\]|\\[.*[A-z]\\]|\\[.*%\\]",x[1,])  
    )
    Nadded<-0
    
    if(length(ind)>0) for(j in ind){
      noBracket<-gsub("^ | $","",gsub(" \\[[<=> ]*[-0-9\\.A-z][-,;0-9\\. A-z]*\\]","",x[,j+Nadded]))
      noBracket[1]<-gsub("  *"," ",gsub(" *\\[([^\\]]*)\\]","",noBracket[1]))
      Bracket<-gsub("  *"," ",gsub(".*\\[([<=> ]*[-0-9\\.][-,;0-9\\. A-z]*)\\].*","\\1",x[,j+Nadded]))
      # set cells that don't have bracket to ""
      k<-grep("\\[([<=> ]*[-0-9\\.][-,;0-9\\. A-z]*)\\]",x[,j+Nadded],invert=TRUE)
      Bracket[k]<-""
      # add code in brackets to first cell
      Bracket[1]<-gsub("  *"," ",gsub(".*\\[([^]]*)\\]","\\1",x[1,j+Nadded]))
      # add empty column
      if(j==1) x<-cbind(noBracket,Bracket,x[,(j+1):ncol(x)])
      if(j>1&(j+Nadded)<ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket,x[,(j+1+Nadded):ncol(x)])
      if(j>1&(j+Nadded)==ncol(x)) x<-cbind(x[,1:(j+Nadded-1)],noBracket,Bracket)
      Nadded<-Nadded+1
    }
    # remove empty colums
    if(nrow(x)>2) x<-x[,colSums(x[-1,]=="")!=(nrow(x)-1)]
    if(nrow(x)==2) x<-x[,(x[-1,]=="")!=(nrow(x)-1)]
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
  
  # take a copy for later activation of warning message
  y<-x
  # remove text behind "F-"
  x<-gsub(" (F) *[-_] *[A-z]*([<=>])"," \\1\\2",x)
  
  # add df2 in lines with F-values
  if(sum(grepl(" F[<=>]",x)&grepl(" [dD]\\.*[fF]\\.*=",x))>0 & 
     sum(!grepl(" F[<=>]",x)&grepl(" [dD]\\.*[Ff]\\.*=",x))>0){
    
    # but no "total"
    i<-which(grepl(" F[<=>]",x)&!grepl(" [Tt]otal[^a-z]|^[Tt]otal[^a-z]",x))
    j<-which(grepl(" [dD]\\.*[Ff]\\.*=",x)&!grepl(" F[<=>]",x)&!grepl("[Tt]otal",x))
    if(length(j)>0&length(grep(" [dD]\\.*[Ff]\\.*=",x[j]))>0){
      x[i]<-gsub(" [dD]\\.*[Ff]\\.*="," df1=",x[i])
      x[j]<-gsub(" [dD]\\.*[Ff]\\.*="," df2=",x[j])
      
      ## extract and add sum of unique df2 if i is a single sequence of increasing numbers or only one unique df2
      if(is.single.sequence(i)|length(unique(gsub(".*df2=([0-9][0-9\\.]*).*","\\1",x[j])))==1){
        df2<-sum(as.numeric(unique(gsub(".*df2=([0-9][0-9\\.]*).*","\\1",x[j]))),na.rm=TRUE)
        # add df2 to df1
        x[i]<-gsub("(df1=[0-9][0-9\\.]*)",paste0("\\1",", df2=", df2),x[i])
      }
      
      # extract and add df2 to df1, if i has multiple sequences  
      if(!is.single.sequence(i)){
        s<-sequenceSplit(i)
        df2<-(as.numeric((gsub(".*df2=([0-9][0-9\\.]*).*","\\1",x[j]))))
        
        # try to correct sequence if s and df2 are not of same length
        if(length(s)>length(df2)){
          s<-list()
          for(k in 1:length(j)){
          if(k==1) s[[k]]<-i[i<j[k]]
          if(k>1) s[[k]]<-i[i<j[k]&i>j[k-1]]
          }
          }
        
        # add df2 to df 1
        if(length(s)==length(df2)){
          for(k in 1:length(s))
          x[s[[k]]]<-gsub("(df1=[0-9][0-9\\.]*)",paste0("\\1",", df2=", df2[k]),x[s[[k]]])
         }

        }
      
      }
    # warning
    if(sum(x!=y)>1) warning("Special handling for degrees of freedom in ANOVA tables was performed. This may result in erroneous df2-values, which where imputed behind df1-values in lines with F-values in order to check the reported p-values.",call. = FALSE)
  }
  y
  # in lines with F-values
  i<-grep(" F[<=>]",x)
  if(length(i)>0){
   # convert df1 and df2 in brackets to text
   x[i]<-gsub(" [dD]\\.*[Ff]\\.*: \\(([0-9][0-9\\.]*)[,;] ([0-9][0-9\\.]*)\\)"," df1=\\1, df2=\\2",x[i])
   # unify df1 and df2 in text
#   x[i]<-gsub(",[^,]*[Dd][Ff][=: ]*([0-9][0-9\\.]*),[^,]*[Dd][Ff]=([0-9][0-9\\.]*)",", df1=\\1, df2=\\2",x[i])
  }

  # unify df1 and df2 in text
  x<-gsub("(:[^:]*)[dD]\\.*[Ff]\\.*[=: ]*([0-9][0-9\\.]*)[,;] *([0-9][0-9\\.]*)","\\1df1=\\2, df2=\\3",x)
  x<-gsub("(,[^,]*)[dD]\\.*[Ff]\\.*[=: ]*([0-9][0-9\\.]*)[,;] *([0-9][0-9\\.]*)","\\1df1=\\2, df2=\\3",x)

  # remove df1 and df2 if both are the same
  df1<-gsub(".*df1=([0-9][0-9\\.]*).*","\\1",x)
  df2<-gsub(".*df2=([0-9][0-9\\.]*).*","\\1",x)
  i<-which(df1==df2)
  x[i]<-gsub("df1=([0-9][0-9\\.]*),.*df2=[0-9][0-9\\.]*","\\df=",x[i])
  return(x)
}

#######################################################
# function to detect simple sequences
is.single.sequence<-function(x){
  if(length(x)<2) return(TRUE)
  temp<-NULL
  for(j in 1:(length(x)-1)) temp[j]<-x[j]+1==x[j+1]
  return(ifelse(sum(!temp)==0,TRUE,FALSE))
}



## function to split to list of sequences
sequenceSplit<-function(x){
  if(is.single.sequence(x)) return(list(x))
  temp<-TRUE
  for(j in 2:(length(x)))
    temp[j]<-(x[j-1])==(x[j]-1)
  w<-which(!temp)
  out<-list()
  for(j in 1:(length(w)+1)){
    if(j==1) out[[j]]<-x[1]:x[(w[j]-1)]
    if(j>1&j<(length(w)+1)) out[[j]]<-x[w[j-1]]:x[(w[j]-1)]
    if(j==(length(w)+1)) out[[j]]<-x[w[j-1]]:x[length(x)]
  }
  return(out)
}

## collapse all cells with pasted statistics per col 
hasSequence<-function(x){
  if(length(x)==0) return(FALSE)
  if(length(x)==1) return(TRUE)
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

# get first sequence from grep result
firstSeq<-function(x){
  if(length(x)<=1) return(x)
  seq<-NULL
  for(i in 1:(length(x)-1)){
    if((x[i]+1)==x[1+i]) seq<-unique(c(seq,x[i],x[i+1]))
    if((x[i]+1)!=x[1+i]&length(seq)>1) break()
  }
  return(seq)
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
  


## paste model and standard statistics in first or second col to numeric/result fields behind
modelStatsHandler<-function(x){
  if(!is.matrix(x)) return(x)
  if(nrow(x)<3) return(x)
  if(ncol(x)<3) return(x)
  
  ## paste first cell to all cells with numbers in rows starting with search term 
  fun<-function(x){
    if(!is.matrix(x)) return(x)
    if(nrow(x)<3) return(x)
    if(ncol(x)<3) return(x)
  
  ## for standard results
  ind1<-grep("[Nn]umber of observation|eta\\^2|omega\\^2|chi\\^2|^[tZprRbBd]$|^[RO]R$|^[Ss]\\.* *[EeDd]\\.* *$|^[SsDd]\\.*[FfEeDd]\\.*$|^.\\^2$|^AIC|^BIC|^[cC]hi\\^2|^[dD]egrees* of freedom|^[Dd]\\.*[Ff]\\.*$",
             gsub(".*: |[- ]val*u*e*s*|[_-][A-z0-9\\*+-]*|[ *_]\\(.*\\)","",x[,1]))
  ## for model statistics
  ind2<-NULL
  if(length(grep("[Cc]onstant|[Ii]ntercept|[Mm]odel",x))>0|(length(grep("[Mm]odel",class(x)))>0)) 
  ind2<-which(grepl("^R\\^*2$|[- ]R\\^*2$|^R2 |R\\^2|^R[- ][Ss]q|^[Ll]ikelihood",
             x[,1])  
               # and that have less than 5 characters in first column
               #nchar(x[,1])<5
               # and that don't have results in that line
               #rowSums(matrix(grepl("[A-z\\)][\\^2]* *[<=>][<=>]* *[\\.0-9]",x),ncol=ncol(x),byrow = TRUE))==0
             )
  
  # if first hit is in second row -> reset ind2 to NULL
  if(isTRUE(ind2==2)) ind2<-NULL
  
  ## take all the last rows if first hit is in lower half of table
  if(length(ind2)>0) 
    if(min(ind2)>(nrow(x)/2)) ind2<-min(ind2):nrow(x)
      
  # model index if second cell (constant/first variable) only exists once
  #if(length(ind2)>0&!is.element(x[2,1],x[c(-1,-2),1])) ind2<-ind2[1]:nrow(x)
  # combined index
  ind<-sort(unique(c(ind1,ind2)))
  # for all stats
  #ind<-ind1
  if(length(ind)==0) return(x)
  
  # check if has only unique stats and extract first duplicated stat
  uniques<-ifelse(sum(duplicated(x[,1][ind]))==0,TRUE,FALSE)
  dups<-duplicated(x[,1][ind])
  dupSplit<-NULL
  if(sum(dups>0)&!uniques) dupSplit<-x[,1][ind][dups][1]
  pos<-grep(paste0("^",specialChars(dupSplit),"$"),x[,1])
  k<-sum(x[,1]==dupSplit)
  
  # paste paste first cell to numeric cells in same row
  for(h in ind){
      x[h,grep("^-*[\\.0-9]",x[h,])]<-paste0(x[h,1],"=",x[h,grep("^-*[\\.0-9]",x[h,])])
      x[h,grep("^[<=>][<=>]*-*[\\.0-9]",x[h,])]<-paste0(x[h,1],"",x[h,grep("^[<=>][<=>]*-*[\\.0-9]",x[h,])])
    }
  # change first cell content
  x[ind,1]<-"TEMP_TEXT"
  
  ## collapse results in multiple rows
  # - if contains duplicated stats
  if(!uniques){
       indexes<-list()
       # if duplicated stat is first stat
       if(pos[1]==min(ind)){
         for(i in 1:k){
           if(i<k) indexes[[i]]<-pos[i]:(pos[i+1]-1)
           if(i==k) indexes[[i]]<-pos[i]:max(ind)
           indexes[[i]]<-indexes[[i]][is.element(indexes[[i]],ind)]
           }
         }
       
       # if duplicated stat is last stat but not first
       if(length(indexes)==0&(pos[k]==max(ind))&!(pos[1]==min(ind))){
         for(i in 1:k){
           if(i==1) indexes[[i]]<-min(ind):pos[i]
           if(i>1) indexes[[i]]<-(pos[i-1]+1):pos[i]
           indexes[[i]]<-indexes[[i]][is.element(indexes[[i]],ind)]
         }
       }
       
       if(length(indexes)>0)
       for(l in 1:length(indexes)){
         i<-indexes[[l]]
         for(j in 2:ncol(x)) x[i[1],j]<-paste0(x[i,j],collapse=", ")
           x[i[-1],]<-"removeLine"
        }
      }
        
  # - if no duplicated stat is detected
  if(uniques){
    while(hasSequence(ind)){
      i<-firstSeq(ind)
      for(j in 2:ncol(x)) x[i[1],j]<-paste0(x[i,j],collapse=", ")
      x[i[-1],]<-"removeLine"
      ind<-ind[!is.element(ind,i)]
    }
  }
  
  # clean up 
  x<-x[grep("removeLine",x[,1],invert=TRUE),]
  x<-gsub("(, )(, )*|^, |, $","\\1",x)
  x<-gsub("^[, ]*$","",x)
  
  if(!is.matrix(x)) x<-matrix(x,nrow=1)
  
  return(x)
  }
  
  
  # apply function to table     
  out<-fun(x)
  
  # check if is also senseful for second column if nothing was detected in first col
  out2<-fun(x[,-1])
  if(nrow(out)==nrow(x)&nrow(out2)!=nrow(x)){
    # and only if unique labels in first column match to length of result 
    if(sum(unique(x[-1,1])!="")==(nrow(out2)-1)){
    out2[-1,1]<-unique(x[-1,1])[unique(x[-1,1])!=""]
    # paste cell 1, 1 to cells in first column
    if(nchar(x[1,1])>0) out2[-1,1]<-paste(x[1,1],", ",out2[-1,1])
    out2<-gsub("^[, ]*$","",out2)
    # return result
    return(out2)
    }
    }
  
  # else 
  return(out)
  }


# handling of model statistics. Use with: unlist(lapply(dupSplit(stats),modelhandler))
modelHandler<-function(x){
  fun<-function(x){
    i<-grep(" R2|^R2|R\\^2|R[- ][Ss]q|[Rr]esidual|AIC|BIC|[Ii]nformation [Cr]iter|chi\\^2|degrees* of freedom|^df$|^DF$|^F$",x)
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
    stats[i]<-gsub(" *standardized beta[,:] | *beta[,:] | \u0392[,:] | \u03b2[,:] | \u00df[,:] | *SE[,:] | [tTzZFpPORb][R]*[,:] |^[tTzZFpPORb][R]*[,:] | *[8-9][0-9]\\%[ -]CI[,:]* *"," ",stats[i])
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
    n<-rep(n,times=ifelse(isTRUE(j),lapply(x[j],length),1))
    for(i in 1:length(j)){
      if(i==1){ 
        if(isFALSE(j[i])) y<-x[[i]]
        if(isTRUE(j[i])) y<-unlist(x[[i]],recursive=FALSE)
      }
      if(i>1){ 
        if(isFALSE(j[i])) y<-c(y,list(x[[i]]))
        if(isTRUE(j[i])) y<-c(y,x[[i]])
      }
    }
    x<-y
    #    names(x)<-n
  }
  return(x)
}



# Custom wrapper function to capture warnings
capture_warnings <- function(expr) {
  warnings_list <- list()
  value <- withCallingHandlers(
    expr, warning = function(w) {
      # Store warnings as character vectors
      warnings_list[[length(warnings_list) + 1]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    })
  ## output result as list
  return(list(value = value, warnings = warnings_list))
}

uniqueWarnings<-function(exp){
  y<-capture_warnings(exp)
  ## unique warnings
  if(length(y$warnings)>0){
    uWarn<-unique(unlist(y$warnings))
    # reorder
    i<-grep("Some textual result",uWarn)[1]
    j<-grep("Special handling for degrees",uWarn)[1]
    if(!is.na(i)>0&!is.na(j)>0){
      if(i>j){
        x<-uWarn[i]
        uWarn[i]<-uWarn[j]
        uWarn[j]<-x
      }}
    for(i in 1:length(uWarn))
          warning(gsub("^[- ]*","",uWarn[i]),call. = FALSE)
    }
  ## output result
  return(y$value)
}

