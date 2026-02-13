#' tableClass
#'
#' Classifies matrix content to either 'tabled results', 'correlation', 'matrix', 'text', 'vector', 'model with model statistics', or 'multi model with model statistics'. 
#' @param x A character matrix.
#' @param legend A text vector with the tables caption and/or footnote.
#' @returns A character object of length=1 with the table's class.
#' @export
#' 
tableClass<-function(x,legend=NULL){
  if(length(x)==0) return(NULL)
  if(!is.matrix(x)) return(NULL)
  ## standard output:
  class<-"tabled results"
  
  # remove inserted p-values
  x<-gsub(";; p[<=>].*","",x)
  
  # remove html tags
  x<-gsub("<su[bp]>[^<]*</su[bp]>","",x)
  x<-gsub("<[^>]*>[^<]*</[^>]*>","",x)
  x<-gsub("<[^>]*>[^<]*<[^>]*/>","",x)
  
  # remove minus at beginning
  x<-gsub("^-","",x)
  # remove hight anything
  x<-gsub("\\^.*","",x)
  
  # take a copy
  m<-x

  nCol<-ncol(m)
  if(is.null(nCol)|nCol==1|nrow(m)==1) return("vector")
  if(nrow(m)==1&ncol(m)==1) return("vector")
  
  # if has many cells (>20%) with many characters (>150), return text
  nCells<-nrow(m)*ncol(x)
  
  if(sum(unlist(lapply(m,nchar))>150,na.rm=TRUE)/nCells>.2) return("text")

  ## check if matrix is text matrix
  # is cell with character?
  characterCell<-
    matrix(gsub("</*[a-z][^>]*/*>|[[:punct:]]|[A-z][A-z0-9]* *[<=>][<=>]* *[-0-9\\.]*| *[-0-9\\.]*[0-9]\\^[A-z]*|[^a-zA-Z]|[1-2][0-9][0-9][0-9]","",as.vector(m)),ncol=nCol)!=""
  characterCell[is.na(characterCell)]<-FALSE
  # empty cells to TRUE
  characterCell[m==""]<-TRUE
  # cells with chinese/cyrillic characters to TRUE
  characterCell[grep("[\u4E00-\u9FFF]|[\u0400-\u04FF]",m)]<-TRUE
  
  # escape if more than 90% are character cells
  if(sum(characterCell,na.rm=TRUE)/length(characterCell)>.9){
    return("text")
  }
  
  # remove grouping lines
  m<-m[!rowSums(matrix(grepl("^$",m),ncol=ncol(m)))==(ncol(m)-1),]
  if(length(m)==0) return("tabled results")
  if(!is.matrix(m)) m<- as.matrix(m)
  m<-m[rowSums(m==m[,1])!=ncol(m),]
  if(length(m)==0) return("tabled results")
  if(!is.matrix(m)) m<- as.matrix(m)
  
  
  ###################################
  ## check if is correlation matrix?
  if(ncol(m)>2&nrow(m)>2){
  # remove p-values/stars behind numbers
  m<-gsub("([0-9])[,;]* p[<=>][<=>]*[\\.0-9][\\.0-9]*","\\1",m)
  m<-gsub("([0-9])\\^[A-z[:punct:]]*","\\1",m)
  m<-gsub("([0-9])[\\*+]*","\\1",m)
  # remove numbers in brackets behind numbers
  m<-gsub("([0-9])[,;]* \\([-\\.0-9][\\.0-9]*\\)","\\1",m)
  # remove everything behind numbers
  m<-gsub("([0-9])[,;]* .*","\\1",m)
  # remove n.s. from inner matrix
  m[-1,-1]<-gsub("^[Nn]\\.*[Ss]\\.*$","",m[-1,-1])
  
  # check 1: all numeric fields have values [-1; 1] and "Correlation" is in legend
  cors<-matrix(suppressWarnings(as.numeric(gsub("[^0-9\\.-]","",m[-1,-1]))),ncol=ncol(m)-1)
  empty<-matrix(gsub("[[:punct:]]","",m[-1,-1])=="",ncol=ncol(m)-1)
  
  # set rows/cols with mean/sd/alpha/... to NA
  cors[grep("^ICC|^[Mm]ed*i*an$|^AVE$|^M$|Cronbach|\u03b1|[aA]lpha|[Ss]tandard [Dd]eviation|^SDs*$|^[Ll]oadings*$",m[-1,1]),]<-NA
  cors[,grep("^ICC|^[Mm]ed*i*an$|^AVE$|^M$|Cronbach|\u03b1|[aA]lpha|[Ss]tandard [Dd]eviation|^SD|^[Ll]oadings*$$",m[1,-1])]<-NA
  empty[grep("^ICC|^[Mm]ed*i*an$|^AVE$|^M$|Cronbach|\u03b1|[aA]lpha|[Ss]tandard [Dd]eviation|^SD$|^[Ll]oadings*$",m[-1,1]),]<-TRUE
  empty[,grep("^ICC|^[Mm]ed*i*an$|^AVE$|^M$|Cronbach|\u03b1|[aA]lpha|[Ss]tandard [Dd]eviation|^SD$|^[Ll]oadings*$",m[1,-1])]<-TRUE
  
  # prepare removal matrix of correlations
  cors <- cors<=1&cors>=-1
  cors[is.na(cors)]<-FALSE
  # are all numbers [-1; 1]?
  t1<-cors|empty
  t1<-sum(t1)==length(t1)
  # has correlation in legend?
  t2<-length(grep("[Cc]orr*elation|[Aa]ssociation|[Re]elation",legend))>0
  # is there no p/d/beta/R^2 value in matrix?
  t3<-length(grep("[Ii]ntercept|^[PpdDBb]$| [PpdDBb]$|[Pp]-value|[0-9]%[-]*[Cc][oIi]|R\\^*2|eta\\^*2$",m))==0
  # check if matrix has at not more than 3 times as many rows as columns and vice versa
  t4<-TRUE #(nrow(m)/ncol(m))<3 & (nrow(m)/ncol(m))>.33
  # first column has no digit number starting with 0 
  t5<-length(grep("^0\\.[0-9]",m[,1]))==0
  # exclude cases with first column only numbers but no 1 nor 2
  t6<-TRUE
  if(length(grep("^[0-9][0-9\\.]*$|^$",m[-1,1]))==(nrow(m)-1))
    t6<-(length(grep("^1\\.*[^0-9]*$",m[-1,1]))>0|length(grep("^2\\.*[^0-9]*$",m[-1,1]))>0)
  
  if(t1&t2&t3&t4&t5&t6){
    class<-"correlation"
    return(class)
  }
  }
  
  ###################################
  # check 2 for correlation: Sequence of increasing number of NA
  if(ncol(m)>3&nrow(m)>3){
    # in lines that are correlations
  r<-rowSums(cors)>0
  cols<-colSums(cors)>0
  empty<-gsub("\\([0-9\\.-][0-9\\.\\[:punct::]]*\\)","",m[r,cols])==""
  
  # number of empty cells per row
  if(is.matrix(empty)){  
    s<-rowSums(empty)[-1]
  # only go on if there is empty cells
  if(sum(s,na.rm=TRUE)>0){   #&(sum(s>1)>length(s)*.5)){
    i<-which(s==min(s,na.rm=TRUE))[1]:which(s==max(s,na.rm=TRUE))[1]
    # minimum length of sequence: 3
    hasSeq<-length(grep("0 1 2|1 2 3|2 3 4|4 5 6",paste0(s[i],collapse=" ")))>0
    # are there no p/d/beta/R^2 value in matrix?
    t3<-length(grep("^[Ppd]DBb$| [PpdDBb]$[Pp]-value|[0-9]%[-]*[Cc][oIi]|R\\^*2$|R[- ]squa|eta\\^*2",m))==0
    if(!t3) hasSeq<-FALSE 
    #block<-empty%*%t(empty)
    ## must be better done!!!
    block<-((empty)%*%t(empty))%*%(empty)
    nums<-suppressWarnings(as.numeric(gsub("[^0-9\\.-]","",m[r,cols][block>0])))
    nums<-nums[!is.na(nums)]
    hasCor<-sum(nums>=-1&nums<=1)>.9*length(nums)
    # is there no p/d/beta/R^2 value in matrix?
    t3<-length(grep("[Ii]ntercept|^[PpdDBb]$| [PpdDBb]$|[Pp]-value|[0-9]%[-]*[Cc][oIi]|R\\^*2|eta\\^*2$",m))==0
    
    if(hasSeq&hasCor&t3){
      class<-"correlation"
      return(class)
      }
  }
  }
  }
  

  # check 3 for correlation: has increasing numbers in first row and col
  if(ncol(m)>3&nrow(m)>3){
    row1<-m[1,]
    col1<-m[,1]
  t1<-
  # 1, 2, 3 in frist row
    grep("^1|^\\(1",gsub("[^:]*: ","",row1))[1]==grep("^2|^\\(2",gsub("[^:]*: ","",row1))[1]-1&
  grep("^1|^\\(1",gsub("[^:]*: ","",row1))[1]==grep("^3|^\\(3",gsub("[^:]*: ","",row1))[1]-2 &
  # 1, 2, 3 in frist col
    grep("^1|^\\(1",gsub("[^:]*: ","",col1))[1]==grep("^2|^\\(2",gsub("[^:]*: ","",col1))[1]-1&
  grep("^1|^\\(1",gsub("[^:]*: ","",col1))[1]==grep("^3|^\\(3",gsub("[^:]*: ","",col1))[1]-2|
  # 2, 3, 4 in frist row
    grep("^2|^\\(2",gsub("[^:]*: ","",row1))[1]==grep("^3|^\\(3",gsub("[^:]*: ","",row1))[1]-1&
  grep("^2|^\\(2",gsub("[^:]*: ","",row1))[1]==grep("^4|^\\(4",gsub("[^:]*: ","",row1))[1]-2 &
  # 2, 3, 4 in frist col
    grep("^2|^\\(2",gsub("[^:]*: ","",col1))[1]==grep("^3|^\\(3",gsub("[^:]*: ","",col1))[1]-1&
  grep("^2|^\\(2",gsub("[^:]*: ","",col1))[1]==grep("^14|^\\(14",gsub("[^:]*: ","",col1))[1]-2

  if(is.na(t1)) t1<-FALSE
  
  # has correlation in legend?
  t2<-length(grep("[Cc]orr*elation|[Aa]ssociation|[Re]elation",legend))>0
  # is there no p/d/beta/R^2 value in matrix?
  t3<-length(grep("[Ii]ntercept|^[PpdDBb]$| [PpdDBb]$[Pp]-value|[0-9]%[-]*[Cc][oIi]|R\\^*2$|eta\\^*2",m))==0
  
  if(t1&t2&t3){
    class<-"correlation"
    return(class)
  }
  }
  
    
  # detect matrix content
  # if 1st row and col contain more than 2 cells with the same name
  if(class!="correlation"&
     sum(is.element(unique(m[1,]),unique(m[,1])))>2){
    class<-"matrix"
  }
  
  # model and multi model
  if(nrow(m)>2 &
     # has R^2|F|etc
     (length(grep("^R\\^*2|R\\^*2$|[^A-z]R\\^*2|R[- ][Ss]q|^F$|^AIC|^BIC|Aikaike|[Ii]nformation [Cr]iter",m[-1:-2,1]))>0 
      | 
      length(grep("^R\\^*2|R\\^*2$|[^A-z]R\\^*2|R[- ][Ss]q|^F$|^AIC|^BIC|Aikaike|[Ii]nformation [Cr]iter",m[1,-1:-2]))>0 
      )
     & 
    # has Regression/Model in legend or header?
    (length(grep("[Rr]egression|[Mm]odel",legend))>0|length(grep("[Rr]egression|[Mm]odel",m))>0)
  ){
    class<-"model with model statistics"
  }
  
  if(class=="model with model statistics"){
    ind<-grep("^R\\^*2|R\\^*2$|[^A-z]R\\^*2|R[- ][Ss]q|^F$|^AIC|^BIC|Aikaike|[Ii]nformation [Cr]iter",m[,1])
    if( 
     # has more than 1 R^2
     length(grep("[\\.0]*[0-9]",m[ind[1],]))>1|
     length(grep("[\\.0]*[0-9]",m[ind[2],]))>1
     ){
    class<-"multi model with model statistics"
  }}
  
  return(class)
}
