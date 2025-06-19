#' flattenList
#' flatten multi level list to simple list
#' @param x a list with listed elements
#' @returns single level list
#' @example 
#' a<-list(matrix(3:0,ncol=2),matrix(10:5,ncol=3))
#' a
#' b<-list(matrix(3:0,ncol=2),matrix(10:5,ncol=3))
#' b
#' c<-list(a,b)
#' c
#' x<-list(a,b,c)
#' x
#' lapply(x,flattenList)
#' lapply(x,flattenList)
#' @export 
 
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
