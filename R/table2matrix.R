#' table2matrix 
#'
#' Extracts tables from HTML, HML, XML, DOCX, PDF files, or plain HTML code to a list of character matrices.
#' @param x A file path to a DOCX, PDF, or HTML encoded file, or text with HTML code.
#' @param unifyMatrix Logical. If TRUE, matrix cells are unified for better post-processing (see '?unifyMatrixContent').
#' @param letter.convert Logical. If TRUE, html and hexadecimal encoded letters will be unified and converted to Unicode with html2unicode() and JATSdecoder::letter.convert().
#' @param greek2text Logical. If TRUE and 'letter.convert=TRUE', converts and unifies various Greek letters to a text-based form (e.g.: 'alpha', 'beta'). 
#' @param correctComma Logical. If TRUE, commas used as decimal are converted to dots, big mark commas are removed. 
#' @param replicate Logical. If TRUE, the content of cells with row/col span > 1 is replicated in all connected cells; if FALSE, the value will only be placed in the first of the connected cells.
#' @param repNums Logical. If TRUE, cells with numbers that have row/col span > 1 are replicated in every connected cell.
#' @param rm.empty.row.col Logical. If TRUE, empty rows/columns are removed from output.
#' @param rm.html Logical. If TRUE, all HTML tags are removed, except <sub> and <sup>, and </break> is converted to space.
#' @param collapseHeader Logical. If TRUE, header cells are collapsed for each column if the header has 2 or more lines.
#' @param header2colnames Logical. If TRUE and 'collapseHeader=TRUE', the first table row is used for column names and removed from the table.
#' @examples 
#' ## - Download example DOCX file
#' d<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx'
#' download.file(d,paste0(tempdir(),"/","tableExamples.docx"))
#' 
#' # Extract tables from example file as matrices
#' table2matrix(paste0(tempdir(),"/","tableExamples.docx"))
#' 
#' ## - Download example HTML file
#' h<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.html'
#' download.file(h,paste0(tempdir(),"/","tableExamples.html"))
#' 
#' # Extract tables from example file as matrices
#' table2matrix(paste0(tempdir(),"/","tableExamples.html"),rm.html=TRUE)
#' 
#' ## - Download example PDF file
#' p<-'https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.pdf'
#' download.file(p,paste0(tempdir(),"/","tableExamples.pdf"))
#'
#' # Extract tables from example file as matrices
#' \donttest{
#' table2matrix(paste0(tempdir(),"/","tableExamples.pdf"))
#' 
#' # Note: The extraction of tables within PDF documents with tabulapdf::extract_tables()  
#' # does not work properly here. 
#' # Also, the table captions and footnotes cannot be used for decoding (e.g., p-values). 
#'
#' tabulapdf::extract_tables(paste0(tempdir(),"/","tableExamples.pdf"))
#' }
#' 
#' ## Another example with a website that contains simple and nested HTML-tables
#' 
#' # download file
#' x<-readLines("https://en.wikipedia.org/wiki/R_(programming_language)",warn=FALSE)
#' 
#' # apply function
#' table2matrix(x,rm.html=TRUE,unifyMatrix=TRUE)
#' @importFrom tabulapdf extract_tables
#' @importFrom JATSdecoder letter.convert
#' @importFrom JATSdecoder strsplit2
#' @return List with detected tables as character matrices.
#' @export

# @param rm.duplicated Logical. If TRUE duplicated rows are removed from output.
##############################################
table2matrix<-function(x,unifyMatrix=FALSE,
                       letter.convert=TRUE,
                       greek2text=FALSE,
                       correctComma=FALSE,
                       replicate=FALSE, 
                       repNums=FALSE,
                       rm.html=FALSE, 
                       rm.empty.row.col=FALSE,
                       collapseHeader=TRUE,
                       header2colnames=FALSE
){
  legendText<-NULL
  # escapes
  if(length(x)==0) return(NULL)
  if(is.list(x)|is.matrix(x)) stop("Input must be a file path or plain HTML code.")
  
  # escapes for bad file formats
  if(length(grep("^<table",x))==0 & length(x)==1)
     if(file.exists(x) &  !is.element(toupper(gsub(".*\\.([A-z][A-z]*)$","\\1",x)),c("HTML","HML","XML","NXML","CERMXML","DOCX","PDF")))
    stop("File input format must be of either DOCX, PDF, HTML, HML, NXML, or XML format.")
  
  # get file type
  type<-tolower(gsub(".*\\.([A-z][A-z]*)$","\\1",x[1]))
  isFile<-file.exists(x[1])
  
  if(nchar(type)>2&nchar(type)<8) 
    if(!file.exists(x)) stop("Input file does not exist.")
       
  # take a copy of file path for guessing the table legend text in html files
  if(is.element(type,c("html","hlm","xml","nxml","cermxml"))){
    file<-x[1]
  }
  
  # get matrix from DOCX or PDF
  if(is.element(type,c("docx","pdf"))){
  # docx 
  if(is.element(type,c("docx"))){
    m<-uniqueWarnings(docx2matrix(x,replicate=replicate,
                                  unifyMatrix=FALSE,correctComma=FALSE))
    
    if(isTRUE(unifyMatrix)|isTRUE(correctComma)) m<-uniqueWarnings(unifyMatrixContent(m,letter.convert=FALSE,
                                                  greek2text=FALSE,
                                                  correctComma=correctComma,text2num=FALSE))
    # convert letters in matrix
    if(isTRUE(greek2text)|isTRUE(letter.convert)) m<-lapply(m,JATSdecoder::letter.convert,greek2text=greek2text)
    # get captions and footer
    legendText<-guessCaptionFootnote(x)
    if(length(legendText)==2){
      caption<-gsub("([^[:punct:]])$","\\1.",legendText$caption)
      footer<-legendText$footer
      legendText<-paste(gsub("([^[:punct:]])$","\\1.",legendText$caption),legendText$footer)
      legendText<-JATSdecoder::letter.convert(legendText,greek2text=greek2text)
    }
    
    # add as attribute
    if(length(m)==length(caption)){
      for(i in 1:length(m)){
        attributes(m[[i]])$caption <- caption[i]
        attributes(m[[i]])$footer <- footer[i]
      }
    }
    
  }
  # pdf
  if(type=="pdf"){
    x<-tabulapdf::extract_tables(x,output="matrix")
    m<-lapply(x,as.matrix)
    if(isTRUE(unifyMatrix)|isTRUE(correctComma)) m<-uniqueWarnings(unifyMatrixContent(m,letter.convert=letter.convert,
                                                  greek2text=FALSE,
                                                  correctComma=correctComma,text2num=FALSE))
    if(isTRUE(greek2text)) m<-lapply(m,JATSdecoder::letter.convert,greek2text=greek2text)
  }
    
  # apply options
  # remove html
    if(isTRUE(rm.html)){
      # convert </break> to space, <sub> to _, <sup> to ^ 
      m<-lapply(m,function(x) gsub("</*break/*>"," ",x))
      m<-lapply(m,function(x) gsub(" *<sub> *","_",x))
      m<-lapply(m,function(x) gsub(" *<sup> *","^",x))
      m<-lapply(m,function(x) gsub("</*sub/*>|</*sup/*>"," ",x))
      # convert bold and italic numbers to number^bold/number^italic
      m<-lapply(m,function(x) gsub("<(/*)b(/*)>","<\\1\\2bold>",x))
      m<-lapply(m,function(x) gsub("<(/*)i(/*)>","<\\1\\2italic>",x))
      m<-lapply(m,function(x) gsub("<(/*)strong(/*)>","<\\1\\2bold>",x))
      m<-lapply(m,function(x) gsub("</bold>([\\.,; ]*)<bold>","\\1",x))
      m<-lapply(m,function(x) gsub("</it*a*l*i*c*>([\\.,; ]*)<it*a*l*i*c*>","\\1",x))
      m<-lapply(m,function(x) gsub("([0-9])</bold>","\\1^bold",gsub("[0-9]</italic>","\\1^italic",x)))
      m<-lapply(m,function(x) gsub("([0-9])</b>","\\1^bold",gsub("[0-9]</i>","\\1^italic",x)))
      # remove styles
      m<-lapply(m,function(x) gsub(">[\\.@][a-z][^\\{]*\\{[a-z].*\\}\\}",">",x))
      m<-lapply(m,function(x) gsub(">[\\.@][a-z][^\\{]*\\{[a-z][^\\{]*\\}",">",x))
      # remove all other html-tags
      m<-lapply(m,function(x) gsub("</*[a-z][^>]*/*>|</*[a-z]/*>","",x))
      m<-lapply(m,function(x) gsub("</*inline[^>]*/*>|</*inline[^>]*/*>","",x))
      # space reduction
      m<-lapply(m,function(x) gsub("  *"," ",x))
      m<-lapply(m,function(x) gsub("^ $","",x))
      # remove styles
      m<-lapply(m,function(x) gsub("[\\.@][a-z][^\\{]*\\{[a-z].*\\}\\}","",x))
      m<-lapply(m,function(x) gsub("[\\.@][a-z][^\\{]*\\{[a-z][^\\{]*\\}","",x))
      
    }
    
    if(isTRUE(header2colnames)) {
     m<-lapply(m,function(x){
       if(length(colnames(x)==0)){
         colnames(x)<-x[1,]
         x<-x[-1,]
         }
       return(x)
     })
    }
    if(isFALSE(header2colnames)) {
      m<-lapply(m,function(x){
        if(length(colnames(x)>0)){
          x<-rbind(colnames(x),x)
        }
        return(x)
      })
    }
    if(isTRUE(collapseHeader)) warnings("Header collapsing is only possible for HTML coded tables.")
    
    # add table class as attribute
    if(length(m)>0) 
      for(i in 1:length(m)){
        attributes(m[[i]])$class<-tableClass(m[[i]],legend = legendText[i])
      }
    # copy for output
    out<-m
    
  }else{
    
  # for all other inputs
  # run pre checks or readLines(x) if x is file

    # check if x is of length 0
    if(length(x)==0) return(NULL)
    # check if x is NA
    if(is.na(x)[1]) return(NULL)
    # check if x is character
    stopifnot('"x" must be a HTML encoded file or text' = is.character(x[1]))
    # readLines if x is file
    if(file.exists(x[1])){
      # check if x is of length=1
      stopifnot('File input must be of length=1.'=length(x)==1)
      # read file content
      x<-readLines(x,warn=FALSE,encoding="UTF-8")
    }
    
  # extract HTML tables if is not already a vector with <tables>
  if(length(grep("<table",substr(x[1],1,7)))==0) 
    x<-get.HTML.tables(x)
    # remove newline sign
    x<-gsub("\\n"," ",x)
    # split multiple tables inside of one <table-wrap>-tag
    x<-multiTable(x)
    # apply function singleTable2matrix
    out<-list() 
    if(length(x)==1) out[[1]]<-singleTable2matrix(x,letter.convert=letter.convert,
                                                greek2text=greek2text,replicate=replicate,
                                                repNums=repNums,rm.empty.row.col=rm.empty.row.col,
                                                rm.html=rm.html,collapseHeader=collapseHeader,
                                                header2colnames=header2colnames)
    if(length(x)>1) out<-lapply(x,singleTable2matrix,letter.convert=letter.convert,
                              replicate=replicate,repNums=repNums,
                              greek2text=greek2text,rm.empty.row.col=rm.empty.row.col,
                              rm.html=rm.html,collapseHeader=collapseHeader,
                              header2colnames=header2colnames)
    
    # escape if no matrix content is extracted
    if(length(unlist(out))==0) return(NULL)
  
    # classify table with legend text
    caption<-lapply(x,get.caption)
    footer<-lapply(x,get.footer)
    caption<-unlist(lapply(caption,paste,collapse=" "))
    footer<-unlist(lapply(footer,paste,collapse=" "))
    
    legendText<-gsub("^ *| *$","",paste(caption,footer))
  
    # if something was found add as attribute
    if(sum(nchar(legendText),na.rm=TRUE)>0){
    if(length(out)==length(caption))
      for(i in 1:length(out)){
        attributes(out[[i]])$caption <- caption[i]
        attributes(out[[i]])$footer <- footer[i]
      }
    }
    
    # if nothing was found -> guess legend text
    if(sum(nchar(legendText),na.rm=TRUE)==0 & isFile){
      legendText<-guessCaptionFootnote(file)
      if(length(legendText)==2){
        caption<-JATSdecoder::letter.convert(gsub("([^[:punct:]])$","\\1.",legendText$caption),greek2text=greek2text)
        footer<-JATSdecoder::letter.convert(legendText$footer,greek2text=greek2text)
      }
      # add as attribute
      if(length(out)==length(caption))
        for(i in 1:length(out)){
          attributes(out[[i]])$caption <- caption[i]
          attributes(out[[i]])$footer <- footer[i]
          }
      # and re-classify tables
      if(length(out)==length(legendText) & isFile)
        for(i in 1:length(out))
          attributes(out[[i]])$class <- tableClass(out[[i]],legend=legendText[i])
    
    }
  
    # remove empty lists
    if(length(out)>0){
      out<-out[which(unlist(lapply(out,length))!=0)]
      if(length(out)>0&!is.list(out)){
        temp<-out
        out<-list()
        out[[1]]<-temp
    }
  }
  
  }# end of all other file formats

  # escape if no content is left
  if(length(unlist(out))==0) return(NULL)
 
  # apply options
  uniqueWarnings({
  if(isTRUE(unifyMatrix)) 
      for(i in 1:length(out))
        if(length(out[[i]])>1){
          # get attributes
          class<-attributes(out[[i]])$class
          caption<-attributes(out[[i]])$caption
          footer<-attributes(out[[i]])$footer
          
           out[[i]]<-unifyMatrixContent(out[[i]],letter.convert=FALSE,
                                        greek2text=FALSE,
                                        correctComma=correctComma,text2num=FALSE)
           
           if(isTRUE(greek2text)|isTRUE(letter.convert)) out[[i]]<-matrix(letter.convert(out[[i]],greek2text=greek2text),ncol=ncol(out[[i]]))
        
           # set attributes
           attributes(out[[i]])$class<-class
           attributes(out[[i]])$caption<-caption
           attributes(out[[i]])$footer<-footer
           
           }
    })
  
  # name list elements  
  if(is.list(out))
   if(length(out)>0)
     names(out)<-paste("Table",1:length(out))
  
  # output
    return(out)
}


#############################
# function for single table
singleTable2matrix<-function(x,letter.convert=TRUE,# Logical. If TRUE hex codes will be unified and converted to utf-8
                             greek2text=FALSE,
                             replicate=TRUE, # Logical. If TRUE the content of cells with row/col span is replicated in all connected cells; if FALSE disconnected cells will be empty,
                             rm.empty.row.col=TRUE,
                             rm.html=FALSE,
                             collapseHeader=TRUE, # Logical. If TRUE header cells are collapsed for each column if header has 2 or more lines
                             header2colnames=FALSE, # Logical. If TRUE and collapse header==TRUE first table row is used for column names and removed from table
                             repNums=FALSE
){
  
  # escape if x is empty
  if(length(x)==0) return(NULL)
  # table rows
  rows<-unlist(JATSdecoder::strsplit2(x,"</tr>",type="after"))
  # remove last row
  rows<-rows[-length(rows)]
  
  #############
  # rows to cells
  cells<-JATSdecoder::strsplit2(rows,"<t[dh][ >]|<t[dh]/>","before")
  # remove first row
  cells<-lapply(cells,"[",-1)
  
  # remove empty cells
  rows<-rows[unlist(lapply(cells,length))>0]
  cells<-cells[unlist(lapply(cells,length))>0]
  
  # escape if no cells are detected
  if(length(cells)==0) return(NULL)
  
  # remove col/rowspan==1
  cells<-lapply(cells,function(x) gsub(' *rowspan="1"| *colspan="1"',"",x))
  rows<-lapply(rows,function(x) gsub(' *rowspan="1"| *colspan="1"',"",x))
  
  # is numeric cell or cell with result
  text<-lapply(cells,function(x) gsub("<[^>]*>","",JATSdecoder::letter.convert(x)))
  is.num<-lapply(text,function(x) gsub("\\^.|[[:punct:] 0-9]","",x)==""|length(grep("[<=>] -*[\\.0-9]",x))>0)
  is.num
  
  ##############
  # function to insert cells to vector
  insert<-function(value,x,at){
    if(length(value)==0) return(x)
    if(length(at)>1) return(x)
    if(at<=1) out<-c(value,x[(at):length(x)])  
    if(at>1&at<=length(x)) out<-c(x[1:(at-1)],value,x[(at):length(x)])  
    if(at>length(x)) out<-c(x[1:length(x)],value)  
    return(out)
  }
  
  ##############
  # function to replicate cells with colspan>1
  insert.colspan<-function(cells,replicate=TRUE,repNums=FALSE){
    line<-which(grepl('colspan=..[1-9]|colspan=.[1-9]',cells))
    if(length(line)>0){
      for(k in line){
        cellIndex<-which(grepl('colspan="[1-9]',cells[[k]]))
        times<-as.numeric(gsub('.*colspan="([1-9][0-9]*).*',"\\1",cells[[k]][cellIndex]))-1
        for(m in 1:length(cellIndex)){
          if(m==1){
            if(isTRUE(repNums)) cell4rep<-ifelse(replicate==TRUE,cells[[k]][cellIndex[m]],"")
            if(isFALSE(repNums)) cell4rep<-ifelse(replicate==TRUE & !is.num[[k]][cellIndex[m]],
                                                cells[[k]][cellIndex[m]],"")
            cells[[k]]<-insert(rep(cell4rep,times[m]),cells[[k]],cellIndex[m]+1)
          }
          if(m>1){
            if(isTRUE(repNums)) cell4rep<-ifelse(replicate==TRUE,cells[[k]][cellIndex[m]+sum(times[1:(m-1)])],"")
            if(isFALSE(repNums)) cell4rep<-ifelse(replicate==TRUE & !is.num[[k]][cellIndex[m]],
                             cells[[k]][cellIndex[m]+sum(times[1:(m-1)])],"")
            cells[[k]]<-insert(rep(cell4rep,times[m]),cells[[k]],cellIndex[m]+1+sum(times[1:(m-1)]))
          }
        } # end entering
        cells[[k]]<-gsub(' *colspan="[0-9][0-9]*"',"",cells[[k]])
      } # per line
    }
    return(cells)
  }
  
  # apply function to insert cols by colspan
  cells<-insert.colspan(cells,replicate=replicate,repNums=repNums)
  
  
  ########  
  # function to replicate cells with rowspan>1
  insert.rowspan<-function(cells,replicate=TRUE,repNums=FALSE){
    warn<-FALSE
    # lines with rowspan
    line<-which(grepl('rowspan=..[1-9]|rowspan=.[1-9]',cells))
    while(length(line)>0){
      # line with first position of rowspan in cells
      line<-line[order(unlist(lapply(cells[line],function(x) grep('rowspan="[1-9]',x)[1])))][1]
      # first cell index
      cellIndex<-which(grepl('rowspan="[1-9]',cells[[line]]))[1]
      times<-as.numeric(gsub('.*rowspan="([1-9][0-9]*).*',"\\1",cells[[line]][cellIndex]))-1
      # check if new cell is outside normal table area
      if((line+times)>length(cells)) warn<-TRUE
      # remove rowspan from processed cell
      cells[[line]][cellIndex]<-gsub(' *rowspan="[1-9][0-9]*"',"",cells[[line]][cellIndex])
      if(times>0&(line+times)<=length(cells)){
        if(isTRUE(repNums))         cell4rep<-ifelse(replicate==TRUE , 
                                                   cells[[line]][cellIndex],"")
        if(isFALSE(repNums)) cell4rep<-ifelse(replicate==TRUE & !is.num[[line]][cellIndex], 
                         cells[[line]][cellIndex],"")
        for(m in 1:times){
          cells[[line+m]]<-insert(cell4rep,cells[[line+m]],cellIndex)
        }
      }
      # lines with rowspan left 
      line<-which(grepl('rowspan=..[1-9]',cells))
    }
    if(isTRUE(warn)) warning("Table compiling might have gone wrong due to complexety of cell connections.",call.=FALSE)
    
    return(cells)
  }
  
  # apply function to insert cells by rowspan
  cells<-insert.rowspan(cells,replicate=replicate,repNums=repNums)
  
  
  # escape if no cells are left
  if(length(cells)==0) return(NULL)
  
  # add missing cells to end of rows
  if(sum(!duplicated(unlist(lapply(cells,length))))>1){
    ind<-which(max(unlist(lapply(cells,length)))-unlist(lapply(cells,length))!=0)
    for(j in ind)   suppressWarnings(cells[[j]]<-c(cells[[j]],rep("",max(unlist(lapply(cells,length)))-unlist(lapply(cells,length))[j])))
  }
  
  # set NA to ""
  if(sum(unlist(lapply(cells,function(x) sum(is.na(x)))))>0){
    na<-which(unlist(lapply(cells,function(x) sum(is.na(x))))>0)
    for(k in na) cells[[k]][is.na(cells[[k]])]<-""
  }
  
  # collapse first header lines if header has 2 or more lines
  ind<-ind1<-grepl("</*th>",cells)
  if(length((1:length(ind))[!ind])>0 & isTRUE(collapseHeader) & length(rows)>1 & sum(grepl("</*th>",cells))>1){
    ind[min((1:length(ind))[!ind]):length(ind)]<-FALSE
    # abort if table has multiple headers  
    if(sum(ind1)!=sum(ind)){
      warnings("The table contains multiple headers. Collapsing headers and conversion to data.frame was omitted.",call.=FALSE)
      collapseHeader<-FALSE
    }else{
      # set index for second block of header lines to FALSE 
      #ind[min((1:length(ind))[!ind]):length(ind)]<-FALSE
      if(sum(ind,na.rm=TRUE)>1){
        h<-rep("",length(cells[[1]]))
        for(j in 1:sum(ind)){
          w<- h!=unlist(cells[ind][j])
          h[w]<-gsub("^ ","",paste(h[w],unlist(cells[ind][j])[w]))
        }
        
        # insert collapsed header
        cells[[1]]<-h
        cells<-cells[-(2:sum(ind))]
      }
    }
  }  
  
  ###########
  # clean up

  # remove table and header html
  cells<-lapply(cells,function(x) gsub("<[/]*th>| *<th/>|<th [^>]*>","",x))
  cells<-lapply(cells,function(x) gsub("</t[dr]>|</t[dr]>|<t[dr]/*>|<t[dr] [^>]*>","",x))
  # remove html  
  if(isTRUE(rm.html)){
    # convert </break> to space, <sub> to _, <sup> to ^ and remove all other html-tags
    cells<-lapply(cells,function(x) gsub("</*break/*>"," ",x))
    cells<-lapply(cells,function(x) gsub(" *<sub> *","_",x))
    cells<-lapply(cells,function(x) gsub(" *<sup> *","^",x))
    # convert bold and italic numbers to number^bold/number^italic
    cells<-lapply(cells,function(x) gsub("</bo*l*d*>([\\.,; ]*)<bo*l*d*>","\\1",x))
    cells<-lapply(cells,function(x) gsub("</it*a*l*i*c*>([\\.,; ]*)<it*a*l*i*c*>","\\1",x))
    cells<-lapply(cells,function(x) gsub("([0-9])</bold>","\\1^bold",gsub("([0-9])</italic>","\\1^italic",x)))
    cells<-lapply(cells,function(x) gsub("([0-9])</b>","\\1^bold",gsub("([0-9])</i>","\\1^italic",x)))
    # remove styles
    cells<-lapply(cells,function(x) gsub(">[\\.@][a-z][^\\{]*\\{[a-z].*\\}\\}",">",x))
    cells<-lapply(cells,function(x) gsub(">[\\.@][a-z][^\\{]*\\{[a-z][^\\{]*\\}",">",x))
    # remove all other tags
    cells<-lapply(cells,function(x) gsub("</*[a-z][^>]*/*>|</*[a-z]/*>","",x))
    cells<-lapply(cells,function(x) gsub("</*inline[^>]*/*>|</*inline[^>]*/*>","",x))
    # space reduction
    cells<-lapply(cells,function(x) gsub("  *"," ",x))
    cells<-lapply(cells,function(x) gsub("^ $","",x))
    # remove styles
    cells<-lapply(cells,function(x) gsub("[\\.@][a-z][^\\{]*\\{[a-z].*\\}\\}","",x))
    cells<-lapply(cells,function(x) gsub("[\\.@][a-z][^\\{]*\\{[a-z][^\\{]*\\}","",x))
  }
  # escape
  if(length(cells)==0) return(NULL)
  # convert special characters
  if(isTRUE(letter.convert)) cells<-lapply(cells,JATSdecoder::letter.convert,greek2text=greek2text)
  # remove duplicated rows
 # if(isTRUE(rm.duplicated)){
#    rowText<-unlist(lapply(cells,function(x) paste(x,collapse="")))
#    cells<-cells[!duplicated(rowText)]
#  }
  # convert to matrix
  m<-suppressWarnings(matrix(unlist(cells),nrow=length(cells),byrow=TRUE))
  
  if(isTRUE(collapseHeader)) m<-headerHandling(m)
  
  # convert header text to column names
  if(isTRUE(header2colnames) & isTRUE(collapseHeader)) {
    colnames(m)<-m[1,]
    m<-m[-1,]
  }
  # remove empty cols/rows
  if(isTRUE(rm.empty.row.col)) m<-rm.empty(m)
  
  # get caption and footer for classification from html table-wrap
  leg<-c(gsub("[^[:punct:]]$","\\1.",get.caption(x)),get.footer(x))

  # classify table
  attributes(m)$class <- tableClass(m,legend=leg)
  # output
  return(m)
}# end  singleTable2matrix


rm.empty<-function(x){
  if(!is.matrix(x)) return(x)
  # non empty rows/cols
  i<-rowSums(x=="")!=ncol(x)
  j<-colSums(x=="")!=nrow(x)
  # escape if all empty
  if(sum(i)==0) return(NULL)
  if(sum(i)>0|sum(j)>0){
   # remove row/cols
   x<-x[i,]
   if(is.vector(x)) x<-matrix(x,ncol=length(x))
   x<-x[,j]
   if(is.vector(x)) x<-matrix(x,nrow=length(x))
  }
  return(x)
}
