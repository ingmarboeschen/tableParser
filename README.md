# tableParser (prototype)
Converts HTML-tables and character matrices into a text vector and extracts and checks statistical standard test results.

The package contains several functions to extract, unify and parse content from scientific tables into a human readable text format by simulating the experience of a screen reader for visually impaired users. It can process HTML-encoded tables, as well as native R character matrices. The functions *table2matrix()*, *table2text()* and *table2stats()* can be applied on documents in HTML, HML, XML, CERMXML, as well as DOCX and PDF file format. The table extraction from DOCX files is performed with the function *docx2matrix()*, tables in PDF documents are extracted with the 'tabulapdf' package. 

The textual representation of characters in matrix content can be unified with *unifyMatrix()* before parsing. The function *table2stats()* extracts tabled statistical results. The function further unifies the parsed text, which is then processed with *JATSdecoder::standardStats()*, in order to extract all statistical standard results and check the reported and coded p-values for consistency, if possible. 

Important note: Due to the great variability in table structures and complexity, parsing accuracy may vary. For best results, it is recommended to work with simple, accessible, and barrier-free table structures to minimize parsing errors. 


## Installation with the devtools package
```R
if(require(devtools)!=TRUE) install.packages("devtools")
devtools::install_github("ingmarboeschen/tableParser")
```

## Examples
```R
library(tableParser)
## Download example files with tables from this repo
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx","tableExamples.docx")
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.html","tableExamples.html")
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.pdf","tableExamples.pdf")

# extract tables from HTML, HML, DOCX, PDF documents
table2matrix("tableExamples.docx")
table2matrix("tableExamples.html",rm.html=TRUE)
table2matrix("tableExamples.pdf")

# parse tabled content in HTML, HML, DOCX, PDF documents to text vector 
table2text("tableExamples.docx")
table2text("tableExamples.html")
table2text("tableExamples.pdf")

# extract and check statistical standard results
table2stats("tableExamples.docx",check=TRUE,estimateZ=T)
table2stats("tableExamples.html",check=TRUE,estimateZ=T)
table2stats("tableExamples.pdf",check=TRUE,estimateZ=T)

# activate next line to remove downloaded files
# file.remove("tableExamples.docx"); file.remove("tableExamples.html") file.remove("tableExamples.pdf")
 
```

## Web application
Test tableParser without installation here:
[http://134.100.146.165:3838/tableParser/](http://134.100.146.165:3838/tableParser/)

