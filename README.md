# tableParser (1.0.3)
Converts HTML-tables and character matrices into a text vector and extracts and checks statistical standard test results.

The package contains several functions to extract, unify and parse content from scientific tables into a human readable text format by simulating the experience of a screen reader for visually impaired users. It can process HTML-encoded tables, as well as native R character matrices. The functions *table2matrix()*, *table2text()* and *table2stats()* can be applied on documents in HTML, HML, XML, CERMXML, as well as DOCX and PDF file format. The table extraction from DOCX files is performed with the function *docx2matrix()*, tables in PDF documents are extracted with the 'tabulapdf' package. 

The textual representation of characters in matrix content can be unified with *unifyMatrix()* before parsing. The function *table2stats()* extracts tabled statistical results. The function further unifies the parsed text, which is then processed with *JATSdecoder::standardStats()*, in order to extract all statistical standard results and check the reported and coded p-values for consistency, if possible. 

Important note: Due to the great variability in table structures and complexity, parsing accuracy may vary. For best results, it is recommended to work with simple, accessible, and barrier-free table structures to minimize parsing errors. 


## Installation 
```R
# latest CRAN release
install.packages("tableParser")

# latest github version
if(require(devtools)!=TRUE) install.packages("devtools")
devtools::install_github("ingmarboeschen/tableParser")
```

## Examples
```R
library(tableParser)
## Download example files with tables from this repo to temp directory
docxFilePath<-paste0(tempdir(),"/","tableExamples.docx")
htmlFilePath<-paste0(tempdir(),"/","tableExamples.html")
pdfFilePath<-paste0(tempdir(),"/","tableExamples.pdf")

download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx",
              docxFilePath)
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.html",
              htmlFilePath)
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.pdf",
              pdfFilePath)

## Extract tables from example files
table2matrix(docxFilePath)
table2matrix(htmlFilePath,rm.html=TRUE)
table2matrix(pdfFilePath)
# Note: The extraction of tables with the tablulapdf package does not work properly here.
# Also, the table's caption and footnotes cannot be used for decoding (e.g., p-values).
# This affects all further processes and results.

## Parse tabled content from example files
table2text(docxFilePath)
table2text(htmlFilePath)
table2text(pdfFilePath)

## Extract the detected statistical standard results and validate the reported and coded p-values
#ä with the recaluclated p-values
table2stats(docxFilePath,checkP=TRUE,estimateZ=T)
table2stats(htmlFilePath,checkP=TRUE,estimateZ=T)
table2stats(pdfFilePath,checkP=TRUE,estimateZ=T,standardPcoding=TRUE)
 
```

## Web application
Test tableParser without installation here:
[http://134.100.146.165:3838/tableParser/](http://134.100.146.165:3838/tableParser/)


## Reference
```R
@Manual{,
    title = {tableParser: Parse Tabled Content to Text Vector and Extract Statistical Standard Results},
    author = {Ingmar Böschen},
    year = {2026},
    note = {R package version 1.0.3},
    url = {https://github.com/ingmarboeschen/tableParser},
  }
```
  
