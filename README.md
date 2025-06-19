# tableParser
Converts HTML-tables and character matrices into a text vector and extracts statistical standard results

The package contains several functions to extract, unify and parse content from scientific tables into a human readable text format by simulating the experience of a screen reader for visually impaired users. 'tableParser' contains several functions to work with HTML-encoded tables, as well as native character matrixes. The functions *table2matrix()*, *table2text()* and *table2stats()* can be applied on documents in HTML, HML, XML, CERMXML, as well as DOCX and PDF file format. The table extraction from DOCX files is performed with the function *docx2matrix()*, tables in PDF documents are extracted with the 'tabulapdf' package. 

The textual representation of characters in matrix content can be unified with *unifyMatrix()* before parsing. The function *table2stats()* extracts tabled statistical results. The function further unifies the parsed text, which is then processed with *JATSdecoder::standardStats()*, in order to extract all statistical standard results and check the reported and coded p-values for consistency, if possible. 

Important note: Due to the great variability in table structures and complexity, parsing accuracy may vary. For best results, it is recommended to work with simple, accessible, and barrier-free table structures to minimize parsing errors. 

## Installation with the devtools package
```R
if(require(devtools)!=TRUE) install.packages("devtools")
devtools::install_github("ingmarboeschen/tableParser")
```

## Examples
```R
# extract tables from HTML, HML, DOCX, PDF documents
table2matrix(file.path)
# parse tabled content in HTML, HML, DOCX, PDF documents to text vector 
table2text(file.path)
# parse tabled content in HTML, HML, DOCX, PDF documents to text vector with statistical results and check consistency of reported and coded p-values 
table2stats(file.path,check=TRUE,estimateZ=T)

## Example with file "test.docx" from this repo
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/test.docx","test.docx")
# extract tables
tableParser::table2matrix("test.docx")
# parse tables to text
tableParser::table2text("test.docx")
# extract and check statistical standard results
tableParser::table2stats("test.docx",check=TRUE,estimateZ=T)
# activate next line to remove file
# file.remove("test.docx")
 
```
