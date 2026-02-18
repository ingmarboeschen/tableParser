# tableParser (1.0.3)
Converts HTML-tables and character matrices into a text vector and extracts and checks statistical standard test results.

The package contains several functions to extract, unify and parse content from scientific tables into a human readable text format by simulating the experience of a screen reader for visually impaired users. It can process HTML-encoded tables, as well as native R character matrices. The functions *table2matrix()*, *table2text()* and *table2stats()* can be applied on documents in HTML, HML, XML, as well as DOCX and PDF file format. The table extraction from DOCX files is performed with the function *docx2matrix()*, tables in PDF documents are extracted with the 'tabulapdf' package. 

The textual representation of characters in matrix content can be unified with *unifyMatrix()* before parsing. The function *table2stats()* extracts tabled statistical results. The function further unifies the parsed text, which is then processed with [JATSdecoder](https://CRAN.R-project.org/package=tableParser/)'s function standardStats(), in order to extract all statistical standard results (t, Z, χ2, F, r, d, β, SE, r, d, η2, ω2, OR, RR, p), the coded p-values and recompute p-values if possible. 

Important note: Due to the great variability in table structures and complexity, parsing accuracy may vary. For best results, it is recommended to work with simple, accessible, and barrier-free table structures to minimize parsing errors. 

## How does it work?
The following process tree is a simplified representation of the conversion of tabled content into a text string and finally extract the statistical standard results. The table within a document is first transposed into a matrix. The matrix is then collapsed into text lines, considering the detected codings within the caption and footnotes (e.g. '*' for p<.05). The representation of numerical results within the text lines are further unified and then processed with JATSdecoder's function standardStats() to detect the reported statistical standard results and recompute p-values if possible.

![alt text](https://github.com/ingmarboeschen/tableParser/blob/main/processTree.png?raw=true)

The collapsing of the matrix content to text is based on the decision of a table classifier. In the case of correlation matrices, the reported sample size specified within the caption or footnote is subtracted by two and then imputed as degrees of freedom in brackets. This enables a subsequent recomputation of many p-values.


## Matrix extraction, collapsing and checking of statistical standard results
To demonstrate how tableParser works, this repo contains three documents in DOCX, HTML and PDF format with the same example tables. For a quick insight, the processing of example Table 4 is displayed here. 

Input matrix gathered with:

__*table2matrix(file.path)*__:

|  			Variable 		     |  			SSq 		  |  			df 		 |  			MSq 		  |  			F 		    |  			P(>F) 		 |
|:----------------|:--------|:------|:--------|:--------|:---------|
|  			__Factor A__ 		     |  			__12__ 		   |  			__2__ 		  |  			__3__ 		    |  			__9.09__ 		 |  			__.00__ 		   |
|  			__Factor B__ 		     |  			*4.5* 		  |  			*1* 		  |  			*4.5* 		  |  			*6.82* 		 |  			*.01* 		   |
|  			__Factor A * B__ 		 |  			3 		    |  			2 		  |  			1.5 		  |  			2.27 		 |  			.12 		   |
|  			__Residuals__ 		    |  			20 		   |  			30 		 |  			0.66 		 |  			   			 		  |  			   			 		   |
|  			__Total__ 		        |  			39.5 		 |  			35 		 |  			1.13 		 |  			   			 		  |  			   			 		   |

*Italic values are p<.05.*
*Bold valus are significant with p<.01.*

The table contains a footnote with codings for p-values, which can be used to impute these values to the table by setting the argument *decodeP=TRUE*. By activating the option *noSign2P=TRUE*, the absence of a coding for p-values is treated as a coding, and non significant p-values are imputed (this may result in false positive imputations!). A special handling and impuation algorithm for degrees of freedom can be activated with *dfHandling=TRUE*. This is very useful for checking the reported results on consistency.

Output of the collapsed matrix with decoded p-values and degree of freedom handling gathered with:

__*table2text(file.path, decodeP=TRUE, noSign2p=TRUE ,dfHandling=TRUE)*__:

|__[1]__| __"Variable: Factor A, SSq=12;; p<0.01, df1=2, df2=30;; p<0.01, MSq=3;; p<0.01, F=9.09;; p<0.01, P(>F)=.00;; p<0.01"__ |    
|:----|:----|
|__[2]__| __"Variable: Factor B, SSq=4.5;; p<0.05, df1=1, df2=30;; p<0.05, MSq=4.5;; p<0.05, F=6.82;; p<0.05, P(>F)=.01;; p<0.05"__ | 
|__[3]__| __"Variable: Factor A * B, SSq=3;; p>0.05, df1=2, df2=30;; p>0.05, MSq=1.5;; p>0.05, F=2.27;; p>0.05, P(>F)=.12;; p>0.05"__|
|__[4]__| __"Variable: Residuals, SSq=20, df2=30, MSq=0.66"__ |                                                                       
|__[5]__| __"Variable: Total, SSq=39.5, df=35, MSq=1.13"__|

Finally, after some further uniformizations, the extraction of statistical standard results is performed with the function standardStats from the JATSdecoder package. The resulting data frame can easily be processed for analysis of results and p-value consistency check ups, when the argument *checkP=TRUE*.

__*table2stats(file.path, noSign2p=TRUE, dfHandling=TRUE, checkP=TRUE)*__:

|  |  F_op  |  F | df1 | df2 |  p_op |   p| codedP_op | codedP | recalculatedP | deltaP2tailed |  error | errorType | alpha4check|
|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|:--|
|1|  =| 9.09 |  2 | 30 |   =| 0.00 |        <  | 0.01 |       0.0008  |      0.0008 |  0 |  <NA>|  0.05 |     
|2|  =| 6.82 |  1 | 30 |   =| 0.01 |        <  | 0.05 |       0.0139  |      0.0039 |  0 |  <NA>|  0.05 |
|3|  =| 2.27 |  2 | 30 |   =| 0.12 |        >  | 0.05 |       0.1208  |      0.0008 |  0 |  <NA>|  0.05 |

Discover further examples for the special handling of correlation matrices and model result tables, by processing the tableExamples documents in the main folder of this repo after installation of tableParser. 


## Installation 
```R
# latest CRAN release
install.packages("tableParser")

# latest github version
if(require(devtools)!=TRUE) install.packages("devtools")
devtools::install_github("ingmarboeschen/tableParser")
```

## Examples for table processing in docx, html and pdf documents
I have prepared a document with several example table structures to demonstrate tableParser's capabilities. You may manually download the examples in three file formats, or use the following lines to temporarely store them within your temporary R directory (tempdir()). 
```R
# Load the package
library(tableParser)

# prepare temporary file paths
docxFilePath<-paste0(tempdir(),"/","tableExamples.docx")
htmlFilePath<-paste0(tempdir(),"/","tableExamples.html")
pdfFilePath<-paste0(tempdir(),"/","tableExamples.pdf")

# Download example DOCX, HTML, and PDF file with tables from this repo to temp directory
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.docx",docxFilePath)
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.html",htmlFilePath)
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/tableExamples.pdf",pdfFilePath)

## Extract tables as matrices from example files
table2matrix(docxFilePath)
table2matrix(htmlFilePath,rm.html=TRUE)
table2matrix(pdfFilePath)
# Note: The extraction of tables with the tablulapdf package does not work properly here.
# Also, the table's caption and footnotes cannot be used for decoding (e.g., p-values).
# This affects all further processes and results.

## Collapse tabled content to text from example files
table2text(docxFilePath)
table2text(htmlFilePath,decodeP=TRUE)
table2text(pdfFilePath,decodeP=TRUE,standardPcoding=TRUE,noSign2p=TRUE)

## Extract the detected statistical standard results and validate the reported and coded p-values
## with the recaluclated p-values
table2stats(docxFilePath,checkP=TRUE,estimateZ=T)
table2stats(htmlFilePath,checkP=TRUE,estimateZ=T,noSign2p=T,alpha=.01)
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
  
