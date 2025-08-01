# tableParser (prototype)
Converts HTML-tables and character matrices into a text vector and extracts statistical standard results

The package contains several functions to extract, unify and parse content from scientific tables into a human readable text format by simulating the experience of a screen reader for visually impaired users. It can process HTML-encoded tables, as well as native R character matrices. The functions *table2matrix()*, *table2text()* and *table2stats()* can be applied on documents in HTML, HML, XML, CERMXML, as well as DOCX and PDF file format. The table extraction from DOCX files is performed with the function *docx2matrix()*, tables in PDF documents are extracted with the 'tabulapdf' package. 

The textual representation of characters in matrix content can be unified with *unifyMatrix()* before parsing. The function *table2stats()* extracts tabled statistical results. The function further unifies the parsed text, which is then processed with *JATSdecoder::standardStats()*, in order to extract all statistical standard results and check the reported and coded p-values for consistency, if possible. 

Important note: Due to the great variability in table structures and complexity, parsing accuracy may vary. For best results, it is recommended to work with simple, accessible, and barrier-free table structures to minimize parsing errors. 

       <table width="321" cellpadding="2" cellspacing="0">
	<colgroup><col width="104">
	<col width="93">
	<col width="112">
	</colgroup><tbody><tr valign="top">
		<td width="104" height="28" style="border: none; padding: 0cm"><p class="western" align="left">
			<b>Demographics</b></p>
		</td>
		<td width="93" style="border: none; padding: 0cm"><p class="western" align="center">
			<b>Categories</b></p>
		</td>
		<td width="112" style="border: none; padding: 0cm"><p class="western" align="center">
			<b>Frequency (%)</b></p>
		</td>
	</tr>
	<tr valign="top">
		<td width="104" height="13" style="border: none; padding: 0cm"><p class="western" align="left">
			Age</p>
		</td>
		<td width="93" style="border: none; padding: 0cm"><p class="western" align="center">
			20–25</p>
		</td>
		<td width="112" style="border: none; padding: 0cm"><p class="western" align="center">
			78 (39.79)</p>
		</td>
	</tr>
	<tr>
		<td width="104" height="13" valign="bottom" style="border: none; padding: 0cm"><p class="western" align="left">
			<br>

			</p>
		</td>
		<td width="93" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			26–31</p>
		</td>
		<td width="112" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			65 (33.16)</p>
		</td>
	</tr>
	<tr>
		<td width="104" height="13" valign="bottom" style="border: none; padding: 0cm"><p class="western" align="left">
			<br>

			</p>
		</td>
		<td width="93" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			32–37</p>
		</td>
		<td width="112" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			32 (16.33)</p>
		</td>
	</tr>
	<tr>
		<td width="104" height="13" valign="bottom" style="border: none; padding: 0cm"><p class="western" align="left">
			<br>

			</p>
		</td>
		<td width="93" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			38 and older</p>
		</td>
		<td width="112" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			21 (10.72)</p>
		</td>
	</tr>
	<tr valign="top">
		<td width="104" height="13" style="border: none; padding: 0cm"><p class="western" align="left">
			Gender</p>
		</td>
		<td width="93" style="border: none; padding: 0cm"><p class="western" align="center">
			Men</p>
		</td>
		<td width="112" style="border: none; padding: 0cm"><p class="western" align="center">
			97 (49.49)</p>
		</td>
	</tr>
	<tr>
		<td width="104" height="13" valign="bottom" style="border: none; padding: 0cm"><p class="western" align="left">
			<br>

			</p>
		</td>
		<td width="93" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			Women</p>
		</td>
		<td width="112" valign="top" style="border: none; padding: 0cm"><p class="western" align="center">
			99 (50.51)</p>
		</td>
	</tr>
</tbody></table>
  

## Installation with the devtools package
```R
if(require(devtools)!=TRUE) install.packages("devtools")
devtools::install_github("ingmarboeschen/tableParser")
```

## Examples
```R
## Download example files with tables from this repo
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/test.docx","test.docx")
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/test.html","test.html")
download.file("https://github.com/ingmarboeschen/tableParser/raw/refs/heads/main/test.pdf","test.pdf")

# extract tables from HTML, HML, DOCX, PDF documents
table2matrix("test.docx")
table2matrix("test.html")
table2matrix("test.pdf")

# parse tabled content in HTML, HML, DOCX, PDF documents to text vector 
table2text("test.docx")
table2text("test.html")
table2text("test.pdf")

# extract and check statistical standard results
table2stats("test.docx",check=TRUE,estimateZ=T)
table2stats("test.html",check=TRUE,estimateZ=T)
table2stats("test.pdf",check=TRUE,estimateZ=T)

# activate next line to remove downloaded files
# file.remove("test.docx"); file.remove("test.html") file.remove("test.pdf")
 
```

## Web application
Test tableParser without installation here:
[http://134.100.146.165:3838/tableParser/](http://134.100.146.165:3838/tableParser/)

