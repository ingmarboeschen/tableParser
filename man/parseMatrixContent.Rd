% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseMatrixContent.R
\name{parseMatrixContent}
\alias{parseMatrixContent}
\title{parseMatrixContent}
\usage{
parseMatrixContent(
  x,
  legend = NULL,
  standardPcoding = TRUE,
  expandAbbreviations = TRUE
)
}
\arguments{
\item{x}{A character matrix or list with a character matrix as first and only element.}

\item{legend}{The tables caption/footer notes as character vector.}

\item{standardPcoding}{Logical. If TRUE, and no other detection of p-value coding is detected, standard coding of p-values is assumed to be: * p<.05, ** p<.01 and *** p<.001.}

\item{expandAbbreviations}{Logical. If TRUE, detected abbreviations are expanded to label detected in table caption/footer with tableParser::legendCodings().}
}
\value{
A text vector.
}
\description{
Function to parse content from a character matrix into a text vector. This is the basic funtion of tableParser, that is implementent in matrix2text() and table2text()
}
