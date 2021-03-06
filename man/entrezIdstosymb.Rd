% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrichmodule.R
\name{entrezIdstosymb}
\alias{entrezIdstosymb}
\title{entrezIdstosymb is a shiny module which aims that takes as input a dataframe with entrez ids and return a dataframe with gene symbols}
\usage{
entrezIdstosymb(input, output, session, data, cutgo, rows_selected)
}
\arguments{
\item{input}{Internal}

\item{output}{Internal}

\item{session}{Internal}

\item{data}{A statistical output dataframe of the DAVID web service}

\item{cutgo}{A numeric value to specify which cluster is selected}

\item{rows_selected}{A reactive character vector of the selected rows within the DAVID output table}
}
\value{
A reactive dataframe that associates Genes (genesymbols) with the corresponding term
}
\description{
entrezIdstosymb is a shiny module which aims that takes as input a dataframe with entrez ids and return a dataframe with gene symbols
}
