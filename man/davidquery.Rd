% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gosearch.R
\name{davidquery}
\alias{davidquery}
\title{davidquery is a function which aim to query DWS with lists of entrez ids to return as output a Functional Annotation Summary dataframe}
\usage{
davidquery(entrezids, species, mycat)
}
\arguments{
\item{entrezids}{List of entrez IDS}

\item{species}{A character name of the species}

\item{mycat}{A character vector corresponding to the categories of the functional analysis: MF, CC, BP or KEGG pathway}
}
\value{
List of data frames of each clusters
}
\description{
davidquery is a function which aim to query DWS with lists of entrez ids to return as output a Functional Annotation Summary dataframe
}
