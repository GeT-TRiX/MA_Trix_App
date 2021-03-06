% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vennplot.R
\name{Vennlist}
\alias{Vennlist}
\title{Vennlist is a function which aim is to return a list of signficant genes for a treshold defined by the user}
\usage{
Vennlist(adj, fc, regulation, cutoffpval, cutofffc)
}
\arguments{
\item{adj}{A dataframe subset of the alltoptable}

\item{fc}{A dataframe subset of the alltoptable}

\item{regulation}{A character for regulation ("up", "both" or "down")}

\item{cutoffpval}{Cut-off for absolute log2 fold-change; default = 1.0}

\item{cutofffc}{Cut-off for pvalue; default = 0.05}
}
\value{
A list of significant genes for each contrast
}
\description{
Vennlist is a function which aim is to return a list of signficant genes for a treshold defined by the user
}
