% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decideTestTrix.R
\name{decTestTRiX}
\alias{decTestTRiX}
\title{decTestTRiX is a function}
\usage{
decTestTRiX(adj, logfc, pval, DEGcutoff = 0.05, FC = 1,
  cutoff_meth = "FDR", maxDE = NULL, contrast = 1:ncol(adj))
}
\arguments{
\item{adj}{A data frame with the "adj.P.Val"}

\item{logfc}{A data frame with the "logFC"}

\item{pval}{A data frame with the "P.value"}

\item{DEGcutoff}{A numeric value for tresholding the data on the pvalue}

\item{FC}{A numeric value for tresholding the data on the FC value}

\item{cutoff_meth}{A character to choose the method appropriate, "FDR" in order to cut off with the "adj.P.Val" and "None" for the "P.value"; default is set to "FDR"}

\item{maxDE}{A numeric value that gives a maximal number of genes to display for all the differents contrasts}

\item{contrast}{A numeric value representing the length of the data frame}
}
\value{
\DEsel a matrix of double values containing the signficant genes
}
\description{
decTestTRiX is a function
}
