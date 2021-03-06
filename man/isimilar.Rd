% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatingtables.R
\name{isimilar}
\alias{isimilar}
\title{isimilar is a function which aims is to ensure that the unique ids are equal between the workingset and the restable as for the samples.}
\usage{
isimilar(restab, pdata, workingset)
}
\arguments{
\item{restab}{A statistical dataframe}

\item{pdata}{A dataframe that associates samples to their respective biological conditions}

\item{workingset}{A normalized expression dataframe}
}
\value{
A boolean list
}
\description{
isimilar is a function which aims is to ensure that the unique ids are equal between the workingset and the restable as for the samples.
}
