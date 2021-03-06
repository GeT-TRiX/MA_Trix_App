% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vennplot.R
\name{topngenes}
\alias{topngenes}
\title{topngenes is a function which aims is to plot the top n genes for the selected contrat(s)}
\usage{
topngenes(dfinter, mycont, inputtop, meandup = "probes")
}
\arguments{
\item{dfinter}{A dataframe which combines (unique ids, genes and logFC)}

\item{mycont}{A character Vector of the selected comparisons}

\item{inputtop}{A numeric value}

\item{meandup}{A character value to get to the level of unique ids or genes}
}
\value{
A ggplot barplot object
}
\description{
topngenes is a function which aims is to plot the top n genes for the selected contrat(s)
}
