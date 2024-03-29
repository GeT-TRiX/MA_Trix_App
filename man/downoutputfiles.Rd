% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/savemodules.R
\name{downoutputfiles}
\alias{downoutputfiles}
\title{downoutputfiles is a shiny module which aims is to export reactive plot}
\usage{
downoutputfiles(input, output, session, projectname, suffix = "plot.png",
  data, w = 12, h = 12, cutheat = F, volcform = F, hm = F,
  clustvenn = NULL, rown = NULL)
}
\arguments{
\item{input}{Internal}

\item{output}{Internal}

\item{session}{Internal}

\item{projectname}{A reactive character (MA project or session data)}

\item{suffix}{A character}

\item{data}{A reactive expression to be plot}

\item{w}{Width of the exported file}

\item{h}{Height pf the expored file}

\item{cutheat}{A boolean value}

\item{volcform}{A boolean value}

\item{hm}{A boolean value}

\item{clustvenn}{A boolean value. Default NULL.}

\item{rown}{A reactive character to display (show) or hide labels in the outputted heatmap}
}
\description{
downoutputfiles is a shiny module which aims is to export reactive plot
}
