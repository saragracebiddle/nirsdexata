% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{remove_rows}
\alias{remove_rows}
\title{Remove rows from the data}
\usage{
remove_rows(
  dt = data.frame(),
  remove,
  ref_col = character(),
  value_col = character()
)
}
\arguments{
\item{dt}{data.frame}

\item{remove}{vector of data to remove}

\item{ref_col}{column name to find \code{remove}}

\item{value_col}{column to remove value from}
}
\value{
data.frame
}
\description{
input data must be in long format
and values are replaced with NA
replacing with NA helps with ggplot geom_line functions
}
