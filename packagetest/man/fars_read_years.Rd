% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{reads files based on vector of one or more years}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{a vector of integers that represents years}
}
\value{
list of dataframes, selecting month and year
}
\description{
reads files based on vector of one or more years
}
\examples{
\dontrun{fars_summarize_years(2022)}
\dontrun{fars_summarize_years(c(2020, 2021, 2022))}
}
