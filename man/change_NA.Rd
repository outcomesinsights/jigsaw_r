% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jigsaw.R
\name{change_NA}
\alias{change_NA}
\title{Replace NA Values}
\usage{
change_NA(original, new = 0)
}
\arguments{
\item{original}{column from data table}

\item{new}{replacement value (default = 0)}
}
\value{
Vector with NA replaced
}
\description{
replaces NA value with value in new (default = 0).
For a different way to accomplish this:
http://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table#7249454
}
\examples{
\dontrun{# nothing here yet}
}
