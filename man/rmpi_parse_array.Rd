% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internals.R
\name{rmpi_parse_array}
\alias{rmpi_parse_array}
\title{Get an array specified by a control record from the text lines analyzed in an \code{RMODPATH} \code{rmp_read.*} function}
\usage{
rmpi_parse_array(remaining_lines, nrow, ncol, nlay, ndim, ...)
}
\arguments{
\item{remaining_lines}{lines to read the array from}

\item{nrow}{number of rows in the array}

\item{ncol}{number of columns in the array}

\item{nlay}{number of layers in the array that should be read}

\item{ndim}{dimensions of the array to read; either 1, 2 or 3. Denotes the if the returned array should be 1D, 2D or 3D.}

\item{...}{additional arguments passed to \link[RMODFLOW]{rmfi_parse_array}}
}
\value{
A list containing the array and the remaining text of the MODPATH input file
}
\description{
Get an array specified by a control record from the text lines analyzed in an \code{RMODPATH} \code{rmp_read.*} function
}
\keyword{internal}
