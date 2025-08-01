% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desplim_cast_substring.R
\name{desplim_cast_substring}
\alias{desplim_cast_substring}
\title{Cast an \code{sf} LINESTRING or MULTILINESTRING object to substrings}
\usage{
desplim_cast_substring(input_lines)
}
\arguments{
\item{input_lines}{object of class sf with geometry type MULTILINESTRING or
LINESTRING.}
}
\value{
An sf object of type LINESTRING containing the substrings of
\code{input_lines}.
}
\description{
Function for computing the substrings of an \code{sf} MULTILINESTRING
or LINESTRING object.
}
\details{
To avoid issues of connecting points from spatially separate
linestrings, the function converts all MULTILINESTRING to LINESTRING. The
output is always LINESTRING of class matching \code{input_lines}. Inputs of type
sfc are not supported.
}
\examples{
# Create input line
input_line <- sf::st_linestring(matrix(
  c(-1, 0, 0, 0, 1, 0, 2, 0),
  ncol = 2,
  byrow = TRUE
))
input_sf <- sf::st_sf(geom = sf::st_sfc(input_line), crs = 32613)
print(input_sf)
# Cast to substring
substring_sf <- desplim_cast_substring(input_sf)
print(substring_sf)
}
