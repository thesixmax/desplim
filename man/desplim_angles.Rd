% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desplim_angles.R
\name{desplim_angles}
\alias{desplim_angles}
\title{Calculate the minimum and maximum angles between two \code{sf} LINESTRING objects}
\usage{
desplim_angles(input_lines, reference_lines, cast_substring = TRUE)
}
\arguments{
\item{input_lines}{object of class sf of type LINESTRING or MULTILINESTRING.}

\item{reference_lines}{object of class sf of type LINESTRING or
MULTILINESTRING.}

\item{cast_substring}{logical; should the input linestrings be cast to
substring? Default is \code{TRUE}.}
}
\value{
An sf object with the geometries from \code{input_lines} with added
columns containing the values of the minimum and maximum angle.
}
\description{
Function for calculating the minimum and maximum angles
between two intersecting \code{sf} MULTILINESTRING or LINESTRING objects.
}
\details{
If the input and reference lines are already cast to substring,
the function can be sped up, notably by setting \code{cast_substring} to \code{FALSE}
for larger geometries.
}
\examples{
# Create input and reference lines
input_line <- sf::st_linestring(matrix(
  c(-1, 0, 0, 0, 1, 0),
  ncol = 2,
  byrow = TRUE
))
ref_line <- sf::st_linestring(matrix(
  c(0, -1, 0, 0, -0.5, 1),
  ncol = 2,
  byrow = TRUE
))
input_sf <- sf::st_sf(geom = sf::st_sfc(input_line), crs = 32613)
ref_sf <- sf::st_sf(geom = sf::st_sfc(ref_line), crs = 32613)

# Calculate angles
angles_sf <- desplim_angles(input_lines = input_sf, reference_lines = ref_sf)
print(angles_sf)

# Plot
plot(sf::st_geometry(input_sf), col = "red", lwd = 2)
plot(sf::st_geometry(ref_sf), add = TRUE, col = "blue", lwd = 2)
text(-0.1, 0.1, paste0(round(angles_sf$min_angle[[1]], 1), "°"), pos = 2)
text(-0.1, -0.1, paste0(round(angles_sf$max_angle[[1]], 1), "°"), pos = 2)
text(0.1, 0.1, paste0(round(angles_sf$max_angle[[2]], 1), "°"), pos = 4)
text(0.1, -0.1, paste0(round(angles_sf$min_angle[[2]], 1), "°"), pos = 4)
}
