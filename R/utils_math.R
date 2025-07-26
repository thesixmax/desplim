#' Internal mathematical functions
#' @keywords internal
#' @noRd
.angle_fun <- function(a, b, c) {
  a_x <- a[1, 1]
  a_y <- a[2, 1]
  b_x <- b[1, 1]
  b_y <- b[2, 1]
  c_x <- c[1, 1]
  c_y <- c[2, 1]
  vector1_x <- a_x - b_x
  vector1_y <- a_y - b_y
  vector2_x <- c_x - b_x
  vector2_y <- c_y - b_y
  mag_sq_vector1 <- vector1_x^2 + vector1_y^2
  mag_sq_vector2 <- vector2_x^2 + vector2_y^2
  epsilon <- 1e-9
  if (mag_sq_vector1 < epsilon || mag_sq_vector2 < epsilon) {
    return(NA_real_)
  }
  dot_product <- vector1_x * vector2_x + vector1_y * vector2_y
  magnitudes_product <- sqrt(mag_sq_vector1) * sqrt(mag_sq_vector2)
  if (abs(magnitudes_product) < epsilon) {
    return(NA_real_)
  }
  ratio <- dot_product / magnitudes_product
  ratio <- max(-1.0, min(1.0, ratio))
  angle_rad <- acos(ratio)
  angle_deg <- angle_rad * (180.0 / pi)
  return(angle_deg)
}
