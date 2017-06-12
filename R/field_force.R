#' Apply a vector field to particles
#'
#' This force adjusts the velocity of particles based on a supplied vector
#' field. The vector field can either be specified using x and y velocities, or
#' angle and magnitude. Velocity adjustments are calculated based on a bilinear
#' interpolation.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `x` : A matrix giving the velocity in the x direction at each grid point
#' - `y` : A matrix giving the velocity in the y direction at each grid point
#' - 'angle' : A matrix giving the direction of the velocity at each grid point.
#'   Will only be considered if `x` and `y` are missing.
#' - 'vel' : A single numeric or a matrix of the same dimensions as `angle`
#'   giving the magnitude of velocity at each grid point.
#' - `xlim` : The coordinate span of the vector field in the x direction.
#' - `ylim` : The coordinate span of the vector field in the y direction.
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
field_force <- structure(list(
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL
), class = c('field_force', 'force'))
#' @export
print.field_force <- function(x, ...) {
  cat('Field Force:\n')
  cat('* A force that applies a vector field to the particles\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
train_force.field_force <- function(force, particles, x, y, angle, vel, xlim = c(-10, 10), ylim = xlim, ...) {
  force <- NextMethod()
  if (missing(x) && missing(y)) {
    if (!is.matrix(vel)) vel <- matrix(vel, ncol = ncol(angle), nrow = nrow(angle))
    stopifnot(all(dim(angle) == dim(vel)))
    x <- cos(angle) * vel
    y <- sin(angle) * vel
  }
  stopifnot(all(dim(x) == dim(y)))
  dims <- dim(x)
  force$x_breaks <- seq(xlim[1], xlim[2], length.out = dims[2])
  force$y_breaks <- seq(ylim[1], ylim[2], length.out = dims[1])
  force$x <- x
  force$y <- y
  force
}
#' @importFrom akima bilinear
apply_force.field_force <- function(force, particles, pos, vel, alpha, ...) {
  vel_x <- bilinear(force$x_breaks, force$y_breaks, force$x, pos[, 1], pos[, 2])
  vel_y <- bilinear(force$x_breaks, force$y_breaks, force$y, pos[, 1], pos[, 2])
  vel <- vel + cbind(vel_x$z, vel_y$z)
  list(position = pos, velocity = vel)
}
