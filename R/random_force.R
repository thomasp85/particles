#' Modify the velocity randomly at each step
#'
#' This force applies a random velocity modification to all particles. The
#' moodfication is uniformly distributed and bound be the parameters provided
#' during initialisation.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `xmin, xmax` : The bounds of the modification in the horizontal direction
#' - `ymin, ymax` : The bounds of the modification in the vertical direction
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
random_force <- structure(list(
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL
), class = c('random_force', 'force'))
#' @export
print.random_force <- function(x, ...) {
  cat('Random Force:\n')
  cat('* A force that modifies the velocity randomly at each step\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
train_force.random_force <- function(force, particles, xmin = -1, xmax = 1, ymin = -1, ymax = 1, ...) {
  force$xmin <- xmin
  force$xmax <- xmax
  force$ymin <- ymin
  force$ymax <- ymax
  force
}
apply_force.random_force <- function(force, particles, pos, vel, alpha, ...) {
  vel[, 1] <- vel[, 1] + runif(nrow(vel), force$xmin, force$xmax) * alpha
  vel[, 2] <- vel[, 2] + runif(nrow(vel), force$ymin, force$ymax) * alpha
  list(position = pos, velocity = vel)
}
