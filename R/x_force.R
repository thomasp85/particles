#' Attrack particles towards a horizontal position
#'
#' This force simply pulls particles towards a fixed position on the x-axis.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `strength` : The strength with which the attraction occurs
#' - `x` : The position on the x-axis to pull towards.
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
x_force <- structure(list(
  x = NULL,
  strength = NULL,
  x_quo = NULL,
  strength_quo = NULL
), class = c('x_force', 'force'))
#' @export
print.x_force <- function(x, ...) {
  cat('X Force:\n')
  cat('* A force that implements horizontal attraction\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
train_force.x_force <- function(force, particles, x = NULL, strength = NULL, ...) {
  force$x_quo <- enquo(x)
  force$strength_quo <- enquo(strength)
  nodes <- as_tibble(particles, active = 'nodes')
  x <- eval_tidy(force$x_quo, nodes) %||% 0
  strength <- eval_tidy(force$strength_quo, nodes) %||% 0.1

  force$x <- rep(x, length.out = nrow(nodes))
  force$strength <- rep(strength, length.out = nrow(nodes))
  force
}
apply_force.x_force <- function(force, particles, pos, vel, alpha, ...) {
  vel[, 1] <- vel[, 1] + (force$x - pos[, 1]) * force$strength * alpha
  list(position = pos, velocity = vel)
}
