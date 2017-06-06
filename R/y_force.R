#' Attrack particles towards a vertical position
#'
#' This force simply pulls particles towards a fixed position on the y-axis.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `strength` : The strength with which the attraction occurs
#' - `y` : The position on the y-axis to pull towards.
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
y_force <- structure(list(
  y = NULL,
  strength = NULL,
  y_quo = NULL,
  strength_quo = NULL
), class = c('y_force', 'force'))
#' @export
print.y_force <- function(x, ...) {
  cat('Y Force:\n')
  cat('* A force that implements vertical attraction\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
train_force.y_force <- function(force, particles, y = NULL, strength = NULL, ...) {
  force$y_quo <- enquo(y)
  force$strength_quo <- enquo(strength)
  nodes <- as_tibble(particles, active = 'nodes')
  y <- eval_tidy(force$y_quo, nodes) %||% 0
  strength <- eval_tidy(force$strength_quo, nodes) %||% 0.1

  force$y <- rep(y, length.out = nrow(nodes))
  force$strength <- rep(strength, length.out = nrow(nodes))
  force
}
apply_force.y_force <- function(force, particles, pos, vel, alpha, ...) {
  vel[, 2] <- vel[, 2] + (force$y - pos[, 2]) * force$strength * alpha
  list(position = pos, velocity = vel)
}
