#' Attract particles towards a vertical position
#'
#' This force simply pulls particles towards a fixed position on the y-axis.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `strength` : The strength with which the attraction occurs (*tidy eval*)
#' - `y` : The position on the y-axis to pull towards. (*tidy eval*)
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
#' @export
train_force.y_force <- function(force, particles, y = NULL, strength = NULL, ...) {
  force <- NextMethod()
  force$y_quo <- enquo(y)
  force$strength_quo <- enquo(strength)
  nodes <- as_tibble(particles, active = 'nodes')
  y <- eval_tidy(force$y_quo, nodes) %||% 0
  strength <- eval_tidy(force$strength_quo, nodes) %||% 0.1

  force$y <- rep(y, length.out = nrow(nodes))
  force$strength <- rep(strength, length.out = nrow(nodes))
  force
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_force.y_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  force$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
  force <- update_quo(force, 'y', dots, nodes, new_particles, 0)
  force <- update_quo(force, 'strength', dots, nodes, new_particles, 0.1)
  force
}
#' @export
apply_force.y_force <- function(force, particles, pos, vel, alpha, ...) {
  vel[, 2] <- vel[, 2] + (force$y - pos[, 2]) * force$strength * alpha
  list(position = pos, velocity = vel)
}
