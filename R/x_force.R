#' Attract particles towards a horizontal position
#'
#' This force simply pulls particles towards a fixed position on the x-axis.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `strength` : The strength with which the attraction occurs (*tidy eval*)
#' - `x` : The position on the x-axis to pull towards. (*tidy eval*)
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
#' @export
train_force.x_force <- function(force, particles, x = NULL, strength = NULL, ...) {
  force <- NextMethod()
  force$x_quo <- enquo(x)
  force$strength_quo <- enquo(strength)
  nodes <- as_tibble(particles, active = 'nodes')
  x <- eval_tidy(force$x_quo, nodes) %||% 0
  strength <- eval_tidy(force$strength_quo, nodes) %||% 0.1

  force$x <- rep(x, length.out = nrow(nodes))
  force$strength <- rep(strength, length.out = nrow(nodes))
  force
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_force.x_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  force$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
  force <- update_quo(force, 'x', dots, nodes, new_particles, 0)
  force <- update_quo(force, 'strength', dots, nodes, new_particles, 0.1)
  force
}
#' @export
apply_force.x_force <- function(force, particles, pos, vel, alpha, ...) {
  vel[, 1] <- vel[, 1] + (force$x - pos[, 1]) * force$strength * alpha
  list(position = pos, velocity = vel)
}
