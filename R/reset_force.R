#' Reset the velocity verlet of particles to a fixed value
#'
#' This force resets the velocity of particles at each generation. It can be
#' used if each generation should start from the same foundation rather than
#' accumulate as the simulation evolve. Particles where the parameters evaluates
#' to `NA` will ignore this force.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `xvel` : The x-velocity to reset to at each generation
#' - `yvel` : The y-velocity to reset to at each generation
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
reset_force <- structure(list(
  xvel = NULL,
  yvel = NULL,
  xvel_quo = NULL,
  yvel_quo = NULL
), class = c('reset_force', 'force'))
#' @export
print.reset_force <- function(x, ...) {
  cat('Reset Force:\n')
  cat('* A force that resets the velocity to a fixed value\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
#' @export
train_force.reset_force <- function(force, particles, xvel = NULL, yvel = NULL, ...) {
  force <- NextMethod()
  force$xvel_quo <- enquo(xvel)
  force$yvel_quo <- enquo(yvel)
  nodes <- as_tibble(particles, active = 'nodes')
  x <- eval_tidy(force$xvel_quo, nodes) %||% 0
  y <- eval_tidy(force$yvel_quo, nodes) %||% 0
  force$xvel <- rep(x, length.out = nrow(nodes))
  force$yvel <- rep(y, length.out = nrow(nodes))
  force
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_force.reset_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  force$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
  force <- update_quo(force, 'xvel', dots, nodes, new_particles, 0)
  force <- update_quo(force, 'yvel', dots, nodes, new_particles, 0)
  force
}
#' @importFrom stats na.omit
#' @export
apply_force.reset_force <- function(force, particles, pos, vel, alpha, ...) {
  vel[!is.na(force$xvel), 1] <- na.omit(force$xvel)
  vel[!is.na(force$yvel), 2] <- na.omit(force$yvel)
  list(position = pos, velocity = vel)
}
