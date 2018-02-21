#' Model attraction or repulsion between all particles in the system
#'
#' This force implements a n-body simulation using the Barnes-Hut approximation
#' for improved performance. An n-body simulation calculates attraction or
#' repulsion between all particles in a system based on their relative distances
#' and each particles capacity and can thus mimick gravity or electrostatic
#' repulsion.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `strength` : The attractive or repulsive force of the particles. If
#'   positive the particle attracts, if negative the particle repulses. The
#'   default is -30. (*tidy eval*)
#' - `theta` : The Barnes-Hut criterion governing the precision of the
#'   approximation. If 0, no approximation is made. Defaults to 0.9.
#' - `min_dist` : A lower distance threshold below which the forces will be
#'   damped, in order to avoid explosive forces when two particles gets very
#'   near each other.
#' - `max_dist` : A distance threshold above which the forces between particles
#'   are ignored. Using this will result in more local changes.
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
manybody_force <- structure(list(
  strength = NULL,
  strength_quo = NULL,
  theta = NULL,
  min_dist = NULL,
  max_dist = NULL
), class = c('manybody_force', 'force'))
#' @export
print.manybody_force <- function(x, ...) {
  cat('Manybody Force:\n')
  cat('* A force that models global attraction or repulsion between all particles\n')
}
#' @importFrom tidygraph as_tibble
#' @importFrom rlang enquo eval_tidy %||%
#' @export
train_force.manybody_force <- function(force, particles, strength = NULL, theta = NULL, min_dist = NULL, max_dist = NULL, ...) {
  force <- NextMethod()
  nodes <- as_tibble(particles, active = 'nodes')
  force$strength_quo <- enquo(strength)
  strength <- eval_tidy(force$strength_quo, nodes) %||% -30
  force$strength <- rep(strength, length.out = nrow(nodes))
  force$theta <- theta %||% 0.9
  force$min_dist <- min_dist %||% 0
  force$max_dist <- max_dist %||% -1
  force
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_force.manybody_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  force$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
  force <- update_quo(force, 'strength', dots, nodes, new_particles, 1)
  force <- update_unquo(force, 'theta', dots)
  force <- update_unquo(force, 'min_dist', dots)
  force <- update_unquo(force, 'max_dist', dots)
  force
}
#' @export
apply_force.manybody_force <- function(force, particles, pos, vel, alpha, ...) {
  vel_mod <- nbody(pos[force$include, , drop = FALSE], force$strength[force$include], force$theta, force$min_dist, force$max_dist, alpha)
  vel[force$include, ] <- vel[force$include, , drop = FALSE] + vel_mod
  list(position = pos, velocity = vel)
}
