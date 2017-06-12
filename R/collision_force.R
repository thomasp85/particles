#' Models particles as circles with a given radius and pushes overlapping particles apart
#'
#' This force pushes overlapping particles apart by assigning a radius to each
#' particle, treating them as circles, and searches for overlaps through an
#' optimised quad tree algorithm.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `strength` : A dampening of the repulsion between overlapping circles. This
#'   allows the force to iterate towards the optimal solution through iterative
#'   relaxation. Should be a number between 0 and 1. Defaults to 0.7
#' - `radius` : The radius of each particle. Defaults to 1
#' - `n_iter` : The number of iterations to perform in the iterative relaxation.
#'   Defaults to 1.
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
collision_force <- structure(list(
  strength = NULL,
  radius = NULL,
  radius_quo = NULL,
  n_iter = NULL
), class = c('collision_force', 'force'))
#' @export
print.collision_force <- function(x, ...) {
  cat('Collision Force:\n')
  cat('* A force that repels overlapping circles away from each other\n')
}
#' @importFrom tidygraph as_tibble
#' @importFrom rlang enquo eval_tidy %||%
train_force.collision_force <- function(force, particles, radius = NULL, strength = NULL, n_iter = 1, ...) {
  force <- NextMethod()
  nodes <- as_tibble(particles, active = 'nodes')
  force$radius_quo <- enquo(radius)
  radius <- eval_tidy(force$radius_quo, nodes) %||% 1
  force$radius <- rep(radius, length.out = nrow(nodes))
  force$strength <- strength %||% 0.7
  force$n_iter <- n_iter %||% 1
  force
}
apply_force.collision_force <- function(force, particles, pos, vel, alpha, ...) {
  for (i in seq_len(force$n_iter)) {
    vel <- vel + collision(pos, vel, force$radius, force$strength)
  }
  list(position = pos, velocity = vel)
}
