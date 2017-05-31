#' Apply the mean velocity of all the neighbors to a particle
#'
#' This force takes the mean of all the neighbors (in the graph sense) of a
#' particle (and optionally itself) and applies it to itself.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `include_self` : Should the velocity of itself be included in the mean
#'   calculation
#' - `mode` : How should neighbors be found? `'all'` uses all edges. `'out'`
#'   only uses outbound edges, and `'in'` only uses inbound edges. Ignored for
#'   undirected particle graphs
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
manybody_force <- structure(list(
  strength = NULL,
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
train_force.manybody_force <- function(force, particles, strength = NULL, theta = NULL, min_dist = NULL, max_dist = NULL, ...) {
  nodes <- as_tibble(particles, active = 'nodes')
  force$strength_quo <- enquo(strength)
  strength <- eval_tidy(force$strength, nodes) %||% -30
  force$strength <- rep(strength, length.out = nrow(nodes))
  force$theta <- theta %||% 0.9
  force$min_dist <- min_dist %||% 0
  force$max_dist <- max_dist %||% -1
  force
}
apply_force.manybody_force <- function(force, particles, pos, vel, alpha, ...) {
  vel_mod <- nbody(pos, force$strength, force$theta, force$min_dist, force$max_dist, alpha)
  vel <- vel + vel_mod
  list(position = pos, velocity = vel)
}
