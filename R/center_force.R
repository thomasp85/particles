#' Center all particles around the origin without affecting velocity
#'
#' This force repositions the particles at each generation so they are centered
#' around (0,0). It does not affect the velocity of the particles and are thus
#' mainly a guard against the whole body of particles drifting off.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `x` : The x position to center around (*tidy eval*)
#' - `y` : The y position to center around (*tidy eval*)
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
center_force <- structure(list(
  x = NULL,
  x_quo = NULL,
  y = NULL,
  y_quo = NULL,
  center_groups = NULL
), class = c('center_force', 'force'))
#' @export
print.center_force <- function(x, ...) {
  cat('Center Force:\n')
  cat('* A force that keeps all particles centered around the origin\n')
}
#' @export
train_force.center_force <- function(force, particles, x = NULL, y = NULL, ...) {
  force <- NextMethod()
  force$x_quo <- enquo(x)
  force$y_quo <- enquo(y)
  nodes <- as_tibble(particles, active = 'nodes')
  x <- eval_tidy(force$x_quo, nodes) %||% 0
  y <- eval_tidy(force$y_quo, nodes) %||% 0
  force$x <- rep(x, length.out = nrow(nodes))
  force$y <- rep(y, length.out = nrow(nodes))
  force$center_groups <- split(seq_len(nrow(nodes)), paste0(force$x, '-', force$y))
  force
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_force.center_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  if (new_particles) {
    force$particle_hash <- particle_hash
    nodes <- as_tibble(particles, active = 'nodes')
    force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
    force <- update_quo(force, 'x', dots, nodes, new_particles, 0)
    force <- update_quo(force, 'y', dots, nodes, new_particles, 0)
    force$center_groups <- split(seq_len(nrow(nodes)), paste0(force$x, '-', force$y))
  }
  force
}
#' @export
apply_force.center_force <- function(force, particles, pos, vel, alpha, ...) {
  for (i in force$center_groups) {
    adjust <- colMeans(pos[i, , drop = FALSE]) - c(force$x[i[1]], force$y[i[1]])
    center <- matrix(adjust, nrow = length(i), ncol = ncol(pos), byrow = TRUE)
    pos[i, ] <- pos[i, , drop = FALSE] - center
  }
  list(position = pos, velocity = vel)
}
