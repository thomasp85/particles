#' Fixes particles to a horizontal position
#'
#' This constraint simply prevents particles from moving in the x direction. For
#' particles where the constraint evaluates to `NA` this constraint is ignored.
#' If the constraint is enforced the velocity in the x direction will be set to
#' `0`.
#'
#' @section Training parameters:
#' The following parameters defines the training of the constraint and can be
#' passed along a call to [impose()]
#'
#' - `x` : The position on the x-axis to fix to.
#' - `xmin` : The lowest permissable x-value. If `NULL` then `x` will be used.
#' - `xmax` : The highest permissable x-value. If `NULL` then `x` will be used.
#'
#' @family constraints
#' @usage NULL
#' @format NULL
#' @export
x_constraint <- structure(list(
  xmin = NULL,
  xmax = NULL,
  x_quo = NULL,
  xmin_quo = NULL,
  xmax_quo = NULL
), class = c('x_constraint', 'constraint'))
#' @export
print.x_constraint <- function(x, ...) {
  cat('X Constraint:\n')
  cat('* A constraint that fixes particle positions in the x direction and sets their velocity in the x direction to 0\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
#' @export
train_constraint.x_constraint <- function(constraint, particles, x = NULL, xmin = NULL, xmax = NULL, ...) {
  constraint <- NextMethod()
  nodes <- as_tibble(particles, active = 'nodes')
  constraint$x_quo <- enquo(x)
  constraint$xmin_quo <- enquo(xmin)
  constraint$xmax_quo <- enquo(xmax)
  x <- eval_tidy(constraint$x_quo, nodes)
  xmin <- eval_tidy(constraint$xmin_quo, nodes) %||% x %||% NA
  xmax <- eval_tidy(constraint$xmax_quo, nodes) %||% x %||% NA
  constraint$xmin <- rep(xmin, length.out = nrow(nodes))
  constraint$xmax <- rep(xmax, length.out = nrow(nodes))
  constraint
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_constraint.x_constraint <- function(constraint, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != constraint$particle_hash
  constraint$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  constraint <- update_quo(constraint, 'include', dots, nodes, new_particles, TRUE)
  new_x <- 'x' %in% names(dots)
  if (new_x) constraint$x_quo <- dots$x
  x <- eval_tidy(constraint$x_quo, nodes)
  constraint <- update_quo(constraint, 'xmin', dots, nodes, new_particles || new_x, x %||% NA)
  constraint <- update_quo(constraint, 'xmax', dots, nodes, new_particles || new_x, x %||% NA)
  constraint
}
#' @export
apply_constraint.x_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  min_constrained <- !(is.na(constraint$xmin) | pos[, 1] + vel[, 1] > constraint$xmin)
  max_constrained <- !(is.na(constraint$xmax) | pos[, 1] + vel[, 1] < constraint$xmax)
  pos[, 1] <- ifelse(min_constrained, constraint$xmin, pos[, 1])
  pos[, 1] <- ifelse(max_constrained, constraint$xmax, pos[, 1])
  vel[, 1] <- ifelse(min_constrained | max_constrained, 0, vel[, 1])
  list(position = pos, velocity = vel)
}
