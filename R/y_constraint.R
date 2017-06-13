#' Fixes particles to a vertical position
#'
#' This constraint simply prevents particles from moving in the y direction. For
#' particles where the constraint evaluates to `NA` this constraint is ignored.
#' If the constraint is enforced the velocity in the y direction will be set to
#' `0`.
#'
#' @section Training parameters:
#' The following parameters defines the training of the constraint and can be
#' passed along a call to [impose()]
#'
#' - `y` : The position on the y-axis to fix to.
#' - `ymin` : The lowest permissable y-value. If `NULL` then `y` will be used.
#' - `ymax` : The highest permissable y-value. If `NULL` then `y` will be used.
#'
#' @family constraints
#' @usage NULL
#' @format NULL
#' @export
y_constraint <- structure(list(
  ymin = NULL,
  ymax = NULL,
  y_quo = NULL,
  ymin_quo = NULL,
  ymax_quo = NULL
), class = c('y_constraint', 'constraint'))
#' @export
print.y_constraint <- function(x, ...) {
  cat('Y Constraint:\n')
  cat('* A constraint that fixes particle positions in the y direction and sets their velocity in the y direction to 0\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
train_constraint.y_constraint <- function(constraint, particles, y = NULL, ymin = NULL, ymax = NULL, ...) {
  constraint <- NextMethod()
  nodes <- as_tibble(particles, active = 'nodes')
  constraint$y_quo <- enquo(y)
  constraint$ymin_quo <- enquo(ymin)
  constraint$ymax_quo <- enquo(ymax)
  y <- eval_tidy(constraint$y_quo, nodes)
  ymin <- eval_tidy(constraint$ymin_quo, nodes) %||% y %||% NA
  ymax <- eval_tidy(constraint$ymax_quo, nodes) %||% y %||% NA
  constraint$ymin <- rep(ymin, length.out = nrow(nodes))
  constraint$ymax <- rep(ymax, length.out = nrow(nodes))
  constraint
}
#' @importFrom rlang quos
#' @importFrom digest digest
retrain_constraint.y_constraint <- function(constraint, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  constraint$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  constraint <- update_quo(constraint, 'include', dots, nodes, new_particles, TRUE)
  new_y <- 'y' %in% names(dots)
  if (new_y) constraint$y_quo <- dots$y
  y <- eval_tidy(constraint$y_quo, nodes)
  constraint <- update_quo(constraint, 'ymin', dots, nodes, new_particles || new_y, y %||% NA)
  constraint <- update_quo(constraint, 'ymax', dots, nodes, new_particles || new_y, y %||% NA)
  constraint
}
apply_constraint.y_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  min_constrained <- !(is.na(constraint$ymin) | pos[, 2] + vel[, 2] > constraint$ymin)
  max_constrained <- !(is.na(constraint$ymax) | pos[, 2] + vel[, 2] < constraint$ymax)
  pos[, 2] <- ifelse(min_constrained, constraint$ymin, pos[, 2])
  pos[, 2] <- ifelse(max_constrained, constraint$ymax, pos[, 2])
  vel[, 2] <- ifelse(min_constrained | max_constrained, 0, vel[, 2])
  list(position = pos, velocity = vel)
}
