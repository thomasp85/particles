#' Reposition particles outside a canvas so they wrap around
#'
#' This constraint keeps particles inside of a defined area by positioning
#' exiting particles on the other side of the area. In effect this makes
#' particles that moves outside the upper bound reenter at the lower bound and
#' vice versa.
#'
#' @section Training parameters:
#' The following parameters defines the training of the constraint and can be
#' passed along a call to [impose()]
#'
#' - `xlim` : The left and right bound of the area
#' - `ylim` : The upper and lower bound of the area
#'
#' @family constraints
#' @usage NULL
#' @format NULL
#' @export
infinity_constraint <- structure(list(
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL
), class = c('infinity_constraint', 'constraint'))
#' @export
print.infinity_constraint <- function(x, ...) {
  cat('Infinity Constraint:\n')
  cat('* A constraint forces particles to be inside a canvas by wrapping them around it.\n')
}
#' @export
train_constraint.infinity_constraint <- function(constraint, particles, xlim = c(-5, 5), ylim = xlim, ...) {
  constraint <- NextMethod()
  constraint$xmin <- xlim[1]
  constraint$xmax <- xlim[2]
  constraint$ymin <- ylim[1]
  constraint$ymax <- ylim[2]
  constraint
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_constraint.infinity_constraint <- function(constraint, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != constraint$particle_hash
  constraint$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  constraint <- update_quo(constraint, 'include', dots, nodes, new_particles, TRUE)
  if ('xlim' %in% names(dots)) {
    xlim <- eval_tidy(dots$xlim)
    constraint$xmin <- xlim[1]
    constraint$xmax <- xlim[2]
  }
  if ('ylim' %in% names(dots)) {
    ylim <- eval_tidy(dots$ylim)
    constraint$ymin <- ylim[1]
    constraint$ymax <- ylim[2]
  }
  constraint
}
#' @export
apply_constraint.infinity_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  next_pos <- pos + vel
  l <- next_pos[, 1] < constraint$xmin
  r <- next_pos[, 1] > constraint$xmax
  b <- next_pos[, 2] < constraint$ymin
  t <- next_pos[, 2] > constraint$ymax
  next_pos[l, 1] <- constraint$xmax - (abs(constraint$xmin - next_pos[l, 1]) %% (constraint$xmax - constraint$xmin))
  next_pos[r, 1] <- constraint$xmin + (abs(constraint$xmax - next_pos[r, 1]) %% (constraint$xmax - constraint$xmin))
  next_pos[b, 2] <- constraint$ymax - (abs(constraint$ymin - next_pos[b, 2]) %% (constraint$ymax - constraint$ymin))
  next_pos[t, 2] <- constraint$ymin + (abs(constraint$ymax - next_pos[t, 2]) %% (constraint$ymax - constraint$ymin))
  pos <- next_pos - vel
  list(position = pos, velocity = vel)
}
