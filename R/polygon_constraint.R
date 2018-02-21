#' Fixes particles TODO DOC
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
polygon_constraint <- structure(list(
  polygon = NULL,
  polygon_closed = NULL
), class = c('polygon_constraint', 'constraint'))
#' @export
print.polygon_constraint <- function(x, ...) {
  cat('Polygon Constraint:\n')
  cat('* A constraint that fixes particle positions inside a polygon. Particles inside the polygon are unaffected, while those outside will be moved to the closest area inside the polygon\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
#' @export
train_constraint.polygon_constraint <- function(constraint, particles, polygon, ...) {
  constraint <- NextMethod()
  if (is.matrix(polygon)) polygon <- list(polygon)
  if (!all(vapply(polygon, inherits, logical(1), 'matrix'))) {
    stop('Polygon must be provided as a matrix or a list of matrices', call. = FALSE)
  }
  if (!all(vapply(polygon, ncol, integer(1)) == 2)) {
    stop('Polygon matrices must contain two columns', call. = FALSE)
  }
  constraint$polygon <- polygon
  polygon_closed = do.call(rbind, lapply(polygon, rbind, matrix(NA, ncol = 2)))
  constraint$polygon_closed <- polygon_closed[-nrow(polygon_closed), ]
  constraint
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_constraint.polygon_constraint <- function(constraint, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != constraint$particle_hash
  constraint$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  constraint <- update_quo(constraint, 'include', dots, nodes, new_particles, TRUE)
  if ('polygon' %in% names(dots)) {
    polygon <- eval_tidy(dots$polygon)
    if (is.matrix(polygon)) polygon <- list(polygon)
    if (!all(vapply(polygon, inherits, logical(1), 'matrix'))) {
      stop('Polygon must be provided as a matrix or a list of matrices', call. = FALSE)
    }
    if (!all(vapply(polygon, ncol, integer(1)) == 2)) {
      stop('Polygon matrices must contain two columns', call. = FALSE)
    }
    constraint$polygon <- polygon
    polygon_closed = do.call(rbind, lapply(polygon, rbind, matrix(NA, ncol = 2)))
    constraint$polygon_closed <- polygon_closed[-nrow(polygon_closed), ]
  }
  constraint
}
#' @export
apply_constraint.polygon_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  particle_outside <- !in.out(constraint$polygon_closed, pos)
  if (any(particle_outside)) {
    particle_sub <- pos[particle_outside, , drop = FALSE]
    pos[particle_outside, ] <- points_to_path(particle_sub, constraint$polygon, TRUE)$projection
    vel[particle_outside, ] <- 0
  }
  list(position = pos, velocity = vel)
}
