#' Limit particle position to be along a path or outline
#'
#' This constraint repositions particles to their closest point along a given
#' path and sets their velocity to zero.
#'
#' @section Training parameters:
#' The following parameters defines the training of the constraint and can be
#' passed along a call to [impose()]
#'
#' - `path` : A two column matrix giving the path, or a
#'   list of matrices to use multiple disconnected paths.
#' - `closed` : Should the path close on itself. Defaults to `FALSE`
#'
#' @family constraints
#' @usage NULL
#' @format NULL
#' @export
path_constraint <- structure(list(
  path = NULL,
  closed = NULL
), class = c('y_constraint', 'constraint'))
#' @export
print.path_constraint <- function(x, ...) {
  cat('Path Constraint:\n')
  cat('* A constraint that forces particles to be positioned along a path\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
#' @export
train_constraint.path_constraint <- function(constraint, particles, path = NULL, closed = FALSE, ...) {
  constraint <- NextMethod()
  if (is.matrix(path)) path <- list(path)
  if (!all(vapply(path, inherits, logical(1), 'matrix'))) {
    stop('Path must be provided as a matrix or a list of matrices', call. = FALSE)
  }
  if (!all(vapply(path, ncol, integer(1)) == 2)) {
    stop('Path matrices must contain two columns', call. = FALSE)
  }
  constraint$path <- path
  constraint$closed <- closed
  constraint
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @export
retrain_constraint.path_constraint <- function(constraint, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != constraint$particle_hash
  constraint$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  constraint <- update_quo(constraint, 'include', dots, nodes, new_particles, TRUE)
  if ('path' %in% names(dots)) {
    path <- eval_tidy(dots$path)
    if (is.matrix(path)) path <- list(path)
    if (!all(vapply(path, inherits, logical(1), 'matrix'))) {
      stop('Path must be provided as a matrix or a list of matrices', call. = FALSE)
    }
    if (!all(vapply(path, ncol, integer(1)) == 2)) {
      stop('Path matrices must contain two columns', call. = FALSE)
    }
    constraint$path <- path
  }
  constraint <- update_unquo(constraint, 'closed', dots)
  constraint
}
#' @export
apply_constraint.path_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  pos <- points_to_path(pos, constraint$path, constraint$closed)
  vel[] <- 0
  list(position = pos, velocity = vel)
}
