is.force <- function(x) inherits(x, 'force')
train_force <- function(force, particles, ...) {
  UseMethod('train_force')
}
#' @export
train_force.default <- function(force, particles, ...) {
  stop('The provided object is not a force', call. = FALSE)
}
#' @importFrom digest digest
#' @export
train_force.force <- function(force, particles, include, ...) {
  force$include_quo <- include
  nodes <- as_tibble(particles, active = 'nodes')
  include <- eval_tidy(force$include_quo, nodes) %||% TRUE
  force$include <- rep(include, length.out = nrow(nodes))
  force$particle_hash <- digest(particles)
  force
}
retrain_force <- function(force, particles, ...) {
  UseMethod('retrain_force')
}
#' @export
retrain_force.default <- function(force, particles, ...) {
  stop('The provided object is not a force', call. = FALSE)
}
#' @export
retrain_force.force <- function(force, particles, ...) {
  stop('The provided force does not have a retrain method', call. = FALSE)
}
apply_force <- function(force, particles, pos, vel, alpha, ...) {
  UseMethod('apply_force')
}
#' @export
apply_force.default <- function(force, particles, pos, vel, alpha, ...) {
  stop('The provided object is not a force', call. = FALSE)
}
#' @export
apply_force.force <- function(force, particles, pos, vel, alpha, ...) {
  stop('The provided force does not implement an apply method', call. = FALSE)
}

# HELPERS -----------------------------------------------------------------

#' @importFrom rlang eval_tidy
update_quo <- function(force, arg, dots, nodes, new, def) {
  if (arg %in% names(dots)) {
    force[[paste0(arg, '_quo')]] <- dots[[arg]]
  }
  if (arg %in% names(dots) || new) {
    new_val <- eval_tidy(force[[paste0(arg, '_quo')]], nodes) %||% def
    force[[arg]] <- rep(new_val, length.out = nrow(nodes))
  }
  force
}
#' @importFrom rlang eval_tidy
update_unquo <- function(force, arg, dots) {
  if (arg %in% names(dots)) {
    force[[arg]] <- eval_tidy(dots[[arg]])
  }
  force
}
