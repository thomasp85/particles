is.force <- function(x) inherits(x, 'force')
train_force <- function(force, particles, ...) {
  UseMethod('train_force')
}
train_force.default <- function(force, particles, ...) {
  stop('The provided object is not a force', call. = FALSE)
}
train_force.force <- function(force, particles, include, ...) {
  force$include_quo <- include
  nodes <- as_tibble(particles, active = 'nodes')
  include <- eval_tidy(force$include_quo, nodes) %||% TRUE
  force$include <- rep(include, length.out = nrow(nodes))
  force
}
apply_force <- function(force, particles, pos, vel, alpha, ...) {
  UseMethod('apply_force')
}
apply_force.default <- function(force, particles, pos, vel, alpha, ...) {
  stop('The provided object is not a force', call. = FALSE)
}
apply_force.force <- function(force, particles, pos, vel, alpha, ...) {
  stop('The provided force does not implement an apply method', call. = FALSE)
}
