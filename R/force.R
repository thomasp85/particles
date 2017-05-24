is.force <- function(x) inherits(x, 'force')
train_force <- function(force, particles, ...) {
  UseMethod('train_force')
}
train_force.default <- function(force, particles, ...) {
  stop('The provided object is not a force', call. = FALSE)
}
train_force.force <- function(force, particles, ...) {
  stop('The provided force does not implement a training method', call. = FALSE)
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
