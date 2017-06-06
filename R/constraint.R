is.constraint <- function(x) inherits(x, 'constraint')
train_constraint <- function(constraint, particles, ...) {
  UseMethod('train_constraint')
}
train_constraint.default <- function(constraint, particles, ...) {
  stop('The provided object is not a constraint', call. = FALSE)
}
train_constraint.constraint <- function(constraint, particles, ...) {
  stop('The provided constraint does not implement a training method', call. = FALSE)
}
apply_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  UseMethod('apply_constraint')
}
apply_constraint.default <- function(constraint, particles, pos, vel, alpha, ...) {
  stop('The provided object is not a constraint', call. = FALSE)
}
apply_constraint.constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  stop('The provided constraint does not implement an apply method', call. = FALSE)
}
