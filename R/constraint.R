is.constraint <- function(x) inherits(x, 'constraint')
train_constraint <- function(constraint, particles, ...) {
  UseMethod('train_constraint')
}
train_constraint.default <- function(constraint, particles, ...) {
  stop('The provided object is not a constraint', call. = FALSE)
}
#' @importFrom digest digest
train_constraint.constraint <- function(constraint, particles, include, ...) {
  constraint$include_quo <- enquo(include)
  nodes <- as_tibble(particles, active = 'nodes')
  include <- eval_tidy(constraint$include_quo, nodes) %||% TRUE
  constraint$include <- rep(include, length.out = nrow(nodes))
  constraint$particle_hash <- digest(particles)
  constraint
}
retrain_constraint <- function(constraint, particles, ...) {
  UseMethod('retrain_constraint')
}
retrain_constraint.default <- function(constraint, particles, ...) {
  stop('The provided object is not a constraint', call. = FALSE)
}
retrain_constraint.constraint <- function(constraint, particles, ...) {
  stop('The provided constraint does not have a retrain method', call. = FALSE)
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
