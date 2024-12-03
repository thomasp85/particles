is.constraint <- function(x) inherits(x, 'constraint')
train_constraint <- function(constraint, particles, ...) {
  UseMethod('train_constraint')
}
#' @export
train_constraint.default <- function(constraint, particles, ...) {
  stop('The provided object is not a constraint', call. = FALSE)
}
#' @importFrom digest digest
#' @export
train_constraint.constraint <- function(constraint, particles, include, ...) {
  constraint$include_quo <- include
  nodes <- as_tibble(particles, active = 'nodes')
  include <- eval_tidy(constraint$include_quo, nodes) %||% TRUE
  constraint$include <- rep(include, length.out = nrow(nodes))
  constraint$particle_hash <- digest(particles)
  constraint
}
retrain_constraint <- function(constraint, particles, ...) {
  UseMethod('retrain_constraint')
}
#' @export
retrain_constraint.default <- function(constraint, particles, ...) {
  stop('The provided object is not a constraint', call. = FALSE)
}
#' @export
retrain_constraint.constraint <- function(constraint, particles, ...) {
  stop('The provided constraint does not have a retrain method', call. = FALSE)
}
apply_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  UseMethod('apply_constraint')
}
#' @export
apply_constraint.default <- function(constraint, particles, pos, vel, alpha, ...) {
  stop('The provided object is not a constraint', call. = FALSE)
}
#' @export
apply_constraint.constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  stop('The provided constraint does not implement an apply method', call. = FALSE)
}
