#' Fixes particles to a specific velocity range
#'
#' This constraint puts bounds on the magnitude of velocity a particle can have.
#' Particles where either end of the bound is NA ignores the constraint. If a
#' particle with no velocity is forced to have a velocity the direction will be
#' random.
#'
#' @section Training parameters:
#' The following parameters defines the training of the constraint and can be
#' passed along a call to [impose()]
#'
#' - `v` : The velocity allowed for the particle.
#' - `vmin` : The lowest permissable velocity. If `NULL` then `v` will be used.
#' - `vmax` : The highest permissable velocity. If `NULL` then `v` will be used.
#'
#' @family constraints
#' @usage NULL
#' @format NULL
#' @export
velocity_constraint <- structure(list(
  vmin = NULL,
  vmax = NULL,
  v_quo = NULL,
  vmin_quo = NULL,
  vmax_quo = NULL
), class = c('velocity_constraint', 'constraint'))
#' @export
print.velocity_constraint <- function(x, ...) {
  cat('Velocity Constraint:\n')
  cat('* A constraint that fixes particle velocity to a given range\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
train_constraint.velocity_constraint <- function(constraint, particles, v = NULL, vmin = NULL, vmax = NULL, ...) {
  constraint <- NextMethod()
  nodes <- as_tibble(particles, active = 'nodes')
  constraint$v_quo <- enquo(v)
  constraint$vmin_quo <- enquo(vmin)
  constraint$vmax_quo <- enquo(vmax)
  v <- eval_tidy(constraint$v_quo, nodes)
  vmin <- eval_tidy(constraint$vmin_quo, nodes) %||% v %||% NA
  vmax <- eval_tidy(constraint$vmax_quo, nodes) %||% v %||% NA
  constraint$vmin <- rep(vmin, length.out = nrow(nodes))
  constraint$vmax <- rep(vmax, length.out = nrow(nodes))
  constraint
}
apply_constraint.velocity_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  vel_tmp <- vel
  zeroes <- rowSums(vel_tmp) == 0
  vel_tmp[zeroes, ] <- matrix(runif(sum(zeroes)*2, -0.5, 0.5)*1e-6, ncol = 2)
  vel_strength <- sqrt(rowSums(vel^2))
  min_constrained <- !(is.na(constraint$vmin) | vel_strength > constraint$vmin)
  max_constrained <- !(is.na(constraint$vmax) | vel_strength < constraint$vmax)
  problems <- vel_strength == 0 & (min_constrained || max_constrained)
  vel_strength[problems] <- sqrt(rowSums(vel_tmp[problems, ]^2))
  vel[problems, ] <- vel_tmp[problems, , drop = FALSE]
  vel_modmin <- ifelse(min_constrained, constraint$vmin/vel_strength, 1)
  vel_modmax <- ifelse(max_constrained, constraint$vmax/vel_strength, 1)
  vel <- vel * cbind(vel_modmin, vel_modmin) * cbind(vel_modmax, vel_modmax)
  list(position = pos, velocity = vel)
}
