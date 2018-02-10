#' Attract and trap particles within polygons
#'
#' This force creates a trap based on any type of polygon that attracts
#' particles as long as they are outside the polygon, while leaving particles
#' inside the polygon unaffected. The trap as such has no walls and particles
#' are allowed to leave it, but they will be pulled back as soon as they exits
#' the polygon.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `polygon` : A two column matrix giving the corners of the polygon, or a
#'   list of matrices to use multiple polygons. If multiple polygons are
#'   overlapping it is considered a hole.
#' - `strength` : The attractive force of the trap. Particles are attractet
#'   towards the closest part of the polygon, rather than the center, and the
#'   attraction is stronger for particles moving away from the polygon than for
#'   those moving towards it.
#' - `min_dist` : A lower distance threshold below which the strength is not
#'   increased. The attraction of the trap falls of with the square of the
#'   distance to the particle, so particles close by can get an enormous
#'   attraction unless this threshold is set (so much that the shoot out of the
#'   other side of the trap).
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
trap_force <- structure(list(
  polygon = NULL,
  polygon_closed = NULL,
  strength = NULL,
  strength_quo = NULL,
  min_dist = NULL,
  distance_falloff = NULL
), class = c('trap_force', 'force'))
#' @export
print.trap_force <- function(x, ...) {
  cat('Trap Force:\n')
  cat('* A force that attracts particles into polygons\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
train_force.trap_force <- function(force, particles, polygon = NULL, strength = NULL, min_dist = 1, distance_falloff = NULL, ...) {
  force <- NextMethod()
  if (is.matrix(polygon)) polygon <- list(polygon)
  if (!all(vapply(polygon, inherits, logical(1), 'matrix'))) {
    stop('Polygon must be provided as a matrix or a list of matrices', call. = FALSE)
  }
  if (!all(vapply(polygon, ncol, integer(1)) == 2)) {
    stop('Polygon matrices must contain two columns', call. = FALSE)
  }
  force$min_dist = min_dist
  force$polygon <- polygon
  polygon_closed = do.call(rbind, lapply(polygon, rbind, matrix(NA, ncol = 2)))
  force$polygon_closed <- polygon_closed[-nrow(polygon_closed), ]
  nodes <- as_tibble(particles, active = 'nodes')
  force$strength_quo <- enquo(strength)
  strength <- eval_tidy(force$strength_quo, nodes) %||% 1
  force$strength <- rep(strength, length.out = nrow(nodes))
  force$distance_falloff_quo <- enquo(distance_falloff)
  distance_falloff <- eval_tidy(force$distance_falloff_quo, nodes) %||% 2
  force$distance_falloff <- rep(distance_falloff, length.out = nrow(nodes))
  force
}
#' @importFrom rlang quos eval_tidy
#' @importFrom digest digest
retrain_force.trap_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  force$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
  force <- update_quo(force, 'strength', dots, nodes, new_particles, 1)
  force <- update_quo(force, 'distance_falloff', dots, nodes, new_particles, 2)
  force <- update_unquo(force, 'min_dist', dots)
  if ('polygon' %in% names(dots)) {
    polygon <- eval_tidy(dots$polygon)
    if (is.matrix(polygon)) polygon <- list(polygon)
    if (!all(vapply(polygon, inherits, logical(1), 'matrix'))) {
      stop('Polygon must be provided as a matrix or a list of matrices', call. = FALSE)
    }
    if (!all(vapply(polygon, ncol, integer(1)) == 2)) {
      stop('Polygon matrices must contain two columns', call. = FALSE)
    }
    force$polygon <- polygon
    polygon_closed = do.call(rbind, lapply(polygon, rbind, matrix(NA, ncol = 2)))
    force$polygon_closed <- polygon_closed[-nrow(polygon_closed), ]
  }
  force
}
#' @importFrom mgcv in.out
apply_force.trap_force <- function(force, particles, pos, vel, alpha, ...) {
  particle_outside <- !in.out(force$polygon_closed, pos)
  if (any(particle_outside)) {
    particle_sub <- pos[particle_outside, , drop = FALSE]
    vel_sub <- vel[particle_outside, , drop = FALSE]
    attraction <- points_to_path(particle_sub, force$polygon, TRUE)
    direction <- (attraction$projection - particle_sub) / cbind(attraction$distance, attraction$distance)
    distance <- ifelse(
      attraction$distance < force$min_dist,
      force$min_dist,
      attraction$distance
    )
    angle_mod <- angle_diff(vel_sub, direction) / pi + 1
    full_mod <- angle_mod * force$strength[particle_outside] / distance^force$distance_falloff[particle_outside]
    vel[particle_outside, ] <- vel_sub + direction * cbind(full_mod, full_mod)
  }
  list(position = pos, velocity = vel)
}
