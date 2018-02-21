#' Apply a map to particles
#'
#' In mathematics, maps are a functions that translates its input into new
#' values. In the context of particles a map is a translation function that
#' translates the current particle positions to a new one and can e.g. be a
#' strange attractor such as the Lorentz attractor. The particles packages comes
#' with a range of map constructors to facilitate experimentation.
#'
#' Normally a map has no notion of velocity — it simply translates positions. In
#' particles it is possible to decide whether positions should be modified
#' directly or whether the translation magnitude should be added to the velocity
#' verlet using the `fixed` parameter.
#'
#' @section Training parameters:
#' The following parameters defines the training of the force and can be passed
#' along a call to [wield()]
#'
#' - `map` : A function that accepts the particle position matrix and returns
#'   the new positions in the same format.
#' - `fixed` : Logical. Should position be modified directly (`TRUE`) or should
#'   the translation be added to the velocity verlet (`FALSE`)
#'
#' @family forces
#' @usage NULL
#' @format NULL
#' @export
map_force <- structure(list(
  map = NULL,
  fixed = NULL
), class = c('map_force', 'force'))
#' @export
print.map_force <- function(x, ...) {
  cat('Map Force:\n')
  cat('* A force that applies an evolution function to the particles\n')
}
#' @export
train_force.map_force <- function(force, particles, map, fixed = FALSE, ...) {
  force <- NextMethod()
  force$map <- map
  force$fixed <- fixed
  force
}
#' @importFrom rlang quos eval_tidy
#' @importFrom digest digest
#' @export
retrain_force.map_force <- function(force, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != force$particle_hash
  force$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  force <- update_quo(force, 'include', dots, nodes, new_particles, TRUE)
  force <- update_unquo(force, 'map', ...)
  force <- update_unquo(force, 'fixed', ...)
  force
}
#' @export
apply_force.map_force <- function(force, particles, pos, vel, alpha, ...) {
  new_pos <- force$map(pos)
  if (force$fixed) {
    pos <- new_pos
  } else {
    vel <- vel + new_pos - pos
  }
  list(position = pos, velocity = vel)
}
