#' Modify the particles in a simulation
#'
#' The particles that are modelled in a simulation are encoded as a `tbl_graph`,
#' giving support for the particles as well as their interactions (nodes and
#' edges in graph parlor). A simulation supports a subset of the tidygraph/dplyr
#' verbs in order to allow modification of the particles after they have been
#' included in the simulation. In general it is possible to add and remove
#' particles and interactions as well as modify the metadata associated with
#' them. The API follows the tidygraph API where `activate()` is used to select
#' either particles or interactions and subsequent operations are thus related
#' to the last activated datatype. The simulation is automatically retrained
#' after modifying the state of the particles and their interactions.
#'
#' @param .data A simulation object
#'
#' @param particles A `tbl_graph` or an object coercible to one
#'
#' @param ... Parameters passed on to the main verbs in tidygraph/dplyr
#'
#' @param interactions A data.frame of interactions/edges to add along with the
#' particles
#'
#' @param setup A function to calculate the starting conditions for the
#' particles. It receives all particles with the current position and
#' velocity encoded in the `x`, `y`, `x_vel`, and `y_vel` columns. New particle
#' will have NA. The function must return a position and velocity for all
#' particles even though the values for the current particles will be discarded.
#' If NULL it will use the genesis function used when creating the simulation.
#'
#' @return A simulation object
#'
#' @seealso [dplyr::mutate()], [dplyr::mutate_at()], [dplyr::mutate_all()],
#' [dplyr::filter()], [dplyr::slice()], [tidygraph::activate()],
#' [tidygraph::bind_nodes()], [tidygraph::bind_edges()]
#'
#' @name simulation_modification
#' @rdname simulation_modification
NULL

#' @rdname simulation_modification
#' @importFrom tidygraph bind_nodes bind_edges as_tbl_graph
#' @importFrom igraph gorder
#' @export
add_particles <- function(.data, ..., interactions = NULL, setup = NULL) {
  stopifnot(is.simulation(.data))
  n_particles <- gorder(particles(.data))
  setup <- setup %||% universe(.data)$genesis
  particles(.data) <- bind_nodes(particles(.data), ...)
  particles(.data) <- bind_edges(particles(.data), interactions)
  genesis <- setup(as_tbl_graph(.data), universe(.data)$parameters)
  .data <- set_position(.data, rbind(position(.data), genesis$position[-seq_len(n_particles), , drop = FALSE]))
  .data <- set_velocity(.data, rbind(velocity(.data), genesis$velocity[-seq_len(n_particles), , drop = FALSE]))
  retrain(.data)
}
#' @rdname simulation_modification
#' @importFrom tidygraph as_tbl_graph
#' @export
replace_particles <- function(.data, particles, setup = NULL) {
  stopifnot(is.simulation(.data))
  particles <- as_tbl_graph(particles)
  setup <- setup %||% universe(.data)$genesis
  particles(.data) <- particles
  genesis <- setup(particles, universe(.data)$parameters)
  .data <- set_position(.data, genesis$position)
  .data <- set_velocity(.data, genesis$velocity)
  retrain(.data)
}
#' @rdname simulation_modification
#' @importFrom tidygraph bind_edges
add_interaction <- function(.data, ...) {
  stopifnot(is.simulation(.data))
  particles(.data) <- bind_edges(particles(.data), ...)
  retrain(.data)
}
#' @importFrom tidygraph filter active
#' @export
filter.simulation <- function(.data, ...) {
  par <- particles(.data)
  par <- mutate(par, .particle_index = seq_len(nrow(par)))
  par <- filter(par, ...)
  remain <- as_tibble(par)$.particle_index
  particles(.data) <- mutate(par, .particle_index = NULL)
  if (active(par) == 'nodes') {
    position(.data) <- position(.data)[remain, , drop = FALSE]
    velocity(.data) <- velocity(.data)[remain, , drop = FALSE]
  }
  retrain(.data)
}
#' @importFrom tidygraph filter
#' @export
tidygraph::filter
#' @importFrom tidygraph slice active
#' @export
slice.simulation <- function(.data, ...) {
  par <- particles(.data)
  par <- mutate(par, .particle_index = seq_len(nrow(par)))
  par <- slice(par, ...)
  remain <- as_tibble(par)$.particle_index
  particles(.data) <- mutate(par, .particle_index = NULL)
  if (active(par) == 'nodes') {
    position(.data) <- position(.data)[remain, , drop = FALSE]
    velocity(.data) <- velocity(.data)[remain, , drop = FALSE]
  }
  retrain(.data)
}
#' @importFrom tidygraph slice
#' @export
tidygraph::slice
#' @importFrom tidygraph mutate
#' @export
mutate.simulation <- function(.data, ...) {
  particles(.data) <- mutate(particles(.data), ...)
  retrain(.data)
}
#' @importFrom tidygraph mutate
#' @export
tidygraph::mutate
#' @importFrom tidygraph mutate_at
#' @export
tidygraph::mutate_at
#' @importFrom tidygraph mutate_all
#' @export
tidygraph::mutate_all
#' @importFrom rlang quo_text enquo
#' @importFrom tidygraph activate
#' @export
activate.simulation <- function(.data, what) {
  what <- quo_text(enquo(what))
  if (what %in% c('particles', 'nodes', 'vertices')) {
    particles(.data) <- activate(particles(.data), 'nodes')
  } else if (what %in% c('interaction', 'links', 'edges')) {
    particles(.data) <- activate(particles(.data), 'edges')
  }
  .data
}
#' @importFrom tidygraph activate
#' @export
tidygraph::activate
