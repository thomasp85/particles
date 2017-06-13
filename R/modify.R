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
#' @param what Either `"particles"` or `"interactions"` to specify the data that
#' should be active (standard tidygraph names are also supported).
#'
#' @param ... Parameters passed on to the main verbs in tidygraph/dplyr
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
#' @importFrom tidygraph bind_nodes
#' @export
add_particles <- function(.data, ...) {
  particles(.data) <- bind_nodes(particles(.data), ...)
  retrain(.data)
}
#' @rdname simulation_modification
#' @importFrom tidygraph bind_edges
add_interaction <- function(.data, ...) {
  particles(.data) <- bind_edges(particles(.data), ...)
  retrain(.data)
}
#' @importFrom tidygraph filter
#' @export
filter.simulation <- function(.data, ...) {
  particles(.data) <- filter(particles(.data), ...)
  retrain(.data)
}
#' @rdname simulation_modification
#' @importFrom tidygraph filter
#' @export
tidygraph::filter
#' @importFrom tidygraph slice
#' @export
slice.simulation <- function(.data, ...) {
  particles(.data) <- slice(particles(.data), ...)
  retrain(.data)
}
#' @rdname simulation_modification
#' @importFrom tidygraph slice
#' @export
tidygraph::slice
#' @importFrom tidygraph mutate
#' @export
mutate.simulation <- function(.data, ...) {
  particles(.data) <- mutate(particles(.data), ...)
  retrain(.data)
}
#' @rdname simulation_modification
#' @importFrom tidygraph mutate
#' @export
tidygraph::mutate
#' @rdname simulation_modification
#' @importFrom tidygraph mutate_at
#' @export
tidygraph::mutate_at
#' @rdname simulation_modification
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
#' @rdname simulation_modification
#' @importFrom tidygraph activate
#' @export
tidygraph::activate
