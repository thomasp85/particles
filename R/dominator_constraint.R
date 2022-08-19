#' Restrict child position based on parent position
#'
#' This constraint requires children to be positioned at a certain side of their
#' parent and with a certain distance. It can be used to enforce a layering of
#' particles for e.g. DAG and tree layouts.
#'
#' @section Training parameters:
#' The following parameters defines the training of the constraint and can be
#' passed along a call to [impose()]
#'
#' - `distance` : The minimum orthogonal distance to the parent. Default to `0`,
#'   meaning that children are only required to be positioned to the specific
#'   side of their parent. (*tidy eval*)
#' - `angle` : The direction the children should be enforced to be relative to
#'   their parent. Defaults to `-pi/2` which is equivalent to down. (*tidy eval*)
#'
#' @family constraints
#' @usage NULL
#' @format NULL
#' @export
dominator_constraint <- structure(list(
  distance = NULL,
  angle = NULL,
  distance_quo = NULL,
  angle_quo = NULL,
  children = NULL,
  vec = NULL,
  vec_orth = NULL
), class = c('dominator_constraint', 'constraint'))
#' @export
print.dominator_constraint <- function(x, ...) {
  cat('Dominator constraint:\n')
  cat('* A constraint requiring children to be positioned to a certain side of their parent\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble with_graph graph_is_dag
#' @importFrom igraph neighborhood
#' @export
train_constraint.dominator_constraint <- function(constraint, particles, distance = NULL, angle = NULL, ...) {
  constraint <- NextMethod()
  stopifnot(with_graph(particles, graph_is_dag()))
  constraint$distance_quo <- enquo(distance)
  constraint$angle_quo <- enquo(angle)
  nodes <- as_tibble(particles, 'nodes')
  distance <- eval_tidy(constraint$distance_quo, nodes) %||% 0
  angle <- eval_tidy(constraint$angle_quo, nodes) %||% (-pi/2)

  constraint$distance <- rep(distance, length.out = nrow(nodes))
  constraint$angle <- rep(angle, length.out = nrow(nodes)) %% (2*pi)
  constraint$children <- lapply(neighborhood(particles, order = 1, mode = 'out', mindist = 1), as.integer)
  constraint$vec <- cbind(cos(constraint$angle) * constraint$distance, sin(constraint$angle) * constraint$distance)
  constraint$vec_orth <- cbind(cos(constraint$angle - pi/2), sin(constraint$angle - pi/2))
  constraint
}
#' @importFrom rlang quos
#' @importFrom digest digest
#' @importFrom igraph neighborhood
#' @export
retrain_constraint.dominator_constraint <- function(constraint, particles, ...) {
  dots <- quos(...)
  particle_hash <- digest(particles)
  new_particles <- particle_hash != constraint$particle_hash
  constraint$particle_hash <- particle_hash
  nodes <- as_tibble(particles, active = 'nodes')
  constraint <- update_quo(constraint, 'include', dots, nodes, new_particles, TRUE)
  constraint <- update_quo(constraint, 'distance', dots, nodes, new_particles, TRUE)
  constraint <- update_quo(constraint, 'angle', dots, nodes, new_particles, TRUE)
  constraint$angle <- constraint$angle %% (2*pi)
  constraint$vec <- cbind(cos(constraint$angle)*constraint$distance, sin(force$angle)*constraint$distance)
  constraint$vec_orth <- cbind(cos(constraint$angle - pi/2), sin(force$angle - pi/2))
  if (new_particles) {
    constraint$children <- lapply(neighborhood(particles, order = 1, mode = 'out', mindist = 1), as.integer)
  }
  constraint
}
#' @importFrom tidygraph as_tibble
#' @importFrom stats runif
#' @export
apply_constraint.dominator_constraint <- function(constraint, particles, pos, vel, alpha, ...) {
  front_pos <- pos + constraint$vec
  parent_ind <- rep(seq_along(constraint$children), lengths(constraint$children))
  parent_line <- (front_pos + constraint$vec_orth)[parent_ind, , drop = FALSE]
  pos_parent <- front_pos[parent_ind, , drop = FALSE]
  pos_children <- pos[unlist(constraint$children), , drop = FALSE]
  project <- points_to_lines(pos_parent, parent_line, pos_children)
  norm_vec <- pos_children - project$projection
  proj_angle <- atan2(norm_vec[,2], norm_vec[,1]) %% (2*pi)
  dist <- project$distance * ifelse(abs(constraint$angle[parent_ind] - proj_angle) > 1e-6, 1, 0)

  child_mod <- split(seq_along(dist)[dist != 0], unlist(constraint$children)[dist != 0])
  mod_sum <- do.call(rbind, lapply(child_mod, function(i) {
    project$projection[i[which.max(dist[i])], , drop = FALSE]
  }))
  pos[as.integer(names(child_mod)), ] <- mod_sum
  list(position = pos, velocity = vel)
}

points_to_lines <- function(line1, line2, point) {
  storage.mode(line1) <- 'double'
  storage.mode(line2) <- 'double'
  storage.mode(point) <- 'double'
  points_to_lines_c(line1, line2, point)
}
