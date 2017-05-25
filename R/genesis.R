#' @importFrom igraph gorder
#' @export
phyllotactic_genesis <- function(radius = 10, angle = pi * (3 - sqrt(5))) {
  function(particles, ...) {
    n_particles <- gorder(particles)
    p_radius <- radius * sqrt(seq_len(n_particles))
    p_angle <- angle * seq_len(n_particles)
    pos <- cbind(p_radius * cos(p_angle), p_radius * sin(p_angle))
    vel <- matrix(0, ncol = 2, nrow = n_particles)
    list(position = pos, velocity = vel)
  }
}
#' @importFrom rlang enquo eval_tidy
#' @importFrom tidygraph as_tibble
#' @export
predefined_genesis <- function(x = x, y = y, x_vel = 0, y_vel = 0) {
  x <- enquo(x)
  y <- enquo(y)
  x_vel <- enquo(x_vel)
  y_vel <- enquo(y_vel)
  function(particles, ...) {
    nodes <- as_tibble(particles, active = 'nodes')
    x <- rep(eval_tidy(x, nodes), length.out = nrow(nodes))
    y <- rep(eval_tidy(y, nodes), length.out = nrow(nodes))
    x_vel <- rep(eval_tidy(x_vel, nodes), length.out = nrow(nodes))
    y_vel <- rep(eval_tidy(y_vel, nodes), length.out = nrow(nodes))
    if (any(is.null(x), is.null(y), is.null(x_vel), is.null(y_vel))) {
      stop('Cannot extract start position or velocity', call. = FALSE)
    }
    list(position = cbind(x, y), velocity = cbind(x_vel, y_vel))
  }
}
