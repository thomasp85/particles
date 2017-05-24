#' @export
link_force <- structure(list(
  strength = NULL,
  distance = NULL,
  count = NULL,
  bias = NULL,
  n_iter = NULL,
  strength_quo = NULL,
  distance_quo = NULL
), class = c('link_force', 'force'))
#' @export
print.link_force <- function(x, ...) {
  cat('Link Force:\n')
  cat('* A force implementing attraction between connected particles\n')
}
#' @importFrom rlang enquo eval_tidy %||%
#' @importFrom tidygraph as_tibble
#' @importFrom igraph degree
train_force.link_force <- function(force, particles, strength = NULL, distance = NULL, n_iter = 1, ...) {
  force$strength_quo <- enquo(strength)
  force$distance_quo <- enquo(distance)
  edges <- as_tibble(particles, 'edges')
  count <- degree(particles)
  strength <- eval_tidy(force$strength_quo, edges) %||% 1 / pmin(count[edges$from], count[edges$to])
  distance <- eval_tidy(force$distance_quo, edges) %||% 30

  force$n_iter <- n_iter
  force$strength <- rep(strength, length.out = nrow(edges))
  force$distance <- rep(distance, length.out = nrow(edges))
  force$bias <- count[edges$from] / (count[edges$from] + count[edges$to])
  force$count <- count
  force
}
#' @importFrom tidygraph as_tibble
#' @importFrom stats runif
apply_force.link_force <- function(force, particles, pos, vel, alpha, ...) {
  edges <- as_tibble(particles, 'edges')
  for (i in seq_len(force$n_iter)) {
    dist <- pos[edges$to, ] + vel[edges$to, ] - pos[edges$from, ] - vel[edges$from, ]
    dist[dist == 0] <- runif(sum(dist == 0), min = -0.5, max = 0.5) * 1e-6
    l <- sqrt(rowSums(dist^2))
    l <- (l - force$distance) / l * alpha * force$strength
    dist <- dist * cbind(l, l)
    all_mod <- rbind(
      dist * cbind(1 - force$bias, 1 - force$bias),
      -dist * cbind(force$bias, force$bias)
    )
    mod_index <- split(seq_len(nrow(all_mod)), c(edges$from, edges$to))
    all_mod_sum <- do.call(rbind, lapply(mod_index, function(i) {
      colSums(all_mod[i, , drop = FALSE])
    }))
    vel[as.integer(names(mod_index)), ] <- vel[as.integer(names(mod_index)), ] + all_mod_sum
  }
  list(position = pos, velocity = vel)
}
