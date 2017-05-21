inflict <- function(system, force, ...) {
  system$universe <- add_force(
    system$universe,
    force$new(...)
  )
  system
}
