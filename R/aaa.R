load_pkgs <- function(packages, ...) {
  success <- purrr::map_lgl(packages, require, character.only = TRUE, ...)

  if(!all(success))
    stop(sprintf("Failed to load package(s): %s",
                 paste(packages[!success], collapse = ", ")))
}
