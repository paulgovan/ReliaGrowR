#' ReliaGrowR API
#'
#' This function provides an interface to the ReliaGrowR API.#' This function provides an interface to the ReliaGrowR API.
#' @import plumber
#' @return Launches the ReliaGrowR API on a local server.
#' @examples
#' \dontrun{
#' grwr_api()
#' }
#' @export
grwr_api <- function() {
  # Run the API
  root <- plumber::pr("inst/plumber/plumber.R")
  root %>% plumber::pr_run()
}
