#' Launch the aquacropr Shiny studio
#'
#' Opens the graphical workflow for setting up AquaCrop plugin projects,
#' editing parameter YAML, running simulations, and running calibration,
#' sensitivity, and design analyses. Requires a working [aquacropr]
#' installation and, for simulation, an AquaCrop stand-alone executable
#' (or a command template).
#'
#' @param ... Passed to [shiny::runApp()] (for example `launch.browser = TRUE`,
#'   `port = 3838`).
#'
#' @return The value of [shiny::runApp()], invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' aquacropr.studio::run_aquacropr_studio()
#' }
run_aquacropr_studio <- function(...) {
  shiny::runApp(aquacropr_studio_app(), ...)
}

#' Build the aquacropr studio Shiny application object
#'
#' Use this when you want the app object without immediately running it
#' (for example `shiny::runApp(aquacropr_studio_app(), port = 3838)`).
#'
#' @return A `shiny.appobj`.
#' @export
aquacropr_studio_app <- function() {
  options(shiny.maxRequestSize = 80 * 1024^2)
  shiny::shinyApp(ui = aquacropr_studio_ui(), server = aquacropr_studio_server)
}
