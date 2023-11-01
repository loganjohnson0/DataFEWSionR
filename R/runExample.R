#' Function to run the shiny app
#'
#' @param x shiny app to run
#'
#' @importFrom shiny runApp
#'
#' @export
runExample <- function(x) {
  appDir <- system.file("shiny-example",
                        package = "DataFEWSionR")
  if (appDir == "") {
    stop(paste0("Could not find example directory. ",
                "Try re-installing `DataFEWSionR`."), call. = FALSE)
  }
  # the first app will be called
  shiny::runApp(appDir = appDir)
}
