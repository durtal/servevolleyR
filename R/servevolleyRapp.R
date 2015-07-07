#' svRshiny
#'
#' @description shiny app to simulate tennis games:sets:matches between two players
#'
#' @export
svRshiny <- function() {
    shiny::runApp(system.file("servevolleyRapp", package = "servevolleyR"))
}
