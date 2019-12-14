#' @export
run_ptdsAlpha = function(){
  appDir = system.file("server_and_ui", package = "ptdsAlpha")
  shiny::runApp(appDir, display.mode = "normal")
}
