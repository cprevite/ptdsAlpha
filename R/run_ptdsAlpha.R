#' @export
run_ptdsAlpha = function(){
  shiny::runApp(paste0(system.file(package = 'ptdsAlpha'),
                       '/shiny/server_and_ui.R'),
                display.mode = 'normal')
}
