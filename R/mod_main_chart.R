#' main_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
chartUI <- function(id){
  ns <- NS(id)
  tagList(

  )
}
    
#' main_data Server Function
#'
#' @noRd 
chartServer <- function(id, state){
  moduleServer(
    id,
    function(input, output, server){
 
    }
  )
  

  

}
    
## To be copied in the UI
# mod_main_data_ui("main_data_ui_1")
    
## To be copied in the server
# callModule(mod_main_data_server, "main_data_ui_1")
 
