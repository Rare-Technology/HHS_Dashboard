#' start UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
startUI <- function(id){
  ns <- NS(id)
  div(id="startpage",
    tags$h1("Welcome!"),
 
    tags$p("To get started, click on ", icon('filter'),
           " to select a country and then select the managed access areas you ",
           " would like to see."),
    div(class = 'timeouttxt',
        h3(class = 'timeouttitle', "Please note"),
        p("This app may time-out if left idle too long, which will cause the",
          " screen to grey-out. To use the app again, refresh the page.")
    )
  )
}
    
#' start Server Functions
#'
#' @noRd 
startServer <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_start_ui("start_ui_1")
    
## To be copied in the server
# mod_start_server("start_ui_1")
