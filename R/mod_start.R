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
  tagList(
      tags$h1("Welcome!"),
      tags$h2("August 30 2021 Notes"),
      tags$li("New plots for questions 8, 11, 14, 16, 17, 18, and 24"),
      tags$li("Question 24 (assets owned) now shows breakdown by each asset type."),
      tags$li("Fixed labels on question 37 (members who know size restrictions)"),
      tags$li("Managed access area selection is now blank by default."),
      tags$br(),
      tags$p("To get started, select a country on the left and then select the managed access areas you would like to see."),
      tags$p(tags$strong("No plots will show unless you select a managed access area."))
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
