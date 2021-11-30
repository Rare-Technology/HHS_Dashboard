#' side_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
sidebarYearUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("year_ui"))
}
    
#' side_year Server Functions
#'
#' @noRd 
sidebarYearServer <- function(id, state){
  moduleServer( id, function(input, output, session){
    ns <- NS(id)
    
    output$year_ui <- renderUI({
      ui <- tagList(
        div(class = "sidetitle", tr(state, "Year")),
        selectInput(
          ns("sel_year"),
          tr(state, "Survey Year"),
          choices = hhs_init_year_selections$choices,
          selected = hhs_init_year_selections$selected
        )
      )
      
      ui
    })
    
    observeEvent(input$sel_year, {
      state$sel_year <- input$sel_year
    }, ignoreInit = TRUE
    )
  })
}
    
## To be copied in the UI
# mod_side_year_ui("side_year_ui_1")
    
## To be copied in the server
# mod_side_year_server("side_year_ui_1")
