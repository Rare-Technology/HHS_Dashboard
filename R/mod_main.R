#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mainUI <- function(id){
  ns <- NS(id)
    fillPage(
              sidebarLayout(
                sidebarUI("sidebarUI"),
                mainPanel(
                  tabsetPanel(id = ns("tabs"),
                    tabPanel("Start", startUI("startUI")),
                    tabPanel("Summary data", dataUI("dataUI")),
                    tabPanel("Survey charts", chartUI("chartUI"))
                  )
                )
              )
    
  )
}

#' main Server Function
#'
#' @noRd 
mainServer <- function(id, state){
  moduleServer(
    id,
    function(input, output, session){

      # observeEvent(input$tabs, {
      #   state$current_tab <- input$tabs
      # })
    }
  )
  
  
}
