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
    fluidPage(class='container',
              sidebarLayout(
                sidebarUI("sidebarUI"),
                mainPanel(
                  tabsetPanel(id = ns("tabs"),
                    tabPanel("Summary data", dataUI("dataUI")),
                    tabPanel("Survey charts", chartUI("chartUI")),
                    tabPanel("View map", "View map"),
                    tabPanel("Generate report", "Generate report")
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
