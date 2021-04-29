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
                    tabPanel("Survey charts", div(style="display:flex;",
                      selectInput("state", "Survey section",
                                  choices = unique(hhs_questions$section)
                      ),
                      selectInput("state", "Survey question",
                                  choices = unique(hhs_questions$question[c(3, 4, 5)])
                      ),
                    )),
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
