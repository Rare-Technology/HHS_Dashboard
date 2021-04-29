#' main_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
chartUI <- function(id) {
  ns <- NS(id)
  tagList(
    inline_select(
      ns("section"),
      "Section",
      isolate(state$section$choices),
      isolate(state$section$selected)
    ),
    inline_select(
      ns("question"),
      "Question",
      isolate(state$question$choices),
      isolate(state$question$selected)
    ),
    uiOutput(ns("chart_ui"))
  )
}


chartServer <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$section,
      {
        questions <- hhs_questions[input$section] %>%
          unlist() %>%
          unname()
        state$question <- list(
          choices = questions,
          selected = questions[1]
        )

        updateSelectInput(
          session,
          "question",
          choices = questions,
          selected = questions[1]
        )
      },
      ignoreInit = TRUE
    )
    
    observeEvent(input$question, {
      

      output$chart_ui <- renderUI({
        
        output$chart <- renderPlot(plot(1:10))
        
        list(
          p("This is a warning"),
          plotOutput(ns("chart"))
        )
      })
      
    })
    
  })
}
