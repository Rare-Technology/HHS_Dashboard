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
      
      plot_hhs <- switch (input$question,
        # Basic Information Plots
        "q8" = plot_q08_religion,
        "q9" = plot_q09_region_member,
        "q10" = plot_q10_mpa_important
        
      )
      
      

      output$chart_ui <- renderUI({
        

        output$chart <- renderPlotly({
          plot_hhs(state$data_filtered, use_plotly = TRUE)
        })
        
        list(
          p("This is a warning"),
          plotlyOutput(ns("chart"), height = '750px')
        )
      })
      
    })
    
  })
}
