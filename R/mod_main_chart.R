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


chartServer <- function(id, state, HHS_PLOT_FUNS) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observeEvent(input$section,
      {
        questions <- hhs_questions[input$section] #%>%
         # unlist() %>%
          #unname()
        state$question <- list(
          choices = questions,
          selected = questions[1]
        )

        updateSelectInput(
          session,
          "question",
          choices = questions#,
          #selected = questions[1]
        )
      },
      ignoreInit = TRUE
    )
    
    observeEvent(input$question, {
  
      plot_hhs <- base::get(HHS_PLOT_FUNS[grepl(input$question, HHS_PLOT_FUNS)])
      p <- plot_hhs(state$hhs_data_filtered)
      
      output$chart_ui <- renderUI({
        
        if(FALSE){
          output$chart <- renderPlotly(p)
          p_output <- plotlyOutput(ns("chart"), height = '750px')
        } else {
          output$chart <- renderPlot(p)
          p_output <- plotOutput(ns("chart"), height = '750px')
        }

        list(
          p("This is a warning"),
          p_output
        )
      })
      
    })
    
  })
}
