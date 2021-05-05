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
      NULL, #isolate(state$section$choices),
      NULL #isolate(state$section$selected)
    ),
    inline_select(
      ns("question"),
      "Question",
      NULL, #isolate(state$question$choices),
      NULL #isolate(state$question$selected)
    ),
    uiOutput(ns("chart_ui"))
  )
}


chartServer <- function(id, state, HHS_PLOT_FUNS) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$section, {
      updateSelectInput(
        session,
        "section",
        choices = state$section$choices,
        selected = state$section$selected
      )
    }, once = TRUE)
    
    
    
    observeEvent(input$section,
      {
        questions <- hhs_questions[input$section] 
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
      req(input$question, input$question!="")
   
      plot_hhs <- base::get(HHS_PLOT_FUNS[grepl(input$question, HHS_PLOT_FUNS)])
      p <- plot_hhs(state$hhs_data_filtered)
      
      output$chart_ui <- renderUI({
        
        if(TRUE){
          #output$chart <- renderPlotly(ggplot(mtcars, aes(cyl, mpg)) + geom_point() + ggtitle("This is my title"))
          output$chart <- renderPlotly(p)
          p_output <- plotlyOutput(ns("chart"), height = '750px')
        } else {
          output$chart <- renderPlot(p)
          p_output <- plotOutput(ns("chart"), height = '750px')
        }

        list(
          p_output
        )
      })
      
    })
    
  })
}
