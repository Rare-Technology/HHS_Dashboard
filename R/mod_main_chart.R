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
    
    observeEvent(c(input$question, state$hhs_data_filtered), {
      req(input$question, input$question!="")
   

      f <- HHS_PLOT_FUNS[grepl(input$question, HHS_PLOT_FUNS)]
      if(input$question %in% c("q51a", "q51b", "q51c")) f <- "plot_q51_fishers_permission"
      if(input$question %in% c("q51d", "q51e")) f <- "plot_q51_fishers_caught"
      if(input$question %in% c("q73")) f <- "plot_q72_current_economic"
      plot_hhs <- base::get(f)
      p <- try(plot_hhs(state$hhs_data_filtered, iso3 = state$iso3$selected), silent = TRUE)
      

      output$chart_ui <- renderUI({
        
        if("try-error" %in% class(p)) return(div("There was an error in plot generation"))
        
        if(TRUE){
          #output$chart <- renderPlotly(ggplot(mtcars, aes(cyl, mpg)) + geom_point() + ggtitle("This is my title"))
          p <- make_plotly(p)
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
