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
    # inline_select(
    #   ns("section"),
    #   "Section",
    #   NULL, #isolate(state$section$choices),
    #   NULL #isolate(state$section$selected)
    # ),
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
    
    observeEvent(input$question, {
      updateSelectInput(
        session,
        "question",
        choices = state$question$choices,
        selected = state$question$selected
      )
    }, once = TRUE)
    
    
    
    observeEvent(input$question,
      {
        # questions <- hhs_questions[input$section] 
        state$question <- list(
          selected = input$question
        )

        # updateSelectInput(
        #   session,
        #   "question",
        #   choices = hhs_questions#,
        #   #selected = questions[1]
        # )
      },
      ignoreInit = TRUE
    )
    
    observeEvent(c(input$question, state$hhs_data_filtered), {
      req(input$question, input$question!="")
   


      f <- HHS_PLOT_FUNS[grepl(input$question, HHS_PLOT_FUNS)]
      if(input$question %in% c("q51a", "q51b", "q51c")) f <- "plot_q51_fishers_permission"
      if(input$question %in% c("q51d", "q51e")) f <- "plot_q51_fishers_caught"
      if(input$question %in% c("q73")) f <- "plot_q72_current_economic"
      if(input$question %in% c("q07")) f <- "plot_q07_people"


      plot_hhs <- base::get(f)
      p <- try(plot_hhs(state$hhs_data_filtered, iso3 = state$iso3$selected), silent = TRUE)
      

      output$chart_ui <- renderUI({
        
        if("try-error" %in% class(p)){
          state$current_plot <- NULL
          return(div("There was an error in plot generation"))
        }
        
        state$current_plot <- p
        
        if(FALSE){
          #output$chart <- renderPlotly(ggplot(mtcars, aes(cyl, mpg)) + geom_point() + ggtitle("This is my title"))
          p <- make_plotly(p)
          output$chart <- renderPlotly(p)
          p_output <- plotlyOutput(ns("chart"), height = '750px')
        } else {
          output$chart <- renderPlot(p)
          p_output <- plotOutput(ns("chart"), height = '750px')
        }

        list(
          downloadButton(ns("downloadPlot"),class = "download-button", 'Download Plot'),
          p_output
        )
      })
      
    }, ignoreNULL = TRUE)
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste0("plot_", tolower(gsub(" ", "_", state$question$selected)), ".png")},
      content = function(file){
        ggsave(file,plot=state$current_plot, width = 27, height = 20, units = "cm")
      })
    
  })
}
