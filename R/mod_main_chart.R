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
    uiOutput(ns("questions_ui")),
    uiOutput(ns("chart_ui"))
  )
}


chartServer <- function(id, state, HHS_PLOT_FUNS) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$questions_ui <- renderUI({
      inline_select(
        ns("question"),
        "Question",
        choices = state$question$choices,
        selected = state$question$selected
      )
    })
    
    observeEvent(input$question,
      {
        state$question$selected <- input$question
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
      result <- try(plot_hhs(state$hhs_data_filtered, iso3 = state$iso3$selected), silent = TRUE)

      if('try-error' %in% class(result)) {
        output$chart_ui <- renderUI({
          state$current_plot <- NULL
          return(div("There was an error in plot generation"))  
        })
        return(TRUE)
      }
      
      p <- result$plot
      state$current_plot_data <- result$data

      output$chart_ui <- renderUI({
        
        if(is.null(state$maa$selected)) {
          div('Select a managed access area.')
        } else if(is.null(p)) {
          div("There was not enough data to create this plot. Contact Rare S&T if you believe there is a mistake.")
        } else {
          state$current_plot <- p
          plot_height <- 400 + 50*length(state$sel_maa)
          plot_height <- paste0(plot_height, 'px')
          
          if(FALSE){
            # tweak this up some time, could be cool
            p <- make_plotly(p)
            output$chart <- renderPlotly(p)
            p_output <- plotlyOutput(ns("chart"), height = '750px')
          } else {
            output$chart <- renderPlot(p)
            p_output <- plotOutput(ns("chart"), height = '750px')
          }
          
          list(
            downloadButton(ns("downloadPlot"),class = "download-button", 'Download Plot'),
            div(style='height: calc(100vh - 220px); overflow-y: scroll', p_output)
          )
        }
      })
      
    }, ignoreNULL = TRUE)
    
    output$downloadPlot <- downloadHandler(
      filename = function(){paste0("hhs_", tolower(gsub(" ", "_", state$question$selected)), ".zip")},
      content = function(file){
        wd <- getwd()
        setwd(tempdir())
        
        data_name <- 'data.csv'
        plot_name <- paste0("plot_", tolower(gsub(" ", "_", state$question$selected)), ".png")

        write.csv(state$current_plot_data, data_name, row.names = FALSE)
        ggsave(plot_name, plot=state$current_plot, width = 27, height = 20, units = "cm")
        fs = c(data_name, plot_name)
        zip(zipfile=file, files=fs)
        
        setwd(wd)
      }, contentType = "application/zip")
    
  })
}
