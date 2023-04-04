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
      
      # Check if there are any responses to the question.
      # If so, return message indicating the question was not answered without attempting any
      # plotting logic.
      if(input$question %in% c("q08", "q09")) {
        # q08 -> 8
        qnum <- stringr::str_sub(input$question, 3)
      } else {
        # q65d -> 65d
        qnum <- stringr::str_sub(input$question, 2)
      }
      pattern <-  paste0("^", qnum, "[a-z]?_")
      qcols <- stringr::str_subset(names(hhs_data), pattern)
      responses <- state$hhs_data_filtered %>% 
        dplyr::select(dplyr::all_of(qcols)) %>% 
        unlist() %>% 
        unique() %>% 
        stringr::str_to_lower()
      
      # Question is considered unanswered if the unique responses to all relevant columns are
      # NA or codes for NA (such as "Not Answered")
      if(purrr::is_empty(setdiff(responses, NA_ANSWERS))) {
        output$chart_ui <- renderUI({
          state$current_plot <- NULL
          return(div(
            paste("This question was not answered 🤔 Maybe it did not appear on the survey. If you think this is a mistake, contact", EMAIL_ADDR, "and include 'dashboard' in the subject line.")
          ))  
        })
        return(TRUE)
      }

      f <- HHS_PLOT_FUNS[grepl(input$question, HHS_PLOT_FUNS)]
      if(input$question %in% c("q65a", "q65b", "q65c")) f <- "plot_q65_fishers_permission"
      if(input$question %in% c("q65d", "q65e")) f <- "plot_q65_fishers_caught"
      if(input$question %in% c("q73")) f <- "plot_q72_current_economic"

      plot_hhs <- base::get(f)
      result <- try(plot_hhs(state$hhs_data_filtered, iso3 = state$iso3$selected), silent = TRUE)

      if('try-error' %in% class(result)) {
        output$chart_ui <- renderUI({
          state$current_plot <- NULL
          return(div(
            paste("There was an error while making the plot 😱 Please report this bug to", EMAIL_ADDR, "and include 'dashboard' in the subject line.")
           ))
        })
        return(TRUE)
      }
      
      p <- result$plot
      state$current_plot_data <- result$data

      output$chart_ui <- renderUI({
        
        if(is.null(state$maa$selected)) {
          div('Select a managed access area.')
        } else if(is.null(p)) {
          div(paste("There was not enough data to create this plot 😳 If you think there is a mistake, contact", EMAIL_ADDR, "and include 'dashboard' in the subject line."))
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
