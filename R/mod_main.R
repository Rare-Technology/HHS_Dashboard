#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets dropdown
#' @importFrom shinyjs useShinyjs extendShinyjs
mainUI <- function(id){
  ns <- NS(id)
  div(id = "hhs-body",
  fillPage(
    useShinyjs(),
    extendShinyjs(script="www/toggleFullScreen.js", functions=c("toggleFullScreen")),
    div(class='flow-div',
        dropdown(id='step1',
                 sidebarUI("sidebarUI"),
                 width='300px',
                 size='sm',
                 icon = icon('filter'),
                 status = 'success',
                 style='material-circle'
        ),
        div(style='flex-grow: 1;'),
        div(id="lang-select", selectInput(ns("language"), "", width = 80,
                                          c("EN" = "English",
                                            "ID" = "Bahasa Indonesia",
                                            "BRA" = "Português (BRA)",
                                            "MOZ" = "Português (MOZ)",
                                            "ES" = "Español"
                                            ))
        ),
        div(class = 'fs-button',
            HTML(
              "<svg xmlns='http://www.w3.org/2000/svg' width='24' height='24' fill='currentColor' class='bi bi-fullscreen' viewBox='0 0 16 16'>
          <path d='M1.5 1a.5.5 0 0 0-.5.5v4a.5.5 0 0 1-1 0v-4A1.5 1.5 0 0 1 1.5 0h4a.5.5 0 0 1 0 1h-4zM10 .5a.5.5 0 0 1 .5-.5h4A1.5 1.5 0 0 1 16 1.5v4a.5.5 0 0 1-1 0v-4a.5.5 0 0 0-.5-.5h-4a.5.5 0 0 1-.5-.5zM.5 10a.5.5 0 0 1 .5.5v4a.5.5 0 0 0 .5.5h4a.5.5 0 0 1 0 1h-4A1.5 1.5 0 0 1 0 14.5v-4a.5.5 0 0 1 .5-.5zm15 0a.5.5 0 0 1 .5.5v4a1.5 1.5 0 0 1-1.5 1.5h-4a.5.5 0 0 1 0-1h4a.5.5 0 0 0 .5-.5v-4a.5.5 0 0 1 .5-.5z'/>
        </svg>"),
            onclick = "shinyjs.toggleFullScreen();"
        )
    ),
    uiOutput(ns("tabPanels"))
  )
  )
}

#' main Server Function
#'
#' @noRd 
mainServer <- function(id, state){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session){
      output$tabPanels <- renderUI({
        ui <- tabsetPanel(id = ns("tabs"),
                    tabPanel(tr(state, "Start"), startUI("startUI")),
                    tabPanel(tr(state, "Table"), dataUI("dataUI")),
                    tabPanel(tr(state, "Survey charts"), chartUI("chartUI"))
        )
        ui
      })
      # observeEvent(input$tabs, {
      #   state$current_tab <- input$tabs
      # })
      observeEvent(input$language, {
        state$language <- input$language
        
        hhs_questions_copy <- hhs_questions
        for (section_idx in 1:length(hhs_questions_copy)) {
          for (q_idx in 1:length(hhs_questions_copy[[section_idx]])) {
            current_q <- names(hhs_questions_copy[[section_idx]])[q_idx]
            translated_q <- tr(state, current_q)
            names(hhs_questions_copy[[section_idx]])[q_idx] <- translated_q
          }
          current_section <- names(hhs_questions_copy)[section_idx]
          translated_section <- tr(state, current_section)
          names(hhs_questions_copy)[section_idx] <- translated_section
        }

        state$question$choices <- hhs_questions_copy
        state$question$selected <- hhs_questions_copy[[1]][1]
      })
    }
  )
  
  
}
