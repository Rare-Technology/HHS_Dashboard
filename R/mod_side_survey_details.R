#'  UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebarSurveyUI <- function(id) {
  ns <- NS(id)
  tagList(
    #uiOutput(ns("survey_details"))
    div('Sidebar survey')
  )
}

#' sidebar performance indicators Server Function
#'
#' @noRd
sidebarSurveyServer <- function(id, state) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {

    
    }
  )
}
