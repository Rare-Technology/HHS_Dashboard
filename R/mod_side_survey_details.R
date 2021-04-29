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
    collapse_wrapper(
      div(
        radioButtons(
          "a",
          "",
          choices = c(
            "Household survey summary",
            "8. What is the religion of the head of household?",
            "9. Do you identify yourself as a member...?"
          )
        )
      ),
      open       = 'false',
      collapseid = 'collapse2',
      title      = "Basic information"
    ),
    collapse_wrapper(
      div(
        radioButtons(
          'b',
          '',
          choices = c(
            "11. Sources of household income this year...",
            "12. Pretend question for example"
          )
        )
      ),
      open       = 'false',
      collapseid = 'collapse3',
      title      = "Livelihood"
    )
    # radioButtons(
    #   inputId = 'hhs_section',
    #   label = "Select survey section",
    #   choices =  unique(hhs_questions$section),
    #   selected =  "Basic Information",
    #   inline = FALSE
    # )
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
