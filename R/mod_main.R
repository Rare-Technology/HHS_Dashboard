#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mainUI <- function(id){
  ns <- NS(id)
    fixedPage(
              sidebarLayout(
                sidebarUI("sidebarUI"),
                mainPanel(
                  tabsetPanel(id = ns("tabs"),
                    tabPanel("Summary data", dataUI("dataUI")),
                    tabPanel("Survey charts", chartUI("chartUI")),
                    tabPanel("Download data",
                    div(
                      h3("Data downloads by question"),
                      tags$ul(
                      tags$li(a("hh_activities, Q15", href = "https://query.data.world/s/vh35m4iydwb6ii7olxeiydgj63zgjl")),
                      tags$li(a("hh_enforcement, Q48", href = "https://query.data.world/s/6bowin4lfthqltrmvjmhi5ern4uqxe")),
                      tags$li(a("hh_people, Q7", href = "https://query.data.world/s/jwlx4blr6tn76asurbxcp7ymgriwtq")),
                      tags$li(a("hh_customers, Q69", href = "https://query.data.world/s/oltaab47rb5ixiwlecxemhrwunbnej")),
                      tags$li(a("hh_leadership, Q45", href = "https://query.data.world/s/w4o2im3qbp7mmv4oajax2q5iowmxeg")),
                      tags$li(a("hh_meetings, Q44", href = "https://query.data.world/s/5kdowi3cmdyd4mayrdgdvzh6eqq4cj")),
                      tags$li(a("hh_responsibilities, Q14", href = "https://query.data.world/s/7irm5jn2zjyza2anjfpinzhzxhnlha")),
                      tags$li(a("hh_surveys, all other Qs", href = "https://query.data.world/s/sua67ju6acax2o3zyw4de4wbouqyhv"))
                      )
                    )      
                             
                    )
                    #tabPanel("Map", mapUI("mapUI")),
                    #tabPanel("Generate report", "Generate report")
                  )
                )
              )
    
  )
}

#' main Server Function
#'
#' @noRd 
mainServer <- function(id, state){
  moduleServer(
    id,
    function(input, output, session){

      # observeEvent(input$tabs, {
      #   state$current_tab <- input$tabs
      # })
    }
  )
  
  
}
