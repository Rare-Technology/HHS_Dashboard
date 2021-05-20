#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  state <- create_state()
  
  # Main application
  mainServer("mainUI", state)
  
  # Sidebar
  sidebarServer("sidebarUI")
  sidebarGeoServer("sidebarGeoUI", state)
  #sidebarStockServer("sidebarStockUI", state)
  sidebarSurveyServer("sidebarSurveyUI", state)
  
  
  # Main panel
  dataServer("dataUI", state)
  chartServer("chartUI", state, HHS_PLOT_FUNS)
  mapServer("mapUI", state)
  #selectServer("selectUI")
  #visualizeServer("visualizeUI")
  #interpretServer("interpretUI")
  #managementServer("managementUI")

}
