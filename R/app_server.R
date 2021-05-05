#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # The state object is read in automatically from state.R
  HHS_PLOT_FUNS <- ls("package:rarehhs", pattern = "plot_")
  
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
  #selectServer("selectUI")
  #visualizeServer("visualizeUI")
  #interpretServer("interpretUI")
  #managementServer("managementUI")

}
