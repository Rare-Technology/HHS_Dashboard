
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mapUI <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(
      outputId = ns("map"),
      height = "800px"
    )
    
  )
}

#' main_data Server Function
#'
#' @noRd 
mapServer <- function(id, state){
  moduleServer(
    id,
    function(input, output, server){
      
      output$map <- leaflet::renderLeaflet({
      tmap::tmap_options(basemaps = "Esri.WorldTopoMap")
      sites <- sf::st_as_sf(tibble(
        id = 1:2,
        lat = c(42, 43),
        lng = c(-122,-133)
      ), coords = c("lng", "lat"), crs = 4326, agr = "identity")
        map <- tmap::tm_shape(sites) +
          # tm_tiles("Esri.x") +
          tmap::tm_dots("id", title = "Title", size = 0.1) +
          tmap::tm_view(set.view = 3)


        tmap::tmap_leaflet(map)
      })
      
    }
  )
  
  
  
  
}

## To be copied in the UI
# mod_main_data_ui("main_data_ui_1")

## To be copied in the server
# callModule(mod_main_data_server, "main_data_ui_1")







# tmap::tmap_options(basemaps = "Esri.WorldTopoMap")
# 
# output$map <- leaflet::renderLeaflet({
#   req(input$variable)
#   
#   
#   data_chart <- dataset$tmp$spatial
#   
#   if (input$stream != "all") {
#     data_chart <- data_chart %>%
#       dplyr::filter(stream_number == input$stream)
#   }
#   
#   if (input$year != "all") {
#     data_chart <- data_chart %>%
#       dplyr::filter(year == input$year)
#   }
#   
#   data_chart <- data_chart %>%
#     dplyr::filter(
#       # var_type == "Response",
#       variable_name == !!input$variable
#     )
#   
#   req(NROW(data_chart) > 0)
#   
#   #tmap::tm_tiles("Esri.WorldTopoMap") + not sure why worldtopo doesn't work
#   map <- tmap::tm_shape(data_chart) +
#     # tm_tiles("Esri.x") + 
#     tmap::tm_dots("measure", title = input$variable, size = 0.1,
#                   popup.vars = names(data_chart)[names(data_chart)!="geometry"]) +
#     tmap::tm_view(set.view = 8)
#   
#   
#   tmap::tmap_leaflet(map)
# })