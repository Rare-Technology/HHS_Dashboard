#' side_geography UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
sidebarGeoUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("sel_datasource"),
      "Data source",
      choices = hhs_data_source,
      selected = hhs_data_source,
    ),
    selectInput(
      ns("sel_country"),
      "Country",
      choices = hhs_init_geo_selections$country$choices,
      selected = hhs_init_geo_selections$country$selected
    ),
    selectInput(
      ns("sel_subnational"),
      "Subnational government",
      choices = hhs_init_geo_selections$subnational$choices,
      selected =hhs_init_geo_selections$subnational$selected,
      multiple = TRUE,
      selectize = TRUE
    ),
    selectInput(
      ns("sel_local"),
      "Local government",
      choices = hhs_init_geo_selections$local$choices,
      selected = hhs_init_geo_selections$local$selected,
      multiple = TRUE,
      selectize = TRUE
    ),
    selectInput(
      ns("sel_maa"),
      "Managed access area",
      choices = hhs_init_geo_selections$maa$choices,
      selected = hhs_init_geo_selections$maa$selected,
      multiple = TRUE,
      selectize = TRUE
    )
  )
}

#' side_geography Server Function
#'
#' @noRd
sidebarGeoServer <- function(id, state) {
  moduleServer(
    id,
    function(input, output, session) {
      # observeEvent(input$sel_datasource,
      #   {
      #     # state$data_source <- input$sel_datasource
      #     # state$data_full <- rarefma::data_raw[[input$sel_datasource]]$data
      #     # state$hhs_data_geo <- rarefma::data_geo_family_species[[input$sel_datasource]]
      #     # state$data_summary <- rarefma::data_summary[[input$sel_datasource]]
      #   },
      #   ignoreInit = TRUE
      # )
      # 
      # 
      # observeEvent(state$data_source,
      #   {
      #     country_info <- get_country_selections(state$hhs_data_geo)
      #     state$country <- list(
      #       choices = country_info$choices,
      #       selected = country_info$selected
      #     )
      # 
      #     updateSelectInput(
      #       session,
      #       "sel_country",
      #       choices = country_info$choices,
      #       selected = country_info$selected
      #     )
      #   },
      #   ignoreInit = TRUE
      # )
      # 
      # 
      observeEvent(input$sel_country,
        {
          if (input$sel_country != state$country$selected) {
            state$country$selected <- input$sel_country
          }

          subnational_info <- get_subnational_selections(
            state$hhs_data_geo,
            country_selected = input$sel_country
          )
          state$subnational <- list(
            choices = subnational_info$choices,
            selected = subnational_info$selected
          )

          updateSelectInput(
            session,
            "sel_subnational",
            choices = subnational_info$choices,
            selected = subnational_info$selected
          )
        },
        ignoreInit = TRUE
      )


      observeEvent(input$sel_subnational,
        {
          if (!setequal(input$sel_subnational, state$subnational$selected)) {
            state$subnational$selected <- input$sel_subnational
          }

          local_info <- get_local_selections(state$hhs_data_geo,
            country_selected = input$sel_country,
            subnational_selected = input$sel_subnational
          )
          state$local <- list(
            choices = local_info$choices,
            selected = local_info$selected
          )
          updateSelectInput(
            session,
            "sel_local",
            choices = local_info$choices,
            selected = local_info$selected
          )
        },
        ignoreInit = TRUE
      )

      observeEvent(input$sel_local,
        {
          if (!setequal(input$sel_local, state$local$selected)) {
            state$local$selected <- input$sel_local
          }

          maa_info <- get_maa_selections(
            state$hhs_data_geo,
            country_selected = input$sel_country,
            subnational_selected = input$sel_subnational,
            local_selected = input$sel_local
          )
          state$maa <- list(
            choices = maa_info$choices,
            selected = maa_info$selected
          )
          updateSelectInput(
            session,
            "sel_maa",
            choices = maa_info$choices,
            selected = maa_info$selected
          )
        },
        ignoreInit = TRUE
      )


      observeEvent(input$sel_maa,
        {
          if (!setequal(input$sel_maa, state$maa$selected)) {
            state$maa$selected <- input$sel_maa
          }

          state$data_filtered <- state$data_full %>%
            dplyr::filter(
              country %in% input$sel_country,
              subnational %in% input$sel_subnational,
              local %in% input$sel_local,
              maa %in% input$sel_maa
            )
        },
        ignoreInit = TRUE
      )
    }
  )



  # return(
  #   sel_maa = reactive({ input$sel_maa })
  # )
}