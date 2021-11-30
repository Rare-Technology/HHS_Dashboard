create_state <- function(){
  reactiveValues(
    # data_source = rarefma::init_data_source,
    hhs_data_all = hhs_data,
    hhs_data_filtered = hhs_data_filtered,
    
    # data_summary = rarefma::data_summary[[rarefma::init_data_source]],
    # data_summary_filtered = rarefma::init_data_summary_filtered,
    hhs_data_geo = hhs_data_geo,
    # dates = rarefma::init_dates,
    # selections_geo = rarefma::init_geo_selections,
    sel_year = hhs_init_year_selections$selected,
    country = list(
      choices = hhs_init_geo_selections$country$choices,
      selected = hhs_init_geo_selections$country$selected
    ),
    iso3 = list(selected = get_iso3(hhs_init_geo_selections$country$selected)),
    subnational = list(
      choices = hhs_init_geo_selections$subnational$choices,
      selected = hhs_init_geo_selections$subnational$choices
    ),
    local = list(
      choices = hhs_init_geo_selections$local$choices,
      selected = hhs_init_geo_selections$local$choices
    ),
    maa = list(
      choices = hhs_init_geo_selections$maa$choices,
      selected = NULL
    ),
    # section = list(
    #   choices = names(hhs_questions),
    #   selected = hhs_init_section
    # ),
    question = list(
      choices = hhs_questions,
      selected = hhs_questions[[1]][1]
    ),
    current_tab = 'Data',
    performance_indicators = 'Reporting Effort',
    current_plot = NULL,
    language = "English"
  )
}



