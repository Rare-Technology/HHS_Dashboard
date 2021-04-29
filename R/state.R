state <- reactiveValues(
  # data_source = rarefma::init_data_source,
  data_full = hhs_data,
  data_filtered = hhs_data_filtered,
  # data_summary = rarefma::data_summary[[rarefma::init_data_source]],
  # data_summary_filtered = rarefma::init_data_summary_filtered,
  hhs_data_geo = hhs_data_geo,
  # dates = rarefma::init_dates,
  # selections_geo = rarefma::init_geo_selections,
  country = list(
    choices = hhs_init_geo_selections$country$choices,
    selected = hhs_init_geo_selections$country$selected
  ),
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
    selected = hhs_init_geo_selections$maa$choices
  ),

  current_tab = 'Data',
  performance_indicators = 'Reporting Effort'

)