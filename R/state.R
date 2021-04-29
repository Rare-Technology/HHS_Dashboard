state <- reactiveValues(
  # data_source = rarefma::init_data_source,
  data_full = rarehhs::hhs_data,
  data_filtered = rarehhs::hhs_data_filtered,
  # data_summary = rarefma::data_summary[[rarefma::init_data_source]],
  # data_summary_filtered = rarefma::init_data_summary_filtered,
  hhs_data_geo = rarehhs::hhs_data_geo,
  # dates = rarefma::init_dates,
  # selections_geo = rarefma::init_geo_selections,
  country = list(
    choices = rarehhs::hhs_init_geo_selections$country$choices,
    selected = rarehhs::hhs_init_geo_selections$country$selected
  ),
  subnational = list(
    choices = rarehhs::hhs_init_geo_selections$subnational$choices,
    selected = rarehhs::hhs_init_geo_selections$subnational$choices
  ),
  local = list(
    choices = rarehhs::hhs_init_geo_selections$local$choices,
    selected = rarehhs::hhs_init_geo_selections$local$choices
  ),
  maa = list(
    choices = rarehhs::hhs_init_geo_selections$maa$choices,
    selected = rarehhs::hhs_init_geo_selections$maa$choices
  ),

  section = list(
    choices = names(rarehhs::hhs_questions),
    selected = rarehhs::hhs_init_section
  ),
  question = list(
    choices = rarehhs::hhs_questions[rarehhs::hhs_init_section],
    selected = rarehhs::hhs_questions[rarehhs::hhs_init_section]
  ),
  current_tab = 'Data',
  performance_indicators = 'Reporting Effort'

)