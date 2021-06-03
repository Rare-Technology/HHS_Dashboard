library(readr)
library(dplyr)
source("data-raw/spec-hhs-data.R")
source("data-raw/urls-data.R")

#************************************************
# Read most data in ----
#************************************************

# You will see warnings because some variables do not exist in some tables
# Load rarehhs package
hhs_data <- read_multi_csv(urls$hhs, col_type_spec = spec_hhs) %>%
  dplyr::filter(ma_name != "")

#************************************************
# Remove duplicates ----
#************************************************


# rename and remove duplicates that where submitted on different times
hhs_data <- hhs_data %>%
  dplyr::select(-c(updatedat, endformtimestamp, startformtimestamp)) %>%
  unique() %>%
  dplyr::left_join(hhs_data[, c(
    "submissionid",
    "updatedat",
    "startformtimestamp",
    "endformtimestamp"
  )] %>%
    group_by(submissionid) %>%
    summarise(updatedat = max(as.Date(updatedat))),
  by = "submissionid"
  )


#************************************************
# Add the iso3 ----
#************************************************



country <- tribble(
  ~country, ~iso3,
  "Brazil", "BRA",
  "Honduras", "HND",
  "Indonesia", "IDN",
  "Mozambique", "MOZ",
  "Philippines", "PHL",
  "Federated States of Micronesia", "FSM",
  "Palau", "PLW"
)

hhs_data <- hhs_data %>%
  rename(
    iso3 = country,
    subnational = level1_name,
    local = level2_name,
    maa = ma_name
  ) %>%
  left_join(
    country,
    by = "iso3"
  )


#************************************************
# Save for testing ----
#************************************************


hhs_data_save <- hhs_data
#hhs_data <- hhs_data_save


#************************************************
# Function for nesting other questions ----
#************************************************

# Several questions have more than one response within
# an ID so we are nesting based on submission_id
# so that we can merge with the hhs_data


join_with_hhs <- function(.hhs_data, .other_data, var1) {
  .other_data <- .other_data %>%
    select(submissionid, {{ var1 }}) %>%
    dplyr::distinct() %>%
    tidyr::nest({{ var1 }} := {{ var1 }})

  n_other <- nrow(.other_data)
  n_unique <- length(unique(.other_data$submissionid))
  cat(crayon::cyan(glue::glue("{n_other} rows and {n_unique} unique submissionid")), "\n")
  if (n_other != n_unique) stop("N rows and N unique submissionid should be the same")

  n_hhs_data <- nrow(.hhs_data)

  .hhs_data <- left_join(
    .hhs_data,
    .other_data,
    by = "submissionid"
  )

  n_after_join <- nrow(.hhs_data)

  cat(crayon::blue(glue::glue("{n_hhs_data} rows before join {n_after_join} rows after")))

  if (n_hhs_data != n_after_join) stop("N rows before and after should be the same")
  
  .hhs_data
}


#************************************************
# Nest and join other questions ----
#************************************************


# These tables all have multiple answers
hhs_q14 <- read_multi_csv(urls$q14)
hhs_data <- join_with_hhs(hhs_data, hhs_q14, `14_responsibility`)

hhs_q15 <- readr::read_csv(urls$q15, guess_max = 1000000)
hhs_data <- join_with_hhs(hhs_data, hhs_q15, `15_activity`)

hhs_q44 <- read_multi_csv(urls$q44) 
hhs_data <- join_with_hhs(hhs_data, hhs_q44, `44_meeting_attendance`)

hhs_q45 <- read_multi_csv(urls$q45)
hhs_data <- join_with_hhs(hhs_data, hhs_q45, `45_leadership_position`)
  
hhs_q48 <- readr::read_csv(urls$q48, guess_max = 1000000)
hhs_data <- join_with_hhs(hhs_data, hhs_q48, `48_enforcement_participation`)


hhs_q69 <- readr::read_csv(urls$q69) %>%
  select(
    submissionid,
    `69_fish_type`,
    `69_street`,
    `69_customer_home`,
    `69_market`,
    `69_shop`,
    `69_fishing_company`,
    `69_restaurant`,
    `69_own_home`,
    `69_other`
  ) %>%
  dplyr::distinct() %>% 
  tidyr::nest(
    `69_fish_type` = `69_fish_type`,
    `69_street` = `69_street`,
    `69_customer_home` = `69_customer_home`,
    `69_market` = `69_market`,
    `69_shop` = `69_shop`,
    `69_fishing_company` = `69_fishing_company`,
    `69_restaurant` = `69_restaurant`,
    `69_own_home` = `69_own_home`,
    `69_other` = `69_other`
  )

hhs_data <- left_join(hhs_data, hhs_q69, by = 'submissionid')


#************************************************
# Adjust if PLW ----
#************************************************


hhs_data <- hhs_data %>% 
  dplyr::mutate(
    level2_name = ifelse(country == "PLW", subnational, local),
    maa = ifelse(country == "PLW", subnational, maa)
)


#************************************************
# Fix naming issue ----
#************************************************

hhs_data <- hhs_data %>% 
  dplyr::select(
    -`61e_violations_decrease_profit`
  ) %>% 
  dplyr::rename(
    `61e_rights_distribution_fair` = `61f_rights_distribution_fair`,
    `61f_fishing_change_behavior` = `61g_fishing_change_behavior`,
    `61g_individual_behavior` = `61h_individual_behavior`,
    `61h_help_neighbors` = `61i_help_neighbors`
  )


#************************************************
# Review non-numbered variables ----
#************************************************

hhs_data %>%
  dplyr::select(matches("^\\D")) %>%
  dplyr::glimpse()

hhs_q14 %>%
  select(matches("^\\D")) %>%
  glimpse()


#************************************************
# Create the geotable ----
#************************************************


create_geo_table <- function(.data) {
  dplyr::distinct(
    .data,
    iso3,
    country,
    subnational,
    local,
    maa
  ) %>%
    dplyr::arrange(country, subnational, local, maa)
}

hhs_data_geo <- create_geo_table(hhs_data)
hhs_init_geo_selections <- rarehhs::get_geo_selections(
  hhs_data_geo,
  country_selected = "Indonesia"
)

geo_filter_data <- function(.data) {
  .data %>%
    dplyr::filter(country == hhs_init_geo_selections$country$selected) %>%
    dplyr::filter(subnational %in% hhs_init_geo_selections$subnational$selected) %>%
    dplyr::filter(local %in% hhs_init_geo_selections$local$selected) %>%
    dplyr::filter(maa %in% hhs_init_geo_selections$maa$selected)
}


hhs_data_filtered <- geo_filter_data(hhs_data)


#************************************************
# Set other variables ----
#************************************************


hhs_init_section <- "Basic Information"
hhs_data_source <- "Socio-economic baseline"

#Run this to make things faster for testing only.
# hhs_data <- hhs_data %>%
#   dplyr::select(matches("^\\D"), `3_community`, `6_gender`, `8_religion`, `9_region_member`,
#                 `10_mpa_important`, `12a_fishing_men`, `12b_fishing_women`, `12c_fishing_children`)
# 
# 
# 
# hhs_data_filtered <- hhs_data_filtered %>%
#   dplyr::select(matches("^\\D"), `3_community`, `6_gender`, `8_religion`, `9_region_member`,
#                 `10_mpa_important`, `12a_fishing_men`, `12b_fishing_women`, `12c_fishing_children`)

# usethis::use_data(hhs_data, overwrite = TRUE)
# usethis::use_data(hhs_data_filtered, overwrite = TRUE)
# usethis::use_data(hhs_questions, overwrite = TRUE)
# usethis::use_data(hhs_q14, overwrite = TRUE)

detach(pacakge:dplyr)
detach(package:tidyr)

usethis::use_data(
  hhs_data,
  hhs_data_filtered,
  hhs_questions,
  # hhs_q07,
  hhs_q14,
  hhs_q15,
  hhs_q44,
  hhs_q45,
  hhs_q48,
  hhs_q69,
  hhs_data_source,
  # hhs_init_data_source,
  hhs_data_geo,
  hhs_init_geo_selections,
  hhs_init_section,
  overwrite = TRUE
)

# not_list <- function(x) !is.list(x)
# hhs_not_list <- hhs_data %>%
#   select_if(not_list)
# 
# readr::write_csv(hhs_not_list, "inst/data/hhs.csv")
# zip("inst/data/hhs.zip", "inst/data/hhs.csv")

# 
# hhs_is_list <- hhs_data %>% 
#   select_if(is.list)

# I used this to create scripts
# x <- names(hhs_data)[grepl("^\\d.*", names(hhs_data))]
# x[stringr::str_detect(x, "meeting")]
#
# purrr::map(x, function(x){
#   path <- glue::glue("~/junk/t/plot_q{x}.R")
#   fs::file_create(path)
#   cat(glue::glue("
#
# prep_q{x} <- function(.data){{
#
# }}
#
# plot_q{x} <- function(.data, use_plotly = TRUE){{
#
# }}"
#
#   ), file = path)
# })