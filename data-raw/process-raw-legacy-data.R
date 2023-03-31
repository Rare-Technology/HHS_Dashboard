library(readr)
library(dplyr)
library(tidyr)
source("data-raw/spec-hhs-data.R")
source("data-raw/urls-data.R")

#************************************************
# Read most data in ----
#************************************************

legacy_data <- data.world::query(
  data.world::qry_sql("SELECT * FROM hh_surveys_all"),
  "https://data.world/rare/social-science-data"
) %>%
  dplyr::filter(ma_name != "")

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
  "Palau", "PLW",
  "Guatemala", "GTM",
)

legacy_data <- legacy_data %>%
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
# Function for nesting other questions ----
#************************************************

# Several questions have more than one response within
# an ID so we are nesting based on submission_id
# so that we can merge with the legacy_data


join_with_hhs <- function(.legacy_data, .other_data, var1) {
  .other_data <- .other_data %>%
    select(submissionid, {{ var1 }}) %>%
    dplyr::distinct() %>%
    tidyr::nest({{ var1 }} := {{ var1 }})

  n_other <- nrow(.other_data)
  n_unique <- length(unique(.other_data$submissionid))
  cat(crayon::cyan(glue::glue("{n_other} rows and {n_unique} unique submissionid")), "\n")
  if (n_other != n_unique) stop("N rows and N unique submissionid should be the same")

  n_legacy_data <- nrow(.legacy_data)

  .legacy_data <- left_join(
    .legacy_data,
    .other_data,
    by = "submissionid"
  )

  n_after_join <- nrow(.legacy_data)

  cat(crayon::blue(glue::glue("{n_legacy_data} rows before join {n_after_join} rows after")))

  if (n_legacy_data != n_after_join) stop("N rows before and after should be the same")
  
  .legacy_data
}


#************************************************
# Nest and join other questions ----
#************************************************


# These tables all have multiple answers
# TODO changes readr::read_csv -> data.world::query
hhs_q14 <- readr::read_csv(urls$q14, guess_max = 1000000)
legacy_data <- join_with_hhs(legacy_data, hhs_q14, `14_responsibility`)

hhs_q15 <- readr::read_csv(urls$q15, guess_max = 1000000)
legacy_data <- join_with_hhs(legacy_data, hhs_q15, `15_activity`)

hhs_q44 <- readr::read_csv(urls$q44, guess_max = 1000000) 
legacy_data <- join_with_hhs(legacy_data, hhs_q44, `44_meeting_attendance`)

hhs_q45 <- readr::read_csv(urls$q45, guess_max = 1000000) 
#hhs_q45 <- read_multi_csv(urls$q45)
legacy_data <- join_with_hhs(legacy_data, hhs_q45, `45_leadership_position`)
  
hhs_q48 <- readr::read_csv(urls$q48, guess_max = 1000000)
legacy_data <- join_with_hhs(legacy_data, hhs_q48, `48_enforcement_participation`)


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

legacy_data <- left_join(legacy_data, hhs_q69, by = 'submissionid')


#************************************************
# Adjust if PLW ----
#************************************************


legacy_data <- legacy_data %>% 
  dplyr::mutate(
    level2_name = ifelse(country == "PLW", subnational, local),
    maa = ifelse(country == "PLW", subnational, maa)
)


#************************************************
# Fix naming issue ----
#************************************************

legacy_data <- legacy_data %>% 
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

# legacy_data %>%
#   dplyr::select(matches("^\\D")) %>%
#   dplyr::glimpse()
# 
# hhs_q14 %>%
#   select(matches("^\\D")) %>%
#   glimpse()


#************************************************
# year information ----
# any year assignments here are subject to change eg Brazil gets new data, or 
# Honduras 2022 survey is split between two years so that lubridate::year
# trick doesn't work
#************************************************

legacy_data <- legacy_data %>%
  mutate(yearmonth = lubridate::ym(substr(updatedat, 1, 7)))

legacy_data$year[legacy_data$iso3 == "BRA"] <- ifelse(
  legacy_data$yearmonth[legacy_data$iso3 == "BRA"] < lubridate::ym("2021-10"),
  2019,
  2022
)
legacy_data$year[legacy_data$iso3 == "FSM"] <- 2020
legacy_data$year[legacy_data$iso3 == "HND"] <- lubridate::year(
  legacy_data$yearmonth[(legacy_data$iso3 == "HND")]
)
legacy_data$year[legacy_data$iso3 == "IDN"] <- 2019
legacy_data$year[legacy_data$iso3 == "MOZ"] <- ifelse(
  legacy_data$yearmonth[legacy_data$iso3 == "MOZ"] < lubridate::ym("2020-02"),
  2019,
  2021
)
legacy_data$year[legacy_data$iso3 == "PHL"] <- ifelse(
  legacy_data$yearmonth[legacy_data$iso3 == "PHL"] < lubridate::ym("2022-09"),
  2019,
  2022
)
legacy_data$year[legacy_data$iso3 == "PLW"] <- 2020
legacy_data$year[legacy_data$iso3 == "GTM"] <- ifelse(
  legacy_data$yearmonth[legacy_data$iso3 == "GTM"] < lubridate::ym("2022-11"),
  2021,
  2022
)

#### Fix HND geo info ####
# So there was never a follow up survey from Puerto Cortes in 2021. Instead, the 2021 data labeled as
# Puerto Cortes is most likely meant to be from Trujillo. We'll fix that here
legacy_data <- legacy_data %>% 
  dplyr::mutate(
    maa = dplyr::case_when(
      (country == "Honduras") & (year == 2021) & (maa == "Puerto Cortes") ~ "Trujillo",
      TRUE ~ maa
    )
  )

legacy_data <- dplyr::distinct(legacy_data)

usethis::use_data(legacy_data, overwrite=TRUE)