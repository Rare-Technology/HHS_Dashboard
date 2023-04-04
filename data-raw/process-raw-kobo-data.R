library(httr)
library(readxl)
library(data.world)
library(dplyr)
library(stringr)
library(lubridate)

# TODO Use kobo api to download data like we do w/ the Moz data below.
# Currently, doing it exactly the same way doesn't work because the data is too big and the request
# can't be completed.
# As a workaround, go on kobo and create different export settings for each country. I'm thinking 
# that will let the requests complete. But reach out to kobo and see if they can provide any help.
httr::GET("https://query.data.world/s/jwd5niku3brhqgcn7smgfev4sxtw2d",
          httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
kobo_data <- readxl::read_excel(tf)

# Remove some junk columns that are not supposed to be there
# George is working on fixing this, so once that's done, this line
# can be removed
kobo_data <- kobo_data[,c(1:431)]

# Remove leading underscores from column names
names(kobo_data) <- stringr::str_sub(names(kobo_data), 2)

#### Moz 2022 data ####
# Load Mozambique 2022 survey data
KOBO_API_KEY <- Sys.getenv("KOBO_API_KEY")
httr::GET(
  "https://kf.kobotoolbox.org/api/v2/assets/aEe22GR8QKMe3xNGQWDoPD/export-settings/esMqBaZB3jahL4GeNqdVbAf/data.xlsx",
  httr::add_headers(Authorization=paste("Token", KOBO_API_KEY)),
  httr::write_disk(tf2 <- tempfile(fileext=".xlsx"))
)
moz23 <- readxl::read_excel(tf2)

names(moz23) <- stringr::str_sub(names(moz23), 2)

### Map Moz survey q's to complete survey q's
remove_leading_num <- function (q) {
  if (stringr::str_detect(q, "^[0-9]")) {
    tokens <- stringr::str_split(q, "_", simplify = TRUE)[-1]
    newstr <- paste0(tokens, collapse = "_")
  } else {
    newstr <- q
  }
  return(newstr)
}

moz_q_text <- purrr::map(
  names(moz23),
  remove_leading_num
) %>% unlist()

kobo_q_text <- purrr::map(
  names(kobo_data),
  remove_leading_num
) %>% unlist()

### Mapping using descriptive text
# Now we can map based on matching column text.
q_mapping <- purrr::imap(
  names(moz23),
  function(q, i) {
    txt <- moz_q_text[i]
    if (any(txt == kobo_q_text)) {
      q_new <- names(kobo_data)[txt == kobo_q_text]
    } else {
      q_new <- NA
    }

    return(q_new)
  }
)

# Change output names, so we can do something like
# > q_mapping[["11a_months_farming"]] # the old column
# [1] "14a_months_farming" # the new column
names(q_mapping) <- names(moz23)

# Unlike how the mapping is done for the legacy data, in the Moz dataset, if q_mapping has a missing
# value, that is just not a column we care about (it's something like "note_9" or is a column related
# to the VCA survey). So, we will just remove these from q_mapping
q_mapping <- q_mapping[!is.na(q_mapping)]

moz23_names <- names(q_mapping)
kobo_names <- unname(unlist(q_mapping))
# Rename old columns using the mapper we just made
# The Moz survey definitely has a lot less HHS questions than the rest of the kobo surveys
moz23 <- moz23 %>% 
  dplyr::select(dplyr::all_of(moz23_names)) %>% 
  dplyr::rename_with(~ kobo_names, dplyr::all_of(moz23_names)) %>% 
  dplyr::mutate(country="MOZ")

kobo_data <- dplyr::bind_rows(kobo_data, moz23) %>% 
  dplyr::distinct()

# A bit of data cleaning; rename/mutate geographic cols
kobo_data <- kobo_data %>% 
  dplyr::rename(
    submissionid = uuid,
    updatedat = submission_time,
    iso3 = country,
    level1_id = `3_province`, # verify this and the next couple geographic columns
    level2_id = `3_municipality`, # also, these are just ID's, not names
    level4_id = `3_community`,
    lon = store_gps_longitude,
    lat = store_gps_latitude
  ) %>%
  dplyr::mutate(
    country = dplyr::case_when(
      iso3 == "IDN" ~ "Indonesia",
      iso3 == "HND" ~ "Honduras",
      iso3 == "PHL" ~ "Philippines",
      iso3 == "FSM" ~ "Federated States of Micronesia",
      iso3 == "BRA" ~ "Brazil",
      iso3 == "GTM" ~ "Guatemala",
      iso3 == "MOZ" ~ "Mozambique",
      iso3 == "PLW" ~ "Palau",
      TRUE ~ iso3 # something slips through the cracks
    ),
    updatedat_ymd = lubridate::as_date(updatedat),
    yearmonth = stringr::str_sub(updatedat_ymd, 1, 7),
    year = stringr::str_sub(updatedat_ymd, 1, 4) %>% as.integer(),
  ) %>%
  dplyr::select(-updatedat_ymd) %>% 
  dplyr::filter(!(`1_interviewer` %in% c("George Stoyle", "test", "Test", "Test 2", "Teste", "Teste 2")))

### Add geographic info
# The only geographic columns are made up of id's, and there is not info for
# the maa. So let's add names and maa info.

geo <- data.world::query(
  data.world::qry_sql("SELECT * FROM footprint_global"),
  "https://data.world/rare/footprint"
)

# First, there's 5 rows missing all level1_id, level2_id, and level4_id.
# Nothing we can really do there.
# But, there are 14 rows missing only leve1_id, with level2_id and level4_id OK
# So let's join on these two columns to extract the missing level1_id's

kobo_data <- kobo_data %>% 
  dplyr::select(-level1_id) %>% 
  dplyr::left_join(
    geo %>% dplyr::select(
      level2_id, level4_id, level1_id
    )
  )

# Now add names for level1/2/4
kobo_data <- kobo_data %>% 
  dplyr::left_join(
    geo %>% dplyr::select(
      level1_id, subnational = level1_name,
      level2_id, local = level2_name,
      level4_id, `3_community` = level4_name,
      maa = ma_name, ma_area, ma_status
    ),
    by = c("level1_id", "level2_id", "level4_id")
  ) %>%
  dplyr::select(-c(
    level1_id,
    level2_id,
    level4_id,
    tore_gps, # not a typo
    store_gps_altitude,
    store_gps_precision,
    `94_yes_no`,
    id,
    # Basically all the note columns are blank
    stringr::str_subset(names(.), "note_")
  ))

idn_update <- data.world::query(
  data.world::qry_sql("SELECT * FROM hh_surveys_idn_2022_revisions"),
  "https://data.world/rare/hh-surveys"
)

idn_update <- idn_update %>% 
  dplyr::mutate(
    updatedat_ymd = lubridate::as_date(updatedat),
    yearmonth = stringr::str_sub(updatedat_ymd, 1, 7),
    year = stringr::str_sub(updatedat_ymd, 1, 4) %>% as.integer(),
  ) %>% 
  dplyr::select(-updatedat_ymd) %>% 
  dplyr::filter(!(`1_interviewer` %in% c("George Stoyle", "test", "Test", "Test 2", "Teste", "Teste 2"))) %>% 
  dplyr::select(c(intersect(names(kobo_data), names(.)), "77_fishing_in_reserve"))

# Pull 2022 Indo records that were in the edited sheet.
# There are 29 records that weren't in the edited sheet. They'll just stay
# in the main dataset.
idn22 <- kobo_data %>% 
  dplyr::filter(submissionid %in% idn_update$submissionid)

# Remove the idn22 rows from hhs
kobo_data <- kobo_data %>% 
  dplyr::filter(!(submissionid %in% idn_update$submissionid))

# Take out the columns from idn22 that we are pushing in from the edit,
# except for submissionid. We'll need that in a join in the next step
# What's left is the columns that were NOT in the edited sheet.
# It's not a lot:
# [1] "iso3"                           "lat"                           
# [3] "lon"                            "calculate_q14_total_proportion"
# [5] "ma_area"                        "ma_status" 
idn22 <- idn22 %>% 
  dplyr::select(-setdiff(names(idn_update), "submissionid"))

# Before joining idn22 and idn_update, I found out that idn_update has a duplicate
# submissionid. It looks like the only difference between the two rows with this
# submissionid is 2_affiliation. We'll just take the one that was updated most recently
dup_id <- "45d59f45-53f5-453d-8ab6-b76dee5b12b1"
most_recent <- idn_update %>% 
  dplyr::filter(submissionid == dup_id) %>% 
  dplyr::pull(updatedat) %>% 
  max()
idn_update <- idn_update %>% 
  dplyr::filter(submissionid != dup_id | updatedat == most_recent)

# Now just join the bit from kobo that has columns missing from the edit
idn22 <- idn22 %>% 
  dplyr::left_join(idn_update, by="submissionid")

# Convert some columns to make sure the types are compatible
kobo_data <- kobo_data %>% 
  dplyr::mutate(
    # The Indo edit introduced a string to a question on a 0-10 scale --
    # "Rules have not been created"
    `75_mgmt_rules_fair` = as.character(`75_mgmt_rules_fair`),
    `13o_no_strategy` = as.logical(`13o_no_strategy`)
  )

q46_cols <- stringr::str_subset(names(idn22), "^46")
na_list <- as.list(rep(as.character(NA), length(q46_cols)))
names(na_list) <- q46_cols

idn22 <- idn22 %>% 
  tidyr::replace_na(na_list) %>% 
  dplyr::mutate(
    `8_religion_other` = as.character(`8_religion_other`),
    `31l_emergency_ngo_group_specify` = as.character(`31l_emergency_ngo_group_specify`),
    `37a_loan_repay_bank` = as.character(`37a_loan_repay_bank`),
    `37b_loan_repay_buyer` = as.character(`37b_loan_repay_buyer`),
    `37c_loan_repay_nonbank` = as.character(`37c_loan_repay_nonbank`),
    `65a_fishers_gear_not_permitted` = as.double(`65a_fishers_gear_not_permitted`),
    `65b_fishers_reserves` = as.double(`65b_fishers_reserves`),
    `65c_fishers_ma_area` = as.double(`65c_fishers_ma_area`),
    `65d_fishers_violate_fish_size` = as.double(`65d_fishers_violate_fish_size`),
    `65e_fishers_caught` = as.double(`65e_fishers_caught`),
    `80_no_wrong_fishing_reserve` = as.double(`80_no_wrong_fishing_reserve`),
  )

kobo_data <- kobo_data %>% 
  dplyr::mutate(
    `13o_no_strategy` = as.logical(`13o_no_strategy`),
  )

kobo_data <- dplyr::bind_rows(kobo_data, idn22) %>% 
  dplyr::distinct()

# Temporary geo info fix
kobo_data <- kobo_data %>% 
  dplyr::mutate(
    maa = dplyr::case_when(
      # These responses for Moz 2023 do have snu/lgu and communities, but the match on the footprint
      # data indicates there are no existing maa's
      country == "Mozambique" & year == 2023 ~ "Vilankulo",
      TRUE ~ maa
    )
  )

usethis::use_data(kobo_data, overwrite=TRUE)