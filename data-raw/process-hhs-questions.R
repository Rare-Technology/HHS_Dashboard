library(dplyr)
library(readxl)

old_hhs_questions <- readr::read_csv("data-raw/hhs_questions.csv")

# Questions added
old_hhs_questions <- old_hhs_questions %>% 
  dplyr::mutate(
    question = ifelse(q_no == 20, question_no_included, question)
  )

old_hhs_questions <- old_hhs_questions %>% 
  dplyr::filter(!is.na(question)) %>%
  dplyr::filter(grepl("[a-z]", question)) %>%
  dplyr::filter(question != "Household Survey Summary") %>%
  dplyr::select(section, q_no, question)



old_hhs_questions$question[old_hhs_questions$question == "42. Do you know how fishing with restricted gear affect the fishery?"] <- 
  "42b. Do you know how fishing with restricted gear affect the fishery?"
old_hhs_questions$question[old_hhs_questions$question == "42. Do you know how fishing undersize fish affect the fishery?"] <- 
  "42c. Do you know how fishing undersize fish affect the fishery?"
old_hhs_questions$question[old_hhs_questions$question == "42. Do you know how fishing inside the reserve affect the fishery?"] <- 
  "42d. Do you know how fishing inside the reserve affect the fishery?"
old_hhs_questions$question[old_hhs_questions$question == "42. Do you know how unauthorized fishers fishing inside the managed access area affect the fishery?"] <- 
  "42e. Do you know how unauthorized fishers fishing inside the managed access area affect the fishery?"

old_hhs_questions$question[old_hhs_questions$question == "66. If people in the community found out that a fisher was fishing in the reserve, would they say or do anything in response?"] <- 
  "66a. If people in the community found out that a fisher was fishing in the reserve, would they say or do anything in response?"

old_hhs_questions$question[old_hhs_questions$question == "66. If yes to previous question. What would people say or do if they found out that a fisher was fishing in the reserve?"] <- 
  "66b. If yes to previous question. What would people say or do if they found out that a fisher was fishing in the reserve?"



old_hhs_questions$q_no <- stringr::str_extract(old_hhs_questions$question, "[a-zA-Z0-9]+(?=[/\\d]*\\.)")

sections <- unique(old_hhs_questions$section)
old_hhs_questions <- purrr::map(sections, function(x) {
  tmp <- dplyr::filter(old_hhs_questions, section == x)
  
  vect <- tmp %>%
    dplyr::pull(q_no) %>%
    paste0("q", .)
  names(vect) <- tmp %>% dplyr::pull(question)
  vect
})
names(old_hhs_questions) <- sections
old_hhs_questions$`Basic Information`[old_hhs_questions$`Basic Information` == "q8"] <- "q08"
old_hhs_questions$`Basic Information`[old_hhs_questions$`Basic Information` == "q9"] <- "q09"


detach(package:dplyr)
usethis::use_data(
  hhs_questions,
  overwrite = TRUE
)

### NEW SURVEY

library(httr)
library(readxl)

httr::GET("https://query.data.world/s/jwd5niku3brhqgcn7smgfev4sxtw2d",
    httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
new_hhs <- readxl::read_excel(tf)

# Remove some junk columns that are not supposed to be there
# George is working on fixing this, so once that's done, this line
# can be removed
new_hhs <- new_hhs[,c(1:431)]

# Remove leading underscores from column names
names(new_hhs) <- stringr::str_sub(names(new_hhs), 2)

# A bit of data cleaning; rename/mutate geographic cols, change
# variable type of some cols (e.g. turn "yes"/"no" into 1/0)

# We'll prepare some stuff for converting yes/no data to 1/0's
yesno_to_bool <- function(yn) {
  if (is.na(yn)) {return(NA)}
  else {
    yn <- stringr::str_to_lower(yn)
    yn_bool <- switch(yn,
      "yes" = 1,
      "no" = 0,
    )
    return(yn_bool)
  }
}
yesno_to_bool <- Vectorize(yesno_to_bool)
yesno_values <- c("yes", "no", NA)
yesno_questions <- new_hhs %>% 
  dplyr::select(
    where(function (x) {
      col_values <- unique(x)
      
      # Some columns are empty; return false on these
      if (is.na(col_values)) {
        return(FALSE)
      }
      
      # Take the unique values of the column. If it has any values beyond
      # yes/no/NA, return FALSE and we'll deal with it later
      not_yesno_values <- dplyr::setdiff(col_values, yesno_values)
      if (length(not_yesno_values) == 0) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
  ) %>% 
  names()

# Now we do the cleaning
new_hhs <- new_hhs %>% 
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
      iso3 == "PHL" ~ "Philippines"
    ),
    updatedat_ymd = lubridate::as_date(updatedat),
    yearmonth = stringr::str_sub(updatedat_ymd, 1, 7),
    year = stringr::str_sub(updatedat_ymd, 1, 4) %>% as.integer(),
  ) %>% 
  dplyr::mutate(
    dplyr::across(yesno_questions, yesno_to_bool)
  ) %>% 
  dplyr::select(-updatedat_ymd)

# Skipping this cleaning step until it looks helpful to do this
# # Create 4_ma_r_mb col from values of four related columns
# ma_r_mb_vec <- c("marine reserve", "management area", "management body", "none")
# new_hhs$`4_ma_r_mb` <- purrr::pmap(
#     new_hhs %>% dplyr::select(
#       mr = `4_ma_r_mb/marine_reserve`,
#       ma = `4_ma_r_mb/management_area`,
#       mb = `4_ma_r_mb/management_body`,
#       no = `4_ma_r_mb/none`
#     ),
#     function (mr, ma, mb, no) {
#       resp <- c(mr, ma, mb, no)
#       # resp is a row that looks like 0 1 1 0
#       # this example would yield "management area,management body"
#       
#       # first, deal with NA
#       if (any(is.na(resp)) | all(resp == 0)) {return(NA)}
#       
#       # next, convert resp to boolean and make a string based on that
#       resp <- as.logical(resp) # e.g. 0 1 0 1 becomes FALSE TRUE FALSE TRUE
#       out <- paste0(ma_r_mb_vec[resp], collapse = ",")
#       return(out)
#     }
#   )

### Add geographic info
# The only geographic columns are made up of id's, and there is not info for
# the maa. So let's add names and maa info.

geo <- readr::read_csv("https://query.data.world/s/gp7hblhjzx3mx3w54yutspjnzgqi2f")
maa <- readr::read_csv("https://query.data.world/s/eyvv3fdcwk7yjzhe24tt5sck2ousuj")

# First, there's 5 rows missing all level1_id, level2_id, and level4_id.
# Nothing we can really do there.
# But, there are 14 rows missing only leve1_id, with level2_id and level4_id OK
# So let's join on these two columns to extract the missing level1_id's

new_hhs <- new_hhs %>% 
  dplyr::select(-level1_id) %>% 
  dplyr::left_join(
    geo %>% dplyr::select(
      level2_id, level4_id, level1_id
    )
  )

# Now add names for level1/2/4
new_hhs <- new_hhs %>% 
  dplyr::left_join(
    geo %>% dplyr::select(
      level1_id, subnational = level1_name,
      level2_id, local = level2_name,
      level4_id, `3_community` = level4_name
    ),
    by = c("level1_id", "level2_id", "level4_id")
  )

# Finally, add maa info like name, status, and area. Remove cols we won't need.
new_hhs <- new_hhs %>%
  dplyr::left_join(
    maa %>% dplyr::select(
      level1_id, level2_id, level4_id,
      maa = ma_name, ma_area, ma_status),
    by = c("level1_id", "level2_id", "level4_id")
  ) %>% 
  dplyr::select(-c(
    level1_id,
    level2_id,
    level4_id,
    tore_gps,
    store_gps_altitude,
    store_gps_precision,
    `94_yes_no`,
    id,
    # Basically all the note columns are blank
    stringr::str_subset(names(.), "note_")
  ))

### Moving on to the question mapping: Extract useful text from column names
# sometimes it's clear what the mapping is from an old survey question to a new
# survey question because the descriptive text is the same.
# For example, the old 11a_months_farming matches the new 14a_months_farming
# because of the "months_farming" text.
# We will match as many questions as we can with this strategy.
# To do so, we first need to pull this descriptive text for each question.
# Basically, go from this:
# c("8_religion", "8_religion_other", ...)
# to this:
# c("religion", "religion_other")
# We'll do this for the old survey questions as well as the new ones
remove_leading_num <- function (q) {
  if (stringr::str_detect(q, "^[0-9]")) {
    tokens <- stringr::str_split(q, "_", simplify = TRUE)[-1]
    newstr <- paste0(tokens, collapse = "_")
  } else {
    newstr <- q
  }
  return(newstr)
}

q_text <- purrr::map(
  names(hhs_data),
  remove_leading_num
) %>% unlist()

new_q_text <- purrr::map(
  names(new_hhs),
  remove_leading_num
) %>% unlist()

### Mapping using descriptive text
# Now we can map based on matching column text.
q_mapping <- purrr::imap(
  names(hhs_data),
  function(q, i) {
    txt <- q_text[i]
    if (any(txt == new_q_text)) {
      q_new <- names(new_hhs)[txt == new_q_text]
    } else {
      q_new <- NA
    }
    
    return(q_new)
  }
)

# Change output names, so we can do something like
# > q_mapping[["11a_months_farming"]] # the old column
# [1] "14a_months_farming" # the new column
names(q_mapping) <- names(hhs_data)
old_names <- names(hhs_data)
new_names <- purrr::map(old_names, function (x) {
  if (is.na(q_mapping[[x]])) {
    return(x)
  } else {
    return(q_mapping[[x]])
  }
}) %>% unlist()
# Rename old columns using the mapper we just made
hhs_data_update <- hhs_data %>% dplyr::rename_with(~ new_names, dplyr::all_of(old_names))

### Mapping remaining columns
# After going through the output of q_mapping and cross-checking with the
# mapping spreadsheet, we have to manually rename some columns to make
# the mapping work
hhs_data_update <- hhs_data_update %>% 
  dplyr::rename(
    `14d_months_fishing_aquaculture` = `11g_months_aquaculture`,
    `14d_income_fishing_aquaculture` = `11g_income_aquaculture`,
    `14j_months_industrial` = `11d_months_fishing_industrial`,
    `14j_income_industrial` = `11d_income_fishing_industrial`,
    `27a_financial_bank/yes_gender_unspecified` = `25a_financial_bank`,
    `27b_financial_micro/yes_gender_unspecified` = `25b_financial_micro`,
    `27c_financial_ngo/yes_gender_unspecified` = `25c_financial_ngo`,
    `35c_other_nonbank` = `25d_financial_lender`,
    `33_hh_insurance/yes_unspecified` = `25e_financial_insurance`,
    `30_save_monthly_income` = `26_fishing_income_save`,
    `46_ma_gear_trap` = `35l_ma_gear_traps`,
    `58_represent_role` = `47_represent_contributions`,
    `69a_rules_benefit_licensing` = `42a_problem_regulation`,
    `69b_rules_benefit_fishing_permission` = `42e_problem_unauthorized`,
    `69c_rules_benefit_gear` = `42b_problem_restricted_gear`,
    `69d_rules_benefit_reserve_restrict` = `42d_problem_inside_reserve`,
    `69e_rules_benefit_min_fish_size` = `42c_problem_undersize`,
    `74i_change_fishing_behavior` = `61f_fishing_change_behavior`,
    `76_complies_reserve` = `62_reserve_compliance`,
    `78d_violate_reserve_fishing` = `66_reaction_fishing_reserve`
  ) %>% 
  dplyr::select(
    -c(username, username_2, level2_name, level4_id)
  )

### Splitting up Q4, Q44, Q45, and Q48 into columns in the new survey

# Q4 -> Q4
q4_data <- purrr::map_dfr(hhs_data_update$`4_ma_r_mb`, function(tb) {
  tb_data <- tb[[1]]
  tb_string <- stringr::str_split(tb_data, ",", simplify=TRUE)
  mr <- ifelse("marine reserve" %in% tb_string, 1, 0)
  ma <- ifelse("management area" %in% tb_string, 1, 0)
  mb <- ifelse("management body" %in% tb_string, 1, 0)
  none <- ifelse("none" %in% tb_string, 1, 0)
  
  return(list(
    "4_ma_r_mb/marine_reserve" = mr,
    "4_ma_r_mb/management_area" = ma,
    "4_ma_r_mb/management_body" = mb,
    "4_ma_r_mb/none" = none
  ))
})
hhs_data_update <- cbind(hhs_data_update, q4_data)

# Q44 -> Q54
# 44_meeting_attendance is a bunch of tibbles, difficult to work with
# so we will convert it to a bunch of strings instead
# ironically, this was a pain in the ass to do with purrr, so implementing as a
# for loop instead
out44 <- c()
for (i in 1:nrow(hhs_data_update)) {
  out44 <- c(out44, paste0(hhs_data_update$`44_meeting_attendance`[[i]][[1]], collapse = ","))
}
hhs_data_update$`44_meeting_attendance` <- out44

q54_data <- purrr::map_dfr(hhs_data_update$`44_meeting_attendance`, function(tb) {
  tb_data <- tb[[1]]
  tb_string <- stringr::str_split(tb_data, ",", simplify=TRUE)
  yesmale <- ifelse("Yes male" %in% tb_string, 1, 0)
  yesfemale <- ifelse("Yes female" %in% tb_string, 1, 0)
  yesboth <- NA # What does "yes_both_gender" mean when there's no correlation
  # between it and yes_male/yes_female????
  yesnb <- NA # No nonbinary option in old survey
  no <- ifelse("No" %in% tb_string, 1, 0)
  nomgmt <- ifelse("No management" %in% tb_string, 1, 0)
  nomeeting <- NA # No "No meeting" option in old survey
  
  return(list(
    "54_fisheries_management_meeting/yes_male" = yesmale,
    "54_fisheries_management_meeting/yes_female" = yesfemale,
    "54_fisheries_management_meeting/yes_nonbinary" = yesnb,
    "54_fisheries_management_meeting/yes_both_gender" = yesboth,
    "54_fisheries_management_meeting/no_meetings" = nomeeting,
    "54_fisheries_management_meeting/no_mgmt" = nomgmt,
    "54_fisheries_management_meeting/no" = no
  ))
})
hhs_data_update <- cbind(hhs_data_update, q54_data)

# Q45 -> Q57
out45 <- c()
for (i in 1:nrow(hhs_data_update)) {
  out45 <- c(out45, paste0(hhs_data_update$`45_leadership_position`[[i]][[1]], collapse = ","))
}
hhs_data_update$`45_leadership_position` <- out45

q57_data <- purrr::map_dfr(hhs_data_update$`45_leadership_position`, function(tb) {
  tb_data <- tb[[1]]
  tb_string <- stringr::str_split(tb_data, ",", simplify=TRUE)
  yesmale <- ifelse("Yes male" %in% tb_string, 1, 0)
  yesfemale <- ifelse("Yes female" %in% tb_string, 1, 0)
  yesnb <- NA # No nonbinary option in old survey
  no <- ifelse("No" %in% tb_string, 1, 0)
  nomgmt <- ifelse("No management" %in% tb_string, 1, 0)

  return(list(
    "57_fisheries_management_leader/yes_male" = yesmale,
    "57_fisheries_management_leader/yes_female" = yesfemale,
    "57_fisheries_management_leader/yes_nonbinary" = yesnb,
    "57_fisheries_management_leader/no_mgmt" = nomgmt,
    "57_fisheries_management_leader/no" = no
  ))
})
hhs_data_update <- cbind(hhs_data_update, q57_data)

# Q48 -> Q62
out48 <- c()
for (i in 1:nrow(hhs_data_update)) {
  out48 <- c(out48, paste0(hhs_data_update$`48_enforcement_participation`[[i]][[1]], collapse = ","))
}
hhs_data_update$`48_enforcement_participation` <- out48

q62_data <- purrr::map_dfr(hhs_data_update$`48_enforcement_participation`, function(tb) {
  tb_data <- tb[[1]]
  tb_string <- stringr::str_split(tb_data, ",", simplify=TRUE)
  yesmale <- ifelse("Yes male" %in% tb_string, 1, 0)
  yesfemale <- ifelse("Yes female" %in% tb_string, 1, 0)
  yesboth <- NA
  yesnb <- NA # No nonbinary option in old survey
  noenforcement <- ifelse(any(c("No enforcement system", "There is no enforcement system") %in% tb_string), 1, 0)
  other <- NA
  otherspecify <- NA
  
  # There is no column for just "No" ...
  return(list(
    "62_enforcement_participation/yes_male" = yesmale,
    "62_enforcement_participation/yes_female" = yesfemale,
    "62_enforcement_participation/yes_both_gender" = yesboth,
    "62_enforcement_participation/yes_nonbinary" = yesnb,
    "62_enforcement_participation/no_enforcement" = noenforcement,
    "62_enforcement_participation/other" = other,
    "62_enforcement_participation_other_specify" = otherspecify
  ))
})
hhs_data_update <- cbind(hhs_data_update, q62_data)

# Q49 -> Q63
# The values for 49_enforcement_responsible don't need to be cleaned like
# the previous questions; the values are already strings
q63_data <- purrr::map_dfr(hhs_data_update$`49_enforcement_responsible`, function(tb) {
  tb_data <- tb[[1]]
  tb_string <- stringr::str_split(tb_data, ",", simplify=TRUE)
  fmb <- ifelse("Fisheries Management Body" %in% tb_string, 1, 0)
  national <- ifelse("National Government" %in% tb_string, 1, 0)
  subnational <- ifelse("Subnational Government" %in% tb_string, 1, 0)
  noenforcement <- ifelse(any(c("No enforcement system", "There is no enforcement system") %in% tb_string), 1, 0)
  # These are the only values that look like they fit 'other':
  other <- ifelse(
    any(c("Community Bodies", "Myself", "Other") %in% tb_string), 1, 0
  )
  # Only put Community Bodies or Myself in the 'specify' column. Otherwise NA
  otherspecify <- ifelse("Community Bodies" %in% tb_string, "Community Bodies",
    ifelse("Myself" %in% tb_string, "Myself", NA))
  
  return(list(
    "63_enforcement_responsible/fmb" = fmb,
    "63_enforcement_responsible/national" = national,
    "63_enforcement_responsible/subnational" = subnational,
    "63_enforcement_responsible/no_enforcement" = noenforcement,
    "63_enforcement_responsible/other" = other,
    "63_enforcement_responsible_specify" = otherspecify
  ))
})
hhs_data_update <- cbind(hhs_data_update, q63_data)

# Drop all the columns we just used to build these past few columns
hhs_data_update <- hhs_data_update %>% 
  dplyr::select(-c(`4_ma_r_mb`, `44_meeting_attendance`,
                   `45_leadership_position`, `48_enforcement_participation`,
                   `49_enforcement_responsible`
  ))

# Now add every other column; these  are columns that simply have no mapping to
# the old survey (like COVID questions)
remaining_cols <- setdiff(names(new_hhs), names(hhs_data_update))
hhs_data_update[remaining_cols] <- NA

### Add some cols to new_hhs for compatability
# While updating the previous survey data, we added some columns like
# 27a_financial_bank/yes_gender_unspecified, which is not in the new survey
# This was to be compatible with the columns of the new survey, like
# 27a_financial_bank/yes_female
# We will have to add these to the new survey dataset
new_hhs <- new_hhs %>%
  dplyr::mutate(
    `27a_financial_bank/yes_gender_unspecified` = NA,
    `27b_financial_micro/yes_gender_unspecified` = NA,
    `27c_financial_ngo/yes_gender_unspecified` = NA,
    `33_hh_insurance/yes_unspecified` = NA,
    `18_hh_main_fisher` = dplyr::recode(`18_hh_main_fisher`,
      "enterprise_owner" = "Enterprise owner",
      "independently" = "Independently",
      "other" = "Other",
      "salaried_laborer" = "Salaried laborer",
      "cooperative_member" = "Cooperative member",
      "na" = NULL
    )
  )

# See above comments for missing columns !!! Some are indefinitely dropped,
# a few still need to be added (like Q15 -> Q17)
old_dropped_cols <- setdiff(names(hhs_data_update), names(new_hhs))
hhs_data_update <- hhs_data_update %>% dplyr::select(-dplyr::all_of(old_dropped_cols))


### Clean some new_hhs columns
# E.g. change 10_mpa_important values from "neutral", "yes", "no" to 0, 1, -1
# This matches the old survey's values and is more useful for counting
hhs_data_update <- hhs_data_update %>% 
  dplyr::mutate(
    `10_mpa_important` = dplyr::recode(`10_mpa_important`,
      `1` = "yes",
      `0` = "no",
      `-1` = "neutral"
    ),
    dplyr::across(
      stringr::str_subset(names(hhs_data_update), "^22[a-f]"),
      ~ dplyr::recode(.x, `1` = "yes", `0` = "no")
    ),
    `22g_gear_other` = dplyr::recode(`22g_gear_other`,
      `1` = "yes",
      `0` = "no"
    ),
    `25_job_secure` = dplyr::recode(`25_job_secure`,
      `1` = "yes",
      `0` = "no"
    ),
    # Q41: Careful with the old survey; the old survey responses were on a 
    # strongly disagree - strongly agree scale as well but included a 6th option,
    # which I THINK is "I don't depend on or benefit from the fishery"
    # So the old survey data will have values 1-6, the new survey 1-5.
    dplyr::across(
      stringr::str_subset(names(hhs_data_update), "^41[a-d]"),
      ~ dplyr::recode(.x,
        `1` = "strongly_disagree",
        `2` = "disagree",
        `3` = "neither",
        `4` = "agree",
        `5` = "strongly_agree",
        `6` = NULL
      )
    ),
    `43_fishery_benefit_equal` = dplyr::recode(`43_fishery_benefit_equal`,
      `1` = "yes",
      `0` = "no",
      `-1` = "no_dependence"
    ),
    `45_gear_restrictions` = dplyr::recode(`45_gear_restrictions`,
      `1` = "yes",
      `0` = "no",
      `-1` = NULL
    ),
    `48_reserve_fishing_allowed` = dplyr::recode(`48_reserve_fishing_allowed`,
       `1` = "yes",
       `0` = "no",
       `-1` = "no_reserves"
    ),
    `49_reserve_boundry` = dplyr::recode(`49_reserve_boundry`,
      `1` = "yes",
      `0` = "no",
      `-1` = "no_reserves"
    ),
    `51_reserve_boundaries_aware` = dplyr::case_when(
      is.numeric(`51_reserve_boundaries_aware`) ~ as.character(`51_reserve_boundaries_aware`)
    ),
    dplyr::across(
      stringr::str_subset(names(new_hhs), "^74[a-g,i]"),
      ~ dplyr::recode(.x,
        `1` = "strongly_disagree",
        `2` = "disagree",
        `3` = "neither",
        `4` = "agree",
        `5` = "strongly_agree",
        `0` = "na",
        `-1` = "na"
      )
    ),
    `77_fishing_in_reserve` = dplyr::recode(`77_fishing_in_reserve`,
      `1` = "yes",
      `0` = "no",
      `-1` = NULL
    ),
    updatedat_ymd = lubridate::as_date(updatedat),
    yearmonth = stringr::str_sub(updatedat_ymd, 1, 7)
  ) %>% 
  dplyr::select(-updatedat_ymd)

hhs_data_update$survey <- "FF 2.0"
new_hhs$survey <- "2022 Revision"

hhs_final <- dplyr::bind_rows(hhs_data_update, new_hhs)
readr::write_csv(hhs_final, "household_surveys_2022_update.csv")


################################ NOTES ########################################
#
#### Not in new survey:
# 14_reponsibility 14_responsibilities_other
#
# 24i_outboard_no 24i_outboard_value 24j_inboard_no 24j_inboard_value
#
# 29_family_income but maybe this is ok since, as previously mentioned,
# it was kind of redundant with 77_ends_meet
#
# 30_trust_fishers_community 30_trust_fishers_other 30_trust_ngo
# 30_trust_reglious_leaders 30_trust_village_alert
# Fortunately, except for 30_village_alert, these were not used
# on the dashboard.
#
# Q35 other
# Did try to salvage the 35k_ma_gear_other, but the way the question is set up
# on the new survey, 46_ma_gear_other specifies permitted/not permitted and 
# 46_ma_gear_other_specify specifies what the gear is.
# 35k_ma_gear_other is a mix... sometimes it just says the gear, sometimes it says
# "X is not allowed" So we're just dropping it.
#
# 36_fish_size_restriction
#
# 46_represent_interests
#
# 56_reduce_meal_size_adult 57_reduce_meal_size_frequency 58_reduce_meal_size_child
#
# 63_times_fishing_reserve (but 63_fishing_in_reserve OK)
#
# 64_wrong_fishing_reserve
#
# 66_response_fishing_reserve (see note on Q66)
#
# all cols for Q69. Q69 is supposed to map to the new Q82, but Q82 is not
# in the dataset yet ! But 69 wasn't on the dashboard anyway....
#
#
#
#### Other notes:
# The different categories for Q25 have been split up by gender.
# E.g. 25a_financial_bank is now 27a_financial_bank/yes_male,
# 27a_financial_bank/yes_female, 27a_financial_bank/yes_nonbinary,
# and 27a_financial_bank/no
# Since the original responses are not specified by gender, the columns were
# mapped to "gender unspecified" categories:
# 25a_financial_bank -> 27a_financial_bank/gender_unspecified
# This new column name doesn't exist on the new hhs though.
#
# 28_buyer_loans and 28_loan_purpose tie into the new 35 I think.
# Have to tie them in somehow, but for now will drop them.
#
# Q66 was specifically about fishing in the reserve, the new Q78 asks about
# fishing in the reserve and more. So old Q66 corresponds to new Q78d. The
# other column in 66, 66_response_fishing_reserve, is just a yes/no kind of
# question which is not used anymore.
#
#
#
#### TODO
# 
# 15_activity (this might be the other sheets in the new hhs xlsx)