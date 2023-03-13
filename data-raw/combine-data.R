library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(purrr)
library(data.world)

#### Check data.world auth ####
# Let's get this out of the way before getting an error at the very end when we upload to data.world.
# Make sure you have your data.world token somewhere. I keep mine in .Renviron in the main project
# directory. If you run the next couple lines and see some metadata about the hh surveys dataset
# on data.world, you're good to go! Otherwise check your auth token/environment variable setup.

DW_TOKEN <- Sys.getenv("DW_TOKEN")
dwapi::configure(DW_TOKEN)
dwapi::get_dataset("rare", "hh-surveys") %>% head()

#### Update legacy dataset ####
# Get most recent data from old survey. In theory this shouldn't be necessary at
# some point but as of right now we still get some old survey samples.
source("data-raw/process-raw-legacy-data.R")
devtools::document() # Need to document hhs_data for processing kobo data next
rm(list=ls())

#### Update kobo dataset ####
source("data-raw/process-raw-kobo-data.R")
devtools::document()
rm(list=ls())

#### Question mapping ####
# Extract useful text from column names
# Sometimes it's clear what the mapping is from an old survey question to a new
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
  names(legacy_data),
  remove_leading_num
) %>% unlist()

kobo_q_text <- purrr::map(
  names(kobo_data),
  remove_leading_num
) %>% unlist()

### Mapping using descriptive text
# Now we can map based on matching column text.
q_mapping <- purrr::imap(
  names(legacy_data),
  function(q, i) {
    txt <- q_text[i]
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
names(q_mapping) <- names(legacy_data)
legacy_names <- names(legacy_data)
kobo_names <- purrr::map(legacy_names, function (x) {
  if (is.na(q_mapping[[x]])) {
    return(x)
  } else {
    return(q_mapping[[x]])
  }
}) %>% unlist()
# Rename old columns using the mapper we just made
legacy_data <- legacy_data %>% dplyr::rename_with(~ kobo_names, dplyr::all_of(legacy_names))

#### Mapping remaining columns ####
# After going through the output of q_mapping and cross-checking with the
# mapping spreadsheet, we have to manually rename some columns to make
# the mapping work
legacy_data <- legacy_data %>% 
  # Dropping 77 because will have another column take that name
  dplyr::select(-`77_fishing_in_reserve`) %>% 
  dplyr::rename(
    `14d_months_fishing_aquaculture` = `11g_months_aquaculture`,
    `14d_income_fishing_aquaculture` = `11g_income_aquaculture`,
    `14j_months_industrial` = `11d_months_fishing_industrial`,
    `14j_income_industrial` = `11d_income_fishing_industrial`,
    `27a_financial_bank/yes_gender_unspecified` = `25a_financial_bank`,
    `27b_financial_micro/yes_gender_unspecified` = `25b_financial_micro`,
    `27c_financial_ngo/yes_gender_unspecified` = `25c_financial_ngo`,
    `27d_financial_other_specify` = `25f_financial_other`,
    `30_save_monthly_income` = `26_fishing_income_save`,
    `33_hh_insurance/yes_unspecified` = `25e_financial_insurance`,
    `35b_buyer` = `28_buyer_loans`,
    `35c_other_nonbank` = `25d_financial_lender`,
    `46_ma_gear_trap` = `35l_ma_gear_traps`,
    `58_represent_role` = `47_represent_contributions`,
    `61_opinions_considered` = `46_represent_interests`,
    `69a_rules_benefit_licensing` = `42a_problem_regulation`,
    `69b_rules_benefit_fishing_permission` = `42e_problem_unauthorized`,
    `69c_rules_benefit_gear` = `42b_problem_restricted_gear`,
    `69d_rules_benefit_reserve_restrict` = `42d_problem_inside_reserve`,
    `69e_rules_benefit_min_fish_size` = `42c_problem_undersize`,
    `74i_change_fishing_behavior` = `61f_fishing_change_behavior`,
    `76_complies_reserve` = `62_reserve_compliance`,
    `77_fishing_in_reserve` = `63_times_fishing_reserve`,
    `78d_violate_reserve_fishing` = `66_reaction_fishing_reserve`
  ) %>% 
  dplyr::select(
    -c(username, username_2, level2_name, level4_id)
  )

#### 27d_financial_other ####
# Q27 asks if the respondent or their family has an account with a financial
# institution. There is an "other" option and the respondent can specify if they
# check off "other". So we end up with these columns:
# [1] "27d_financial_other/yes_female"    "27d_financial_other/yes_male"     
# [3] "27d_financial_other/yes_nonbinary" "27d_financial_other/no"           
# [5] "27d_financial_other_specify" 
# The first four are just yes/no. The last column is text to explain their response
# In the old survey, we get both columns (yes/no and the explanation) in one:
# (old) 25f_financial_other
# Essentially, if this column was left blank, the answer is no. If it is filled in,
# the answer is yes and the explanation is given. Answers to this column were examined
# to see what counts as having an alternative to a bank account (common "yes" answers
# include rotating savings and credit associations, a mobile payment app, or savings club.
# common "no" answers are something like "at home").
# This information was compiled in a spreadsheet and we will use that to appropriately
# map the old survey responses to the new survey.
hhs25f <- readxl::read_excel("../data/HHS/hhs-25f.xlsx") %>% 
  dplyr::select(
    `27d_financial_other_specify` = `25f_financial_other`,
    `27d_yesno` = `yes/no`
  )

legacy_data <- dplyr::left_join(legacy_data, hhs25f, by = "27d_financial_other_specify") %>% 
  dplyr::mutate(
    `27d_financial_other/yes_gender_unspecified` = dplyr::case_when(
      `27d_yesno` == 1 ~ 1,
      TRUE ~ 0
    ),
    `27d_financial_other/no` = dplyr::case_when(
      `27d_yesno` == 0 | is.na(`27d_financial_other_specify`) ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  dplyr::select(-`27d_yesno`)
rm(hhs25f)

#### Removing 46_ma_gear_other ####
# Old 35_ma_gear_other was mapped to new 46_ma_gear_other but the information 
# that these columns provide is different. The new 46_ma_gear_other simply says if
# the gear specified in 46_ma_gear_other_specify is permitted or not. The old
# 35_ma_gear_other is a mix of these two columns: some entries will say something
# like "X is not permitted" while other entries will just specify a gear type and
# not say if its permitted or not. Because of this inconsistency, we will simply
# not carry forward the old 36_ma_gear_other
legacy_data$`46_ma_gear_other` <- as.character(NA)

#### Pivoting some columns in the old survey data ####
# There are some questions that, in the new survey data structure, there are multiple indicator
# columns (1's and 0's) to indicate if an answer was selected. These are usually multiple choice
# questions, so sometimes the column has multiple values separated by commas.
# So instead of having something like
#
# # A tibble: 6 x 1
# X_some_question                                        
# <chr>
# 1 val1
# 2 val2
# 3 val1,val2
# 4 val3
# 5 NA
# 6 val1,val2,val3
#
# We would have
#
# # A tibble: 6 x 3
#   X_some_question/val1    X_some_question/val2  X_some_question/val3                                        
#   <dbl>                   <dbl>                 <dbl>
# 1 1                       0                     0
# 2 0                       1                     0
# 3 1                       1                     0
# 4 0                       0                     1
# 5 0                       0                     0
# 6 1                       1                     1
#
# We need to transform some columns from the legacy data into this form.
#
# Q4 -> Q4
q4_data <- purrr::map_dfr(legacy_data$`4_ma_r_mb`, function(tb) {
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
legacy_data <- cbind(legacy_data, q4_data)

# Q44 -> Q54
# 44_meeting_attendance is a bunch of tibbles, difficult to work with
# so we will convert it to a bunch of strings instead
# ironically, this was a pain in the ass to do with purrr, so implementing as a
# for loop instead
out44 <- c()
for (i in 1:nrow(legacy_data)) {
  out44 <- c(out44, paste0(legacy_data$`44_meeting_attendance`[[i]][[1]], collapse = ","))
}
legacy_data$`44_meeting_attendance` <- out44

q54_data <- purrr::map_dfr(legacy_data$`44_meeting_attendance`, function(tb) {
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
legacy_data <- cbind(legacy_data, q54_data)

# Q45 -> Q57
out45 <- c()
for (i in 1:nrow(legacy_data)) {
  out45 <- c(out45, paste0(legacy_data$`45_leadership_position`[[i]][[1]], collapse = ","))
}
legacy_data$`45_leadership_position` <- out45

q57_data <- purrr::map_dfr(legacy_data$`45_leadership_position`, function(tb) {
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
legacy_data <- cbind(legacy_data, q57_data)

# Q48 -> Q62
out48 <- c()
for (i in 1:nrow(legacy_data)) {
  out48 <- c(out48, paste0(legacy_data$`48_enforcement_participation`[[i]][[1]], collapse = ","))
}
legacy_data$`48_enforcement_participation` <- out48

q62_data <- purrr::map_dfr(legacy_data$`48_enforcement_participation`, function(tb) {
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
legacy_data <- cbind(legacy_data, q62_data)

# Q49 -> Q63
# The values for 49_enforcement_responsible don't need to be cleaned like
# the previous questions; the values are already strings
q63_data <- purrr::map_dfr(legacy_data$`49_enforcement_responsible`, function(tb) {
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
legacy_data <- cbind(legacy_data, q63_data)

# Drop all the columns we just used to build these past few columns
legacy_data <- legacy_data %>% 
  dplyr::select(-c(`4_ma_r_mb`, `44_meeting_attendance`,
                   `45_leadership_position`, `48_enforcement_participation`,
                   `49_enforcement_responsible`
  ))

# Now add every other column; these are columns that simply have no mapping to
# the old survey (like COVID questions)
remaining_cols <- setdiff(names(kobo_data), names(legacy_data))
legacy_data[remaining_cols] <- NA

#### Add and recode some cols to kobo_data for compatibility ####
# While updating the previous survey data, we added some columns like
# 27a_financial_bank/yes_gender_unspecified, which is not in the new survey
# This was to be compatible with the columns of the new survey, like
# 27a_financial_bank/yes_female
# We will have to add these to the new survey dataset.
# Additionally, we will recode some columns to standardize answers between the
# old and new surveys. 

# As part of the recoding, we want to first collect all the columns
# that have just yes/no responses with the goal of going from
# Yes, No, yes, no, NA
# to
# Yes, No, NA
yesno_questions <- c()
# "Why didn't you just use purrr::map lol" idk why don't you do it then??
for (x in names(kobo_data)) {
  unique_values <- unique(kobo_data[[x]])
  
  # Ignore columns that are blank and only take the ones with yes/no values plus NA
  if (!all(is.na(unique_values)) & all(unique_values %in% c("no", "No", "yes", "Yes", NA))) {
    yesno_questions <- c(yesno_questions, x)
  }
}

kobo_data <- kobo_data %>%
  dplyr::mutate(
    `27a_financial_bank/yes_gender_unspecified` = NA,
    `27b_financial_micro/yes_gender_unspecified` = NA,
    `27c_financial_ngo/yes_gender_unspecified` = NA,
    `27d_financial_other/yes_gender_unspecified` = NA,
    `33_hh_insurance/yes_unspecified` = NA,
    dplyr::across(
      yesno_questions,
      ~ dplyr::recode(
        .x,
        "no" = "No",
        "yes" = "Yes"
      )
    ),
    `6_gender` = dplyr::recode(
      `6_gender`,
      "f" = "F",
      "Female" = "F",
      "m" = "M",
      "Male" = "M",
      "nb" = "NB",
      "Non-Binary" = "NB"
    ),
    `8_religion` = dplyr::recode(
      `8_religion`,
      "other" = "Other",
      "muslim" = "Muslim",
      "catholic" = "Catholic",
      "hindu" = "Hindu",
      "buddhist" = "Buddhist",
      "christian" = "Christian",
      "traditional" = "Traditional",
      "jewish" = "Jewish"
    ),
    `10_mpa_important` = dplyr::recode(
      `10_mpa_important`,
      "yes" = "Yes",
      "no" = "No",
      "neutral" = "Neutral"
    ),
    `18_hh_main_fisher` = dplyr::recode(
      `18_hh_main_fisher`,
      "enterprise_owner" = "Enterprise owner",
      "independently" = "Independently",
      "other" = "Other",
      "salaried_laborer" = "Salaried laborer",
      "cooperative_member" = "Cooperative member",
      "na" = as.character(NA)
    ),
    `19_fishing_low_profit` = dplyr::recode(
      `19_fishing_low_profit`,
      "one_two_per_week" = "1-2 per week",
      "few times_month" = "A few times per month",
      "once_or_never" = "Once or never",
      "few_times_season" = "A few times", # accurate?
      "more_than_one_two_week" = "More than 1-2 times per week"
    ),
    `20_fishing_high_profit` = dplyr::recode(
      `20_fishing_high_profit`,
      "more_than_one_two_week" = "More than 1-2 times per week",
      "few_times_season" = "A few times",
      "once_or_never" = "Once or never",
      "few times_month" = "A few times per month",
      "one_two_per_week" = "1-2 per week"
    ),
    `21_current_fish_catch` = dplyr::recode(
      `21_current_fish_catch`,
      "improved_slightly" = "Improved slightly",
      "declined_slightly" = "Declined slightly",
      "stayed_the_same" = "Stayed the same",
      "declined_alot" = "Declined a lot",
      "improved_heavily" = "Improved heavily"
    ),
    `23_boat_owner_status` = dplyr::recode(
      `23_boat_owner_status`,
      "not_by_foat" = "Not by boat",
      "employee" = "Employee",
      "own" = "Own",
      "collective" = "Collective",
      "rent" = "Rent"
    ),
    `24_catch_5yrs` = dplyr::recode(
      `24_catch_5yrs`,
      "improves_heavily" = "Improves heavily",
      "improves_slightly" = "Improves slightly",
      "stays_the_same" = "Stays the same",
      "declines_slightly" = "Declines slightly",
      "declines_alot" = "Declines a lot"
    ),
    dplyr::across(
      stringr::str_subset(names(kobo_data), "^41[a-f]"),
      ~ dplyr::recode(
        .x,
        "strongly_disagree" = "Strongly disagree",
        "disagree" = "Disagree",
        "neither" = "Neither",
        "agree" = "Agree",
        "strongly_agree" = "Strongly agree",
        "Neither agree nor disagree" = "Neither"
      )
    ),
    `42_my_community_ability` = dplyr::recode(
      `42_my_community_ability`,
      "strongly_disagree" = "Strongly disagree",
      "disagree" = "Disagree",
      "neither" = "Neither",
      "agree" = "Agree",
      "strongly_agree" = "Strongly agree",
      "Neither agree nor disagree" = "Neither"
    ),
    `43_fishery_benefit_equal` = dplyr::recode(
      `43_fishery_benefit_equal`,
      "yes" = "Yes",
      "no" = "No",
      "no_dependence" = "I don't depend on or benefit from the fishery"
    ),
    `45_gear_restrictions` = dplyr::recode(
      `45_gear_restrictions`,
      "yes" = "Yes",
      "no" = "No",
      "na" = as.character(NA)
    ),
    dplyr::across(
      stringr::str_subset(names(kobo_data), "^46"),
      ~ dplyr::recode(
        .x,
        "permitted" = "Permitted",
        "not_permitted" = "Not permitted",
        "unknown" = "Unknown",
        "na" = as.character(NA)
      )
    ),
    `48_reserve_fishing_allowed` = dplyr::recode(
      `48_reserve_fishing_allowed`,
      "no" = "No",
      "yes" = "Yes",
      "no_reserves" = "Community does not have reserves"
    ),
    `52_ma_fishers_allowed` = dplyr::recode(
      `52_ma_fishers_allowed`,
      "community_only" = "Community only",
      "idk" = "Don't know",
      "no_managed_area" = "No managed access",
      "no_restrictions" = "No restrictions",
      "with_authorization" = "With authorization",
      "without_authorization" = "Without authorization"
    ),
    `58_represent_role` = dplyr::recode(`58_represent_role`,
      "agree" = "Agree",
      "disagree" = "Disagree",
      "neither" = "Neither",
      "Neither agree nor disagree" = "Neither",
      "no_mgmt" = "No management body"
    ),
    `61_opinions_considered` = dplyr::recode(
      `61_opinions_considered`,
      "very_low" = "Very low",
      "low" = "Low",
      "neither" = "Neither strong or low",
      "strong" = "Strong",
      "very_strong" = "Very strong",
      "no_mgmt" = "No management body"
    ),
    `64_ma_punishment` = dplyr::recode(
      `64_ma_punishment`,
      "no_punishment" = "No punishment",
      "minor" = "Minor",
      "moderate" = "Moderate",
      "major" = "Major",
      "extreme" = "Extreme",
      "no_regs" = "No regulations",
      "na" = as.character(NA)
    ),
    dplyr::across(
      stringr::str_subset(names(legacy_data), "^65[a-e]"),
      ~ dplyr::recode(
        .x,
        `-1` = as.double(NA) 
      )
    ),
    `66_ma_benefit_5yrs` = dplyr::recode(
      `66_ma_benefit_5yrs`,
      "yes" = "Yes",
      "no" = "No",
      "no_mgmt" = "No management",
      "unsure" = "Unsure",
      "na" = as.character(NA)
    ),
    `67_encourage_regulations` = dplyr::recode(
      `67_encourage_regulations`,
      "never" = "Never",
      "rarely" = "Rarely",
      "sometimes" = "Sometimes",
      "often" = "Often",
      "very_often" = "Very often",
      "no_regulations" = "Community does not have fishing regulations"
    ),
    dplyr::across(
      stringr::str_subset(names(kobo_data), "^68[a-f]"),
      ~ dplyr::recode(
        .x,
        "very_difficult" = "Very difficult",
        "difficult" = "Difficult",
        "dfficult" = "Difficult",
        "neutral" = "Neutral",
        "easy" = "Easy",
        "very_easy" = "Very easy",
        "na" = as.character(NA)
      )
    ),
    `70_food_availability` = dplyr::recode(
      `70_food_availability`,
      "very_bad" = "Very bad",
      "rather_bad" = "Rather bad",
      "ok" = "OK",
      "good" = "Good",
      "very_good" = "Very good"
    ),
    `71_worry_food` = dplyr::recode(
      `71_worry_food`,
      "never" = "Never",
      "sometimes" = "Sometimes",
      "often" = "Often"
    ),
    `72_food_procurement` = dplyr::recode(
      `72_food_procurement`,
      "very_confident_not" = "Very confident not",
      "confident_not" = "Confident not",
      "uncertain" = "Uncertain",
      "high_chance" = "High chance",
      "certain" = "Certain"
    ),
    `73_hh_fish_consumption` = dplyr::recode(
      `73_hh_fish_consumption`,
      "once_or_never" = "Once or never",
      "few_times" = "Few",
      "few_times_per_month" = "Few per month",
      "few_times_per_week" = "Few per week",
      "more_than_few_times" = "More than few per week"
    ),
    dplyr::across(
      stringr::str_subset(names(legacy_data), "^74"),
      ~ dplyr::recode(
        .x,
        "strongly_disagree" = "Strongly disagree",
        "disagree" = "Disagree",
        "neither" = "Neither agree nor disagree",
        "agree" = "Agree",
        "strongly_agree" = "Strongly agree",
        "na" = "Not Answered",
        "NA" = "Not Answered"
      )
    ),
    `75_mgmt_rules_fair` = dplyr::recode(
      `75_mgmt_rules_fair`,
      `-1` = as.character(NA)
    ),
    `76_complies_reserve` = dplyr::recode(
      `76_complies_reserve`,
      "catch_up" = "Go up",
      "catch_same" = "Stay the same",
      "catch_down" = "Go down",
      "idk" = "Don't know",
      "Don’t know" = "Don't know", # this was annoying !
      "Fishers’ catch will go up" = "Go up",
      "Fishers’ catch will stay the same" = "Stay the same",
      "Fishers’ catch will go down" = "Go down"
    ),
    `77_fishing_in_reserve` = dplyr::recode(
      `77_fishing_in_reserve`,
      "never" = "Never",
      "occasionally" = "Occasionally",
      "often" = "Often",
      "frequently" = "Frequently",
      "always" = "Always"
    ),
    dplyr::across(
      stringr::str_subset(names(kobo_data), "^78[a-f]"),
      ~ dplyr::recode(
        .x,
        "negative_formal_sanction" = "Negative formal sanction",
        "negative_informal_sanction" = "Negative informal sanction",
        "nonsanction" = "Non-sanction",
        "na" = as.character(NA)
      )
    ),
    dplyr::across(
      stringr::str_subset(names(kobo_data), "^79[a-f]"),
      ~ dplyr::recode(
        .x,
        "extremely_wrong" = "Extremely wrong",
        "very_wrong" = "Very wrong",
        "moderately_wrong" = "Moderately wrong",
        "slightly_wrong" = "Slightly wrong",
        "not_wrong" = "Not wrong",
        "na" = as.character(NA)
      )
    ),
    `80_no_wrong_fishing_reserve` = dplyr::recode(
      `80_no_wrong_fishing_reserve`,
      `-1` = as.double(NA)
    ),
    `85_current_economic` = dplyr::recode(
      `85_current_economic`,
      "much_worse" = "Much worse",
      "slightly_worse" = "Slightly worse",
      "neither" = "Neither",
      "slightly_better" = "Slightly better",
      "much_better" = "Much better"
    ),
    `86_future_economic` = dplyr::recode(
      `86_future_economic`,
      "much_worse" = "Much worse",
      "slightly_worse" = "Slightly worse",
      "neither" = "Neither",
      "slightly_better" = "Slightly better",
      "much_better" = "Much better"
    ),
    `90_hh_ends_meet` = dplyr::recode(
      `90_hh_ends_meet`,
      "with_great_difficulty" = "With great difficulty",
      "with_difficulty" = "With difficulty",
      "fairly_easy" = "Fairly easy",
      "easy" = "Easy",
      "very_easy" = "Very easy"
    ),
    `91_financial_decisions` = dplyr::recode(
      `91_financial_decisions`,
      "myself" = "Myself",
      "my_male_partner" = "My male partner",
      "my_female_partner" = "My female partner",
      "my_nonbinary_partner" = "My nonbinary partner",
      "My non-binary partner" = "My nonbinary partner",
      "both" = "Both"
    )
  )

# See above comments for missing columns !!! Some are indefinitely dropped,
# a few still need to be added (like Q15 -> Q17)
legacy_dropped_cols <- setdiff(names(legacy_data), names(kobo_data))
# We will need the old 36_fish_size_restriction in order to properly handle
# missing data for the new q47
legacy_dropped_cols <- setdiff(legacy_dropped_cols, "36_fish_size_restriction")
legacy_data <- legacy_data %>% dplyr::select(-dplyr::all_of(legacy_dropped_cols))


#### Recode some legacy_data columns ####
# Just like we recoded columns in kobo_data, we will do the same for legacy_data
# to standardize answers.

# Same code as before to get yes/no question columns
yesno_questions <- c()
for (x in names(legacy_data)) {
  unique_values <- unique(legacy_data[[x]])
  if (!all(is.na(unique_values)) & all(unique_values %in% c(0, 1, -1, NA))) {
    yesno_questions <- c(yesno_questions, x)
  }
}

# Some y/n questions have -1's which mean NA. So in the mutate block below, we recode
# -1's as NA. But a few y/n questions have a special meaning for -1, so we will take
# them out of `yesno_questions` and recode them separately in the the mutate block.
# Additionally, we have some Q's where the columns are indicator variables, so they
# should stay as 1's and 0's.
yesno_questions <- setdiff(yesno_questions,
  c("43_fishery_benefit_equal", "48_reserve_fishing_allowed", "49_reserve_boundry",
  stringr::str_subset(names(legacy_data), "^4_|^27|^33_|^54_|^57_|^62_|^63_")
))

legacy_data <- legacy_data %>% 
  dplyr::mutate(
    dplyr::across(
      yesno_questions,
      ~ dplyr::recode(.x,
        `1` = "Yes",
        `0` = "No",
        `-1` = as.character(NA),
        .missing = as.character(NA)
      )
    ),
    `8_religion` = dplyr::recode(`8_religion`,
      "Católico" = "Catholic",
      "Evangélico" = "Christian", # accurate?
      "Outra" = "Other",
      "Ateu" = "Atheist",
      "Espírita" = "Spiritual", # accurate? no other survey has 'Spiritual'
    ),
    `10_mpa_important` = dplyr::recode(`10_mpa_important`,
      `1` = "Yes",
      `0` = "No",
      `-1` = "Neutral",
      .missing = as.character(NA)
    ),
    # IDK how to do the next few with dplyr::across so we're hard coding it :)))
    `27a_financial_bank/no` = dplyr::case_when(
      `27a_financial_bank/yes_gender_unspecified` == 1 ~ 0,
      `27a_financial_bank/yes_gender_unspecified` == 0 ~ 1,
      TRUE ~ as.double(NA)
    ),
    `27b_financial_micro/no` = dplyr::case_when(
      `27b_financial_micro/yes_gender_unspecified` == 1 ~ 0,
      `27b_financial_micro/yes_gender_unspecified` == 0 ~ 1,
      TRUE ~ as.double(NA)
    ),
    `27c_financial_ngo/no` = dplyr::case_when(
      `27c_financial_ngo/yes_gender_unspecified` == 1 ~ 0,
      `27c_financial_ngo/yes_gender_unspecified` == 0 ~ 1,
      TRUE ~ as.double(NA)
    ),
    `33_hh_insurance/no` = dplyr::case_when(
      `33_hh_insurance/yes_unspecified` == 1 ~ 0,
      `33_hh_insurance/yes_unspecified` == 0 ~ 1,
      TRUE ~ as.double(NA)
    ),
    # Q41: Careful with the old survey; the old survey responses were on a 
    # strongly disagree - strongly agree scale as well but included a 6th option,
    # which I THINK is "I don't depend on or benefit from the fishery"
    # So the old survey data will have values 1-6, the new survey 1-5.
    dplyr::across(
      stringr::str_subset(names(legacy_data), "^41[a-d]"),
      ~ dplyr::recode(.x,
        `1` = "Strongly disagree",
        `2` = "Disagree",
        `3` = "Neither",
        `4` = "Agree",
        `5` = "Strongly agree",
        `6` = as.character(NA)
      )
    ),
    `43_fishery_benefit_equal` = dplyr::recode(`43_fishery_benefit_equal`,
      `1` = "Yes",
      `0` = "No",
      `-1` = "I don't depend on or benefit from the fishery"
    ),
    dplyr::across(
      c("46_ma_gear_dynamite", "46_ma_gear_hookline", "46_ma_gear_trawl",
        "46_ma_gear_harpoon", "46_ma_gear_nets"),
      ~ dplyr::recode(.x, "na" = as.character(NA))
    ),
    `48_reserve_fishing_allowed` = dplyr::recode(
      `48_reserve_fishing_allowed`,
      `1` = "Yes",
      `0` = "No",
      `-1` = "Community does not have reserves"
    ),
    `49_reserve_boundry` = dplyr::recode(
      `49_reserve_boundry`,
      `1` = "Yes",
      `0` = "No",
      `-1` = "Community does not have reserves"
    ),
    `51_reserve_boundaries_aware` = dplyr::case_when(
      is.numeric(`51_reserve_boundaries_aware`) ~ as.character(`51_reserve_boundaries_aware`),
      TRUE ~ as.character(NA)
    ),
    `52_ma_fishers_allowed` = dplyr::case_when(
      `52_ma_fishers_allowed` == "na" ~ "No managed access",
      `52_ma_fishers_allowed` %in% c(
        "Community only",
        "With authorization",
        "Without authorization",
        "No restrictions",
        "Don't know",
        "No managed access"
      ) ~ `52_ma_fishers_allowed`,
      TRUE ~ as.character(NA)
    ),
    `58_represent_role` = dplyr::recode(`58_represent_role`,
      "no mgmt body" = "No management body",
      "No management" = "No management body",
      "na" = as.character(NA)
    ),
    `61_opinions_considered` = dplyr::recode(`61_opinions_considered`,
      # Old scale is disagree-neither-agree.
      # New scale is very low-low-neither-strong-very strong.
      # At first glance, not obvious how to map disagree/agree to the new scale.
      # But according to Brittany, most analyses simply lump e.g. very low
      # and low together. So we will just say disagree is low and agree is strong.
      "Disagree" = "Low",
      "Neither" = "Neither strong or low",
      "Agree" = "Strong",
      "No management" = "No management body",
      "no mgmt" = "No management body",
      "na" = as.character(NA)
    ),
    `64_ma_punishment` = dplyr::recode(`64_ma_punishment`,
      "Weak" = "Minor",
      "Strong" = "Major",
      "Severe" = "Extreme",
      "No management" = "No regulations",
      "na" = as.character(NA)
    ),
    dplyr::across(
      stringr::str_subset(names(legacy_data), "^65[a-e]"),
      ~ dplyr::case_when(
        .x > 10 ~ as.double(NA),
        TRUE ~ .x
      )
    ),
    `66_ma_benefit_5yrs` = dplyr::recode(`66_ma_benefit_5yrs`,"na" = as.character(NA)),
    `72_food_procurement` = dplyr::recode(`72_food_procurement`,
      "Very not confident" = "Very confident not",
      "Not confident" = "Confident not"
    ),
    dplyr::across(
      stringr::str_subset(names(legacy_data), "^74[a-g,i]"),
      ~ dplyr::recode(.x,
        `1` = "Strongly disagree",
        `2` = "Disagree",
        `3` = "Neither agree nor disagree",
        `4` = "Agree",
        `5` = "Strongly agree",
        `0` = as.character(NA),
        `-1` = as.character(NA)
      )
    ),
    `76_complies_reserve` = dplyr::recode(`76_complies_reserve`,
      "go up" = "Go up",
      "stay same" = "Stay the same",
      "go down" = "Go down",
      "not know" = "Don't know",
      "3. La captura de los pescadores aumentará" = "Go up",
      "2. La captura de los pescadores seguirá igual" = "Stay the same",
      "La captura de los pescadores disminuirá" = "Go down",
      "4. No sabe" = "Don't know",
      "na" = as.character(NA)
    ),
    `77_fishing_in_reserve` = as.character(`77_fishing_in_reserve`),
    `78d_violate_reserve_fishing` = dplyr::recode(`78d_violate_reserve_fishing`,
      "negative informal sanction" = "Negative informal sanction",
      "negative formal sanction" = "Negative formal sanction",
      "non-sanction" = "Non-sanction",
      "1. Non-sanction" = "Non-sanction",
      "1. No aplicaría ninguna sanción" = "Non-sanction",
      "2. Negative informal sanction" = "Negative informal sanction",
      "2. Aplicaría una sanción negativa informal" = "Negative informal sanction",
      "3. Aplicaría una sanción negativa formal" = "Negative formal sanction"
    ),
    `80_no_wrong_fishing_reserve` = dplyr::case_when(
      `80_no_wrong_fishing_reserve` > 10 ~ as.double(NA),
      TRUE ~ `80_no_wrong_fishing_reserve`
    ),
    `91_financial_decisions` = dplyr::recode(`91_financial_decisions`,
      "Male partner" = "My male partner",
      "Female partner" = "My female partner"
    ),
    `84_post_hours_woman` = dplyr::case_when( # probably answered wrong? answer should be btwn 0-24
      `84_post_hours_woman` > 24 ~ as.double(NA),
      TRUE ~ `84_post_hours_woman`
    ),
    `84_post_hours_man` = dplyr::case_when(
      `84_post_hours_man` > 24 ~ as.double(NA),
      TRUE ~ `84_post_hours_man`
    ),
    updatedat_ymd = lubridate::as_date(updatedat),
    yearmonth = stringr::str_sub(updatedat_ymd, 1, 7)
  ) %>% 
  dplyr::select(-updatedat_ymd)

#### Join data ####
legacy_data$survey <- "FF 2.0"
kobo_data$survey <- "2022 Revision"

hhs_data <- dplyr::bind_rows(legacy_data, kobo_data)

#### Recode missing values ####
# Generally, we don't want blank (NA) responses. According to Courtney, there are
# many instances where a blank response really indicates e.g. 0.
# The following table provides substitutes for missing responses for every question,
# such as 0, "Not applicable", or "Not answered"
# However, before we can use this, we will have to map the question names just
# as we did for the old survey data
na_dict <- readr::read_csv("https://query.data.world/s/gyxk57xk5fpkgajobmcygxjirvl426")
na_dict <- na_dict %>%
  dplyr::rename(
    colname = `Column Name`,
    na_value = `Blank Definition`) %>% 
  dplyr::mutate(
    colname = dplyr::recode(colname, !!!setNames(kobo_names, legacy_names)),
    na_value = dplyr::recode(na_value,
      # The entry error part was already accounted for; values over 10 were
      # set to NA.
      # So, all that's left to do is make these NA's into "Not Answered"
      "Not Answered, values >10 are entry errors" = "Not Answered")
  ) %>% 
  dplyr::filter(colname %in% names(hhs_data)) %>% 
  dplyr::bind_rows(
    data.frame(
      colname = c("14d_income_fishing_aquaculture", "14d_months_fishing_aquaculture",
                  "14j_income_industrial", "14j_months_industrial"),
      na_value = c("0", "0")
    )
  )

# 26a_item_radio_no likely has the wrong na_value, every other col from q26 has
# "Not Answered" (even 26a_item_radio_value!) whereas 26a_item_radio_no has the
# fisher conditional "If 11c or d >0, Not Answered and if 0, Not a fisher"
na_dict$na_value[na_dict$colname == "26a_item_radio_no"] <- "Not Answered"

# First, we'll use the rows that have a simple substitution. Some of the values
# of `na_value` column are not the actual substitutions, but directions
# for what to fill in. Basically, some of the rows' `na_value` column are
# ready to use (like "Not Answered") but others will require some more logic (like
# "If 11c or d >0, Not Answered and if 0, not applicable")
na_dict_ez <- na_dict %>% 
  dplyr::filter(
    na_value %in% c("Not Answered", "Not Applicable", "0"),
    colname != "updatedat"
  )
na_dict_ez_list <- as.list(setNames(na_dict_ez$na_value, na_dict_ez$colname))
na_dict_ez_list[na_dict_ez_list == "0"] <- 0

hhs_data <- hhs_data %>% 
  tidyr::replace_na(na_dict_ez_list)

# Now to handle the conditional logic of the other NA replacements
# First, there's many columns where the NA replacement depends on whether or not
# the person interviewed is a fisher or not. This is determined based on if the 
# proportion of income from either artisanal or industrial fishing is non-zero
# So, we will handle filling NA based on this condition.
# There's one set of columns that will return "Not Answered" if fisher,
# "Not a fisher" if not a fisher. Another set of columns will return "Not Answered" if fisher,
# "Not Applicable" if not a fisher. We'll handle them separately

na_dict_fishers_condition1 <- na_dict %>% 
  dplyr::filter(na_value == "If 11c or d >0, Not Answered and if 0, Not a fisher")

condition1_cols <- intersect(names(hhs_data), na_dict_fishers_condition1$colname)

hhs_data <- hhs_data %>% 
  dplyr::mutate(
    dplyr::across(condition1_cols,
      ~ dplyr::case_when(
        (`14c_income_fishing_artisanal` > 0 | `14j_income_industrial` > 0) & is.na(.x) ~ "Not answered",
        (`14c_income_fishing_artisanal` == 0) & (`14j_income_industrial` == 0) & is.na(.x) ~ "Not a fisher",
        TRUE ~ .x
      )
    )
  )

# Now the other fisher condition

na_dict_fishers_condition2 <- na_dict %>% 
  dplyr::filter(na_value == "If 11c or d >0, Not Answered and if 0, not applicable")

condition2_cols <- intersect(names(hhs_data), na_dict_fishers_condition2$colname)

hhs_data <- hhs_data %>% 
  dplyr::mutate(
    dplyr::across(condition2_cols,
      ~ dplyr::case_when(
        (`14c_income_fishing_artisanal` > 0 | `14j_income_industrial` > 0) & is.na(.x) ~ "Not Answered",
        (`14c_income_fishing_artisanal` == 0) & (`14j_income_industrial` == 0) & is.na(.x) ~ "Not Applicable",
        TRUE ~ .x
      )
    )
  )

# Q9 condition
na_dict_q9_condition <- na_dict %>% 
  dplyr::filter(na_value == "If 9=1 (Not Answered), If 9= 0 (not applicable)")

q9_condition_cols <- intersect(names(hhs_data), na_dict_q9_condition$colname)

hhs_data <- hhs_data %>% 
  dplyr::mutate(
    dplyr::across(q9_condition_cols,
      ~ dplyr::case_when(
        `9_region_member` == "Yes" & is.na(.x) ~ "Not Answered",
        `9_region_member` == "No" & is.na(.x) ~ "Not Applicable",
        TRUE ~ .x
      )
    )
  )

# Q36 condition
na_dict_q36_condition <- na_dict %>% 
  dplyr::filter(na_value == "If 36 is 1, Not Answered; if 36 is 0 or -1, not applicable")

q36_condition_cols <- intersect(names(hhs_data), na_dict_q36_condition$colname)

hhs_data <- hhs_data %>% 
  dplyr::mutate(
    dplyr::across(q36_condition_cols,
      ~ dplyr::case_when(
        `36_fish_size_restriction` == 1 & (is.na(.x) | .x == "na") ~ "Not Answered",
        `36_fish_size_restriction` == 0 & (is.na(.x) | .x == "na") ~ "Not Applicable",
        TRUE ~ .x
      )
    )
  ) %>% 
  # no longer need old q36 col
  dplyr::select(-`36_fish_size_restriction`)

#### Export ####
usethis::use_data(hhs_data, overwrite=TRUE)

tf <- tempfile(fileext=".csv")
readr::write_csv(tf)
dwapi::upload_file("rare", "hh-surveys", tf, "hh-surveys-all.csv")

#### NOTES ####
#
## Not in new survey:
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
# 36_fish_size_restriction
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
## Other notes:
# The different categories for Q25 have been split up by gender.
# E.g. 25a_financial_bank is now 27a_financial_bank/yes_male,
# 27a_financial_bank/yes_female, 27a_financial_bank/yes_nonbinary,
# and 27a_financial_bank/no
# Since the original responses are not specified by gender, the columns were
# mapped to "gender unspecified" categories:
# 25a_financial_bank -> 27a_financial_bank/gender_unspecified
# This new column name was added and left blank for samples from the new survey
#
# Q35 other
# Did try to salvage the 35k_ma_gear_other, but the way the question is set up
# on the new survey, 46_ma_gear_other specifies permitted/not permitted and 
# 46_ma_gear_other_specify specifies what the gear is.
# 35k_ma_gear_other is a mix... sometimes it just says the gear, sometimes it says
# "X is not allowed" So we're just dropping it.
#
# Q66 was specifically about fishing in the reserve, the new Q78 asks about
# fishing in the reserve and more. So old Q66 corresponds to new Q78d. The
# other column in 66, 66_response_fishing_reserve, is just a yes/no kind of
# question which is not used anymore.
#
# Q50: The old survey was on a scale of "strongly disagree" to "strongly agree"
# while the new survey is one a scale of 1-10
#
# 77_fishing_in_reserve
# New answers are on never-always character scale while old answers (which come from
# old 63_times_fishing_reserve) were numbers describing frequency in the past month.
# Is 7 considered "Often" or "Frequently"? We don't know so these answers are not
# comparable from old survey to new survey
#
#### TODO ####
# 
# 15_activity (this might be the other sheets in the new hhs xlsx)
#
