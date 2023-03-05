library(httr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

KOBO_API_KEY <- Sys.getenv("KOBO_API_KEY")

httr::GET(
  "https://kf.kobotoolbox.org/api/v2/assets/aUwwjWLmQmPkyKggi6s7f4/export-settings/esNmqCpJMQg4ja4GbmpVaRv/data.xlsx",
  httr::add_headers(Authorization=paste("Token", KOBO_API_KEY)),
  httr::write_disk(tf <- tempfile(fileext=".xlsx"))
)
hhs_sections <- readxl::read_excel(tf)
hhs_sections <- hhs_sections %>% 
  names(.) %>% 
  tibble::tibble(colname=.) %>% 
  # Using tidyr::separate to extract the section info, then use dplyr::case_when to fix the colname
  tidyr::separate(colname, into=c("section", "colname", "colname_extra"), sep="/") %>% 
  dplyr::mutate(
    colname = dplyr::case_when(
      is.na(colname_extra) ~ colname,
      TRUE ~ paste(colname, colname_extra, sep="/")
    ),
    colname = stringr::str_sub(colname, 2), # remove leading _
    section = stringr::str_sub(section, 7), # remove leading _note_
    section = dplyr::recode(
      section,
      "introduction" = "Introduction",
      "basic_info" = "Basic Information",
      "covid" = "COVID",
      "livelihood" = "Livelihood",
      "fishing_related" = "Fishing and related activities",
      "fishing_related_001" = "Fishing and related activities",
      "household_assets" = "Household Assets",
      "financial_resilience" = "Financial Resilience",
      "social_capital" = "Social Capital",
      "knowledge" = "Knowledge",
      "fishery_management" = "Fishery Management, Enforcement, and Compliance",
      "food_security" = "Food Security",
      "attitudes" = "Attitudes",
      "expenditures_income" = "Expenditures and Income",
      "expenditures_income_001" = "Expenditures and Income",
      "conclusion" = "Conclusion"
    ),
    # "41f_community_rebuilt" -> "41f"
    # q_number will be used as an index between hhs_sections and hhs_questions
    q_number = stringr::str_split(colname, "_") %>%
      sapply("[", 1),
    # These last few lines are just to make sure q_number matches the q_number from hhs_questions
    q_num_no_letter = stringr::str_extract(q_number, "[^a-z]*"),
    q_number = dplyr::case_when(
      stringr::str_detect(colname, "69a_benefits") ~ "69a",
      q_num_no_letter %in% c("41", "65", "74") ~ q_number,
      TRUE ~ q_num_no_letter
    )
  ) %>% 
  dplyr::filter(
    stringr::str_detect(colname, "^[0-9]"),
    section != "conclusion"
  ) %>% 
  dplyr::select(section, colname, q_number)

usethis::use_data(hhs_sections, overwrite = TRUE)

#### MISSING Q'S ####
# 7 - hh_people
# 17 - activities
# 82 - type_of_fish
#
# These q's have their own sheets in the xlsx download. Will have to do the cleaning for them later
