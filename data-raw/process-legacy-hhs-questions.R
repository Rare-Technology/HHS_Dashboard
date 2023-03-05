library(dplyr)
library(readxl)

legacy_hhs_questions <- readr::read_csv("data-raw/legacy_hhs_questions.csv")

# Questions added
legacy_hhs_questions <- legacy_hhs_questions %>%
  dplyr::mutate(
    question = ifelse(q_no == 20, question_no_included, question)
  )

legacy_hhs_questions <- legacy_hhs_questions %>%
  dplyr::filter(!is.na(question)) %>%
  dplyr::filter(grepl("[a-z]", question)) %>%
  dplyr::filter(question != "Household Survey Summary") %>%
  dplyr::select(section, q_no, question)



legacy_hhs_questions$question[legacy_hhs_questions$question == "42. Do you know how fishing with restricted gear affect the fishery?"] <-
  "42b. Do you know how fishing with restricted gear affect the fishery?"
legacy_hhs_questions$question[legacy_hhs_questions$question == "42. Do you know how fishing undersize fish affect the fishery?"] <-
  "42c. Do you know how fishing undersize fish affect the fishery?"
legacy_hhs_questions$question[legacy_hhs_questions$question == "42. Do you know how fishing inside the reserve affect the fishery?"] <-
  "42d. Do you know how fishing inside the reserve affect the fishery?"
legacy_hhs_questions$question[legacy_hhs_questions$question == "42. Do you know how unauthorized fishers fishing inside the managed access area affect the fishery?"] <-
  "42e. Do you know how unauthorized fishers fishing inside the managed access area affect the fishery?"

legacy_hhs_questions$question[legacy_hhs_questions$question == "66. If people in the community found out that a fisher was fishing in the reserve, would they say or do anything in response?"] <-
  "66a. If people in the community found out that a fisher was fishing in the reserve, would they say or do anything in response?"

legacy_hhs_questions$question[legacy_hhs_questions$question == "66. If yes to previous question. What would people say or do if they found out that a fisher was fishing in the reserve?"] <-
  "66b. If yes to previous question. What would people say or do if they found out that a fisher was fishing in the reserve?"



legacy_hhs_questions$q_no <- stringr::str_extract(legacy_hhs_questions$question, "[a-zA-Z0-9]+(?=[/\\d]*\\.)")

sections <- unique(legacy_hhs_questions$section)
legacy_hhs_questions <- purrr::map(sections, function(x) {
  tmp <- dplyr::filter(legacy_hhs_questions, section == x)

  vect <- tmp %>%
    dplyr::pull(q_no) %>%
    paste0("q", .)
  names(vect) <- tmp %>% dplyr::pull(question)
  vect
})
names(legacy_hhs_questions) <- sections
legacy_hhs_questions$`Basic Information`[legacy_hhs_questions$`Basic Information` == "q8"] <- "q08"
legacy_hhs_questions$`Basic Information`[legacy_hhs_questions$`Basic Information` == "q9"] <- "q09"


detach(package:dplyr)
usethis::use_data(
  legacy_hhs_questions,
  overwrite = TRUE
)