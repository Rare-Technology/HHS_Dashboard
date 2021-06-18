library(dplyr)

hhs_questions <- readr::read_csv("data-raw/hhs_questions.csv")

# Questions added
hhs_questions <- hhs_questions %>% 
  dplyr::mutate(
    question = ifelse(q_no %in% c(7, 20), question_no_included, question)
  )

hhs_questions <- hhs_questions %>% 
  dplyr::filter(!is.na(question)) %>%
  dplyr::filter(grepl("[a-z]", question)) %>%
  dplyr::filter(question != "Household Survey Summary") %>%
  dplyr::select(section, q_no, question)



hhs_questions$question[hhs_questions$question == "42. Do you know how fishing with restricted gear affect the fishery?"] <- 
  "42b. Do you know how fishing with restricted gear affect the fishery?"
hhs_questions$question[hhs_questions$question == "42. Do you know how fishing undersize fish affect the fishery?"] <- 
  "42c. Do you know how fishing undersize fish affect the fishery?"
hhs_questions$question[hhs_questions$question == "42. Do you know how fishing inside the reserve affect the fishery?"] <- 
  "42d. Do you know how fishing inside the reserve affect the fishery?"
hhs_questions$question[hhs_questions$question == "42. Do you know how unauthorized fishers fishing inside the managed access area affect the fishery?"] <- 
  "42e. Do you know how unauthorized fishers fishing inside the managed access area affect the fishery?"

hhs_questions$question[hhs_questions$question == "66. If people in the community found out that a fisher was fishing in the reserve, would they say or do anything in response?"] <- 
  "66a. If people in the community found out that a fisher was fishing in the reserve, would they say or do anything in response?"

hhs_questions$question[hhs_questions$question == "66. If yes to previous question. What would people say or do if they found out that a fisher was fishing in the reserve?"] <- 
  "66b. If yes to previous question. What would people say or do if they found out that a fisher was fishing in the reserve?"



hhs_questions$q_no <- stringr::str_extract(hhs_questions$question, "[a-zA-Z0-9]+(?=[/\\d]*\\.)")

sections <- unique(hhs_questions$section)
hhs_questions <- purrr::map(sections, function(x) {
  tmp <- dplyr::filter(hhs_questions, section == x)
  
  vect <- tmp %>%
    dplyr::pull(q_no) %>%
    paste0("q", .)
  names(vect) <- tmp %>% dplyr::pull(question)
  vect
})
names(hhs_questions) <- sections
hhs_questions$`Basic Information`[hhs_questions$`Basic Information` == "q7"] <- "q07"
hhs_questions$`Basic Information`[hhs_questions$`Basic Information` == "q8"] <- "q08"
hhs_questions$`Basic Information`[hhs_questions$`Basic Information` == "q9"] <- "q09"


detach(package:dplyr)
usethis::use_data(
  hhs_questions,
  overwrite = TRUE
)