library(dplyr)
library(readxl)

hhs_questions_table <- readxl::read_excel("data-raw/hhs_questions.xlsx") %>% 
  dplyr::select(
    q_number = `New Question Number`,
    q_text = `Question English`
  ) %>% 
  dplyr::mutate(
    q_number_no_letter = stringr::str_extract(q_number, "[^a-z]*"),
    q_letter = paste0(stringr::str_extract(q_number, "[a-z]"), ")"),
    q_text = dplyr::case_when(
      q_number_no_letter == "65" ~ paste("65.", q_text),
      q_number_no_letter %in% c("41", "74") ~ paste(
        paste0(stringr::str_sub(q_number, end=2), "."),
        q_letter,
        q_text
      ),
      TRUE ~ paste(q_number, q_text, sep=". ")
    )
  ) %>% 
  dplyr::select(q_number, q_text) %>% 
  dplyr::left_join(hhs_sections) %>% 
  dplyr::filter(!(section %in% c("Introduction", "Conclusion")))

sections <- unique(hhs_questions_table$section)
sections <- sections[!is.na(sections)]

hhs_questions <- purrr::map(sections, function (s) {
    s_questions <- hhs_questions_table %>% dplyr::filter(section == s)
    s_q_texts <- unique(s_questions$q_text)
    
    out <- purrr::map(s_q_texts, function (txt) {
      q_num <- s_questions %>%
        dplyr::filter(q_text == txt) %>%
        dplyr::pull(q_number) %>% 
        unique() %>% 
        # 3 -> 03, otherwise e.g. 62 is 62
        ifelse(
          str_length(.) == 1,
          formatC(as.double(.), width=2, format="d", flag="0"),
          .
        )
      
      out2 <- paste0("q", q_num)
      
      return(out2)
    })
    names(out) <- s_q_texts
    
    return(out)
  })
names(hhs_questions) <- sections

create_geo_table <- function(.data) {
  dplyr::distinct(
    .data,
    year,
    iso3,
    country,
    subnational,
    local,
    maa
  ) %>%
    dplyr::arrange(year, country, subnational, local, maa)
}

hhs_init_year_selections <- list(
  selected = 2021, # subject to change
  choices = hhs_data$year %>% unique() %>% sort()
)

hhs_data_geo <- create_geo_table(hhs_data)
hhs_init_geo_selections <- rarehhs::get_geo_selections(
  hhs_data_geo,
  year_selected = 2021,
  country_selected = "Honduras"
)

geo_filter_data <- function(.data) {
  .data %>%
    dplyr::filter(year == hhs_init_year_selections$selected) %>% 
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

usethis::use_data(
  hhs_data_filtered,
  hhs_questions,
  hhs_data_source,
  hhs_data_geo,
  hhs_init_year_selections,
  hhs_init_geo_selections,
  hhs_init_section,
  overwrite = TRUE
)