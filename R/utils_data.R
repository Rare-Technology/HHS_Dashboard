read_multi_csv <- function(urls, col_type_spec = NULL, rowmax = 1000000) {
  dat <- purrr::map_dfr(1:length(urls), function(i) {
    iso3 <- names(urls)[i]
    url <- urls[i]

    dat <- readr::read_csv(
      url,
      col_types = col_type_spec,
      locale = readr::locale(encoding = "UTF-8"),
      guess_max = rowmax
    )


  })

  dat
}


convert_money <- function(iso3){
  from <- switch (iso3,
    "IDN" = "IDR",
    "HND" = "HNL",
    "PHL" = "PHP",
    "MOZ" = "MZN",
    "BRA" = "BRL",
    "FSM" = "USD",
    "PLW" = "USD"
  )
  
  quantmod::getQuote(glue::glue("{from}USD=X"))$Last
}


# apply_PLW_adjustment <- function(.data){
#   .data %>% 
#     dplyr::mutate(
#       level2_name = ifelse(country == "PLW", level1_name, level2_name),
#       maa = ifelse(country == "PLW", level1_name, maa)
#     )
# }


get_ma_percent <- function(.fulldata, .partdata, newname = "percent"){
  
  ma_count <- .fulldata %>% 
    dplyr::count(maa)
  
  ma_count_hhs <- .partdata %>% 
    dplyr::count(maa)
  
  result <- dplyr::full_join(ma_count, ma_count_hhs, by = "maa", suffix = c("_full", "_part")) %>% 
    dplyr::mutate(
      percent = round(100* n_part/n_full, 1)
    ) %>% 
    dplyr::select(-n_part, -n_full)
  
  names(result)[names(result) == "percent"] <- newname
  result
}

hhs_table_summary <- function(.data) { 
  
  hhs_stats <- .data %>% 
    #dplyr::filter(maa != '') %>% should already be done
    #droplevels %>% 
    dplyr::mutate(
      updatedat = as.Date(updatedat)
    )
  
  #Extract start and end survey date per MA
  hhs_dates <- hhs_stats %>% 
    dplyr::group_by(maa) %>% 
    dplyr::summarise(datestarted = as.character(format(min(as.Date(updatedat),na.rm = TRUE), 
                                                       "%m/%d/%y")),
                     dateended = as.character(format(max(as.Date(updatedat), na.rm = TRUE),
                                                     "%m/%d/%y"))) %>%
    dplyr::ungroup()
  
  surveys_date_max <- max(hhs_stats$updatedat) %>% 
    format(format = "%B %d, %Y") 
  
  surveys_date_min <- min(hhs_stats$updatedat) %>% 
    format(format = "%B %d, %Y") 
  
  #Number of surveys per MA
  surveys_per_ma <- .data %>% 
    dplyr::count(maa, name = "surveys.per.ma") %>% 
    dplyr::arrange(maa)
  
  #Number of MA
  no_of_ma <- nrow(surveys_per_ma)
  
  #lowest number of surveys per MA
  min_surveys_ma <- min(surveys_per_ma$surveys.per.ma)
  
  #highest number of surveys per MA
  max_surveys_ma <-  max(surveys_per_ma$surveys.per.ma)
  
  ### Total Number of communities surveyed
  no_of_communities <- length(unique(.data$`3_community`))
  
  ### Number of communities/villages per managed access
  villages_per_ma <- .data %>% 
    dplyr::select(`3_community`, maa) %>% 
    dplyr::distinct() %>% 
    dplyr::count(maa, name = "villages.per.ma")
  
  ## Name of provinces/regions of study
  province_name <- unique(.data$subnational)
  
  ## Name of country of study
  country_name <- unique(.data$country)
  
  ## Proportion of male and female respondants
  gender_count <- .data %>% 
    dplyr::count(`6_gender`)
  
  gender_count$percent <- round(100 * gender_count$n/nrow(.data), 1)
  
  male_surveyed <-gender_count %>% 
    dplyr::filter(`6_gender` == "M") %>% 
    dplyr::pull(percent)
  
  female_surveyed <- gender_count %>% 
    dplyr::filter(`6_gender` == "F") %>% 
    dplyr::pull(percent)
  
  #Extract maas  levels
  district_names <- hhs_stats %>% 
    dplyr::group_by(maa) %>% 
    dplyr::summarise(lat = mean(lat, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  hhs_fisherhh <- .data %>% 
    dplyr::filter(
      `12a_fishing_men` > 0 |
        `12b_fishing_women` > 0 | 
        `12c_fishing_children` > 0
    )
  fisherhhs <- get_ma_percent(.data, hhs_fisherhh, "fisher_pct")
  
  #Calculate proportion of women interviewed
  hhs_womenhh <- .data %>% 
    dplyr::filter(`6_gender` == "F") #%>% 
    #droplevels()
  
  womenhhs <- get_ma_percent(hhs_stats, hhs_womenhh, "women_pct")
  
  #Calculate proportion of men interviewed
  hhs_menhh <- .data %>% 
    dplyr::filter(`6_gender` == "M") #%>% 
    #droplevels()
  
  menhhs <- get_ma_percent(hhs_stats, hhs_menhh, "men_pct")
  
  ## TABLE 3 Combine number of surveys, number of MA, number of villages, prop of fisher households ###
  table_summary <- district_names %>% 
    dplyr::select(maa) %>% 
    dplyr::left_join(hhs_dates, by = "maa") %>%
    dplyr::left_join(surveys_per_ma, by = "maa") %>% 
    dplyr::left_join(villages_per_ma, by = "maa") %>% 
    dplyr::left_join(fisherhhs, by = "maa") %>% 
    dplyr::left_join(womenhhs, by = "maa") %>% 
    dplyr::left_join(menhhs, by = "maa")
  
  table_summary <- table_summary %>% 
    dplyr::rename(
      "MA name" = "maa",
      "Surveys started" = "datestarted",
      "Surveys ended" = "dateended",
      "Households surveyed" = "surveys.per.ma",
      "No. communities" = "villages.per.ma",
      "Fisher households (%)" = "fisher_pct",
      "Women interviewed (%)" = "women_pct",
      "Men interviewed (%)"= "men_pct"
    )
  
  #browser()
  rbind(
    table_summary,
    c(
      "MA name" = NA,
      "Date surveys started" = NA,
      "Date surveys ended" = NA,
      "Households surveyed" = sum(table_summary$`Households surveyed`),
      "No. communities" = sum(table_summary$`No. communities`),
      "Fisher households (%)" = compute_summary_line(table_summary$`Fisher households (%)`, 1),
      "Women interviewed (%)" = compute_summary_line(table_summary$`Women interviewed (%)`, 1),
      "Men interviewed (%)" = compute_summary_line(table_summary$`Men interviewed (%)`, 1)
    ),
    c(
      "MA name"= NA,
      "Date surveys started" = NA,
      "Date surveys ended" = NA,
      "Households surveyed"= "Total",
      "No. communities" = "Total",
      "Fisher households (%)" = "Mean ± SE",
      "Women interviewed (%)" = "Mean ± SE",
      "Men interviewed (%)" = "Mean ± SE"
    )
  )
}



#' Title
#'
#' @param .data 
#' @param focus_var 
#' @param key_name 
#' @param values_name 
#' @param rounding 
#' @param include_summary_line 
#' @param recoding 
#' @param type 
#' @param bar_column 
#' @param key_order 
#'
#' @return
#' @export
#'
#' @examples
prep_data_for_plot <- function(
  .data, 
  focus_var, 
  key_name = "key", 
  values_name = "Proportion (%)", 
  rounding = 3,
  include_summary_line = FALSE,
  recoding = NULL, # new value as names (on left of assignment)
  type = "bar", # stacked or facet
  bar_column = `1`,
  key_order = NULL,
  unnest = FALSE
  ) {

  browser()
  dat <- .data %>% 
    dplyr::select(maa, {{ focus_var }})
  
  if(unnest){
    dat <- dat %>% 
      tidyr::unnest({{ focus_var }})
  }

  dat <- dat %>%
    dplyr::filter({{ focus_var }} != "", !is.na({{ focus_var}}), {{focus_var}} != "na") 
  
  if(!is.null(recoding)){
    vals <- unique(dat[[rlang::as_name(enquo(focus_var))]])
    recoding <- recoding[recoding %in% vals]
    mode(recoding) <- "character" # needed for fct_recode
    dat <- dat %>% 
      dplyr::mutate(
        {{ focus_var }} := forcats::fct_recode(factor({{ focus_var }}), !!!recoding)
      )
  }
  
  
  
  tot <- dat %>% 
    dplyr::group_by(maa) %>% 
    dplyr::mutate(
      N = dplyr::n()
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(maa, {{ focus_var }}) %>% 
    dplyr::mutate(
      N_question = dplyr::n(),
      pct = 100 * round(N_question/N, rounding)
    ) %>% 
    dplyr::distinct() %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-N_question)
  
  tot <- tot %>% 
    tidyr::pivot_wider(
      id_cols = c(maa, N),
      values_from = pct,
      names_from = {{ focus_var }},
      values_fill = 0
    ) %>% 
    dplyr::rename(
      `MA name` = maa
    )
  
  
  if(include_summary_line){
    summary_stats <- tot %>% 
      dplyr::select(-`MA name`, -N) %>% 
      purrr::map_chr(~compute_summary_line(.))
    
    summary_line <- c("Mean ± SE", sum(tot$N), summary_stats)
    
    tot <- rbind(
      tot,
      summary_line
    ) 
  }
  
  if(type == "bar"){
    tot <- tot %>% 
      dplyr::select(`MA name`, N, {{values_name }} := {{ bar_column }} )
    return(tot)
  }
  
  
  tot <- tot %>% 
    tidyr::pivot_longer(
      cols = c(-`MA name`, -N),
      names_to = key_name,
      values_to = values_name
    ) %>% 
    dplyr::filter (`MA name` !=  "Mean ± SE") %>%
    dplyr::mutate(
      !!values_name := as.numeric( !!sym(values_name) ),
      !!key_name := gsub("[.]", " ", !!sym(key_name))
    )
  
  if(!is.null(key_order)){
    
    key_order <- key_order[key_order%in%unique(tot[[key_name]])]
    tot <- tot %>% 
      dplyr::mutate(
        !!key_name := forcats::fct_relevel(!!sym(key_name), key_order)
      )
  }
  
  
  tot
  
}

prep_data_for_plot_facet <- function(
  .data, 
  select_vars, 
  group_by_var = maa, 
  var_names, 
  key_name = "key", 
  values_name = "Proportion (%)",
  my_func = function(x){round(mean(x, na.rm = TRUE), 1)}
){
  

  browser()
  group_by_str <- rlang::as_name(enquo(group_by_var))
  
  dat <- .data %>% 
    dplyr::select(maa, {{ select_vars }}) %>% 
    dplyr::group_by( {{ group_by_var }})
  
  
  dat_n <- dat %>% 
    dplyr::summarise(N = dplyr::n())
  
  dat_f <- dat %>% 
    dplyr::summarise_at(vars({{select_vars}} ), my_func)
  
  
  dat_summary <- dplyr::full_join(dat_n, dat_f, by = group_by_str )
  names(dat_summary) <- c("maa", "N", var_names)
  
  summary_row <- c(
    NA,
    sum(dat_summary$N),
    purrr::map_chr(var_names, ~compute_summary_line(dat_summary[[.]], 1))
  )
  
  dat_summary <- rbind(dat_summary, summary_row)
  
  dat_summary_long <- dat_summary %>% 
    tidyr::pivot_longer(
      cols = var_names,
      names_to = key_name,
      values_to = values_name
    )
  
  dat_summary_long[[key_name]] <- 
    factor(dat_summary_long[[key_name]], levels = var_names)
  
  
  
  final_dat <- dat_summary_long %>% 
    dplyr::filter (maa !=  "Mean ± SE") %>%
    dplyr::mutate(!!sym(values_name) := as.numeric(!!sym(values_name))) %>% 
    dplyr::rename(
      `MA name` = maa
    )
  
  final_dat
}



clean_plot_data <- function (.data_summary) {
  .data_summary %>% 
    dplyr::filter (`MA name` !=  "Mean ± SE") %>%
    dplyr::mutate(`Proportion (%)` = as.numeric(`Proportion (%)`)) 
}

get_iso3 <- function(country){
  switch (country,
    "Brazil" = "BRA",
    "Indonesia" = "IDN",
    "Honduras" = "HND",
    "Philippines" = "PHL",
    "Mozambique" = "MOZ",
    "Federated States of Micronesia" = "FSM",
    "Palau" = "PLW"
    
  )
}
