
table_summary <- function(.data, hhs_question){
  
  if (hhs_question == "Household Survey Summary") {
  
    hhs_stats <- .data %>% 
      dplyr::filter(ma_name != '') %>% 
      droplevels %>% 
      mutate(
        updatedat = as.Date(updatedat)
      )
    
    #Extract start and end survey date per MA
    hhs_dates <- hhs_stats %>% 
      dplyr::group_by(ma_name) %>% 
      dplyr::summarise(datestarted = as.character(format(min(as.Date(updatedat),na.rm = TRUE), 
                                                         "%m/%d/%y")),
                       dateended = as.character(format(max(as.Date(updatedat), na.rm = TRUE),
                                                       "%m/%d/%y"))) %>%
      ungroup()
    
    surveys_date_max <- max(hhs_stats$updatedat) %>% 
      format(format = "%B %d, %Y") 
    
    surveys_date_min <- min(hhs_stats$updatedat) %>% 
      format(format = "%B %d, %Y") 
    
    #Number of surveys per MA
    surveys_per_ma <- .data %>% 
      dplyr::count(ma_name, name = "surveys.per.ma") %>% 
      dplyr::arrange(ma_name)
    
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
      dplyr::select(`3_community`, ma_name) %>% 
      dplyr::distinct() %>% 
      dplyr::count(ma_name, name = "villages.per.ma")
    
    ## Name of provinces/regions of study
    province_name <-unique(.data$level1_name)
    
    ## Name of country of study
    country_name <-  unique(.data$country)
    
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
    
    #Extract ma_names  levels
    district_names <- hhs_stats %>% 
      dplyr::group_by(level1_name, level2_name, ma_name) %>% 
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
      dplyr::filter(`6_gender` == "F") %>% 
      droplevels()
    
    womenhhs <- get_ma_percent(hhs_stats, hhs_womenhh, "women_pct")
    
    #Calculate proportion of men interviewed
    hhs_menhh <- .data %>% 
      dplyr::filter(`6_gender` == "M") %>% 
      droplevels()
    
    menhhs <- get_ma_percent(hhs_stats, hhs_menhh, "men_pct")
    
    ## TABLE 3 Combine number of surveys, number of MA, number of villages, prop of fisher households ###
    table_summary <- district_names %>% 
      dplyr::select(ma_name) %>% 
      left_join(hhs_dates, by = "ma_name") %>%
      left_join(surveys_per_ma, by = "ma_name") %>% 
      left_join(villages_per_ma, by = "ma_name") %>% 
      left_join(fisherhhs, by = "ma_name") %>% 
      left_join(womenhhs, by = "ma_name") %>% 
      left_join(menhhs, by = "ma_name")
    
    table_summary <- table_summary %>% 
      dplyr::rename(
        "MA name" = "ma_name",
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
       "MA name" = "",
       "Date surveys started" = NA,
       "Date surveys ended" = NA,
       "Households surveyed" = sum(table_summary$`Households surveyed`),
       "No. communities" = sum(table_summary$`No. communities`),
       "Fisher households (%)" = mean_sem(table_summary$`Fisher households (%)`, 1),
       "Women interviewed (%)" = mean_sem(table_summary$`Women interviewed (%)`, 1),
       "Men interviewed (%)" = mean_sem(table_summary$`Men interviewed (%)`, 1)
     ),
     c(
       "MA name"="",
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
  
  else if (hhs_question != "1. Household Survey Summary") {
    NULL
  }
}

