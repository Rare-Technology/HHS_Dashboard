
prep_q40_reserve_boundaries_aware <- function(.data, iso3){

  if (iso3== "MOZ") {
  
    hhs_Q40_moz <- .data[,c("maa", "40_reserve_boundaries_aware")] %>%
      dplyr::filter (`40_reserve_boundaries_aware` != "" &
                       `40_reserve_boundaries_aware` != "No reserve") %>%
      droplevels()
    
    hhs_Q40_moz <- hhs_Q40_moz %>% 
      dplyr::mutate(
        `40_reserve_boundaries_aware` = forcats::fct_recode(
          factor(`40_reserve_boundaries_aware`),
          "Strongly disagree" = "0",
          #"Strongly disagree" = "1", # 1 does not occur
          "Strongly disagree" = "2",
          "Disagree" = "3",
          "Disagree" = "4",
          "Neither" = "5",
          "Agree" = "6",
          "Agree" = "7",
          "Strongly agree" = "8",
          "Strongly agree" = "9",
          "Strongly agree" = "10"
        ),
        `40_reserve_boundaries_aware` = as.character(`40_reserve_boundaries_aware`)
      )
    
    Q40_summary_moz <-
      proportion(
        question = hhs_Q40_moz[[2]],
        grouping = hhs_Q40_moz[[1]],
        rounding = 3, 
        type = 5
      )
    colnames(Q40_summary_moz) <-
      c(
        "MA name",
        "N",
        "Agree",
        "Disagree",
        "Neither",
        "Strongly agree",
        "Strongly disagree"
      )
    
    #pivot table
    Q40_summary_moz_long <-
      Q40_summary_moz %>% tidyr::pivot_longer(
        cols = c(
          "Strongly disagree",
          "Disagree",
          "Neither",
          "Agree",
          "Strongly agree"
        ),
        names_to = "key",
        values_to = "Proportion (%)"
      )
    Q40_summary_moz_long$key <-
      factor(
        Q40_summary_moz_long$key,
        levels = c(
          "Strongly disagree",
          "Disagree",
          "Neither",
          "Agree",
          "Strongly agree"
        )
      )
    
   dat <- clean_plot_data(Q40_summary_moz_long) 
  } else {

      hhs_Q40 <- .data[,c("maa", "40_reserve_boundaries_aware")] %>% 
        dplyr::filter(`40_reserve_boundaries_aware` %in% c(0:10)) %>%
        droplevels()
      
      Q40_length <-
        tapply(hhs_Q40[[2]],
               hhs_Q40[[1]],
               length)
      Q40_length <- as.vector(Q40_length)
      Q40_mean <-
        data.frame(avg = tapply(as.numeric(
          as.character(hhs_Q40$`40_reserve_boundaries_aware`)
        ),
        hhs_Q40$maa, mean) / 10)
      Q40_summary_bind <-
        cbind(N = Q40_length, round(Q40_mean, 3) * 100)
      Q40_summary <-
        rbind(Q40_summary_bind, "Mean ± SE" = c(
          sum(Q40_summary_bind$N),
          compute_summary_line(Q40_summary_bind$avg, 1)
        ))
      Q40_summary <-tibble::rownames_to_column(Q40_summary, "MA name")
      colnames(Q40_summary) <- c("MA name", "N", "Proportion (%)")
      
      dat <- clean_plot_data (Q40_summary)
      
 
      
  }
  
  dat
  
}

plot_q40_reserve_boundaries_aware <- function(.data, ...){
  dots <- list(...)
  .data_plot <- prep_q40_reserve_boundaries_aware(.data, iso3 = dots$iso3)

  
  if(dots$iso3 != "MOZ"){
    p <- plot_horiz_bar(
      .data_plot,
      title = "Proportion of community members that think that most fishers are aware of the boundaries of the reserve area"
    )
  } else {
    p <- plot_horiz_bar(
      .data_plot,
      title = "Proportion of community members that think that most fishers are aware of the boundaries of the reserve area",
      facet_var = key
    )
  }

  result <- list(
    plot = p,
    data = .data_plot
  )
  
}