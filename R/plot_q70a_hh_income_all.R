
prep_q70a_hh_income_all <- function(.data, iso3){
  ### Q70 What is your households' average monthly income from all activities, including salaried and non-salaried labor? ####

  conversion <- convert_money(iso3)
  hhs_Q70 <- .data #%>%
  # dplyr::filter (between(
  #        as.numeric(as.character(`70_hh_average_income`)), 
  #          10, 1.00e+20)) %>%
  #           droplevels()
  Q70_length <-
    tapply(as.numeric(hhs_Q70$`70_hh_average_income`),
           hhs_Q70$maa,
           length)
  
  # tapply(as.numeric(hhs_Q70$`70_hh_average_income`),
  #        hhs_Q70$maa,
  #        mean)
  
  #Proportion of income and Average income per MA
  income_source <-
    hhs_Q70[, c(
      "maa",
      "11a_income_farming",
      "11b_income_harvesting",
      "11c_income_fishing_artisanal",
      "11d_income_fishing_industrial",
      "11e_income_buying_trading",
      "11f_income_processing",
      "11g_income_aquaculture",
      "11h_income_extraction",
      "11i_income_tourism",
      "11k_income_other"
    )]
  
  income_source[is.na(income_source)] <- 0
  income_source$`70_hh_average_income` <- 
    as.numeric(as.character(hhs_Q70$`70_hh_average_income`))
  income_source <- data.frame(income_source)
  
  ## Aggregate income
  income_summary <-
    aggregate(. ~ maa,
              FUN = mean,
              na.rm = TRUE,
              data = income_source)
  
  HH_avg_income <- data.frame (
    N = Q70_length,
    Total_Income = round(income_summary$X70_hh_average_income, 1),
    Farming = round(income_summary$X11a_income_farming, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Harvesting = round(income_summary$X11b_income_harvesting, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Artisinal_Fishing = round(income_summary$X11c_income_fishing_artisanal, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Industrial_Fishing = round(income_summary$X11d_income_fishing_industrial, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Buying_Trading = round(income_summary$X11e_income_buying_trading, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Processing_Fish = round(income_summary$X11f_income_processing, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Aquaculture = round(income_summary$X11g_income_aquaculture, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Extraction = round(income_summary$X11h_income_extraction, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Tourism = round(income_summary$X11i_income_tourism, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100,
    Other = round(income_summary$X11k_income_other, 1) /
      rowSums(income_summary[, c(2:11)], na.rm = TRUE) * 100
  )
  
  HH_avg_income_mean <-
    rbind(
      HH_avg_income,
      "Mean ± SE" = c(
        sum(HH_avg_income$N),
        compute_summary_line(HH_avg_income$Total_Income, 1),
        compute_summary_line(HH_avg_income$Farming, 1),
        compute_summary_line(HH_avg_income$Harvesting, 1),
        compute_summary_line(HH_avg_income$Artisinal_Fishing, 1),
        compute_summary_line(HH_avg_income$Industrial_Fishing, 1),
        compute_summary_line(HH_avg_income$Buying_Trading, 1),
        compute_summary_line(HH_avg_income$Processing_Fish, 1),
        compute_summary_line(HH_avg_income$Aquaculture, 1),
        compute_summary_line(HH_avg_income$Extraction, 1),
        compute_summary_line(HH_avg_income$Tourism, 1),
        compute_summary_line(HH_avg_income$Other, 1)
      )
    )
  
  Q70_summary <-
    tibble::rownames_to_column(HH_avg_income_mean[, c(1:2)], "MA name")
  colnames(Q70_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q70 <- clean_plot_data (Q70_summary)
  
  colnames(Q70) <- c("MA name", "N", "Average")
  Q70$Average <- round(Q70$Average * conversion, 1)
  Q70
}

plot_q70a_hh_income_all <- function(.data, ...){
  
  dots <- list(...)
  
  .data_plot <- prep_q70a_hh_income_all(.data, dots$iso3)
         

  p <- plot_horiz_bar(
    .data_plot,
    title = "Average income in USD",
    limits = NULL,
    y_var = Average,
    y_title = "US Dollars ($)",
    breaks = waiver()
  )

  result <- list(
    plot = p,
    data = .data_plot
  )
}