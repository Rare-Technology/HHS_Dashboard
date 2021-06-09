prep_q70b_hh_income_fishing <- function(.data, iso3){
  

  conversion <- convert_money(iso3)
  
  hhs_Q70 <- .data %>%
    dplyr::filter (dplyr::between(
      as.numeric(`70_hh_average_income`), 10, 1.00e+09)) %>%
    droplevels()
  
  Q70_lenght <-
    tapply(hhs_Q70$`70_hh_average_income`,
           hhs_Q70$maa,
           length)
  
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
      "11k_income_other",
      "70_hh_average_income"
    )]
  
  income_source[is.na(income_source)] <- 0
  income_source$`70_hh_average_income` <-
    as.numeric(as.character(income_source$`70_hh_average_income`))
  income_source <- data.frame(income_source)
  
  ## Agregate income
  income_summary <-
    aggregate(. ~ maa,
              FUN = mean,
              na.rm = TRUE,
              data = income_source)
  
  HH_avg_income <- data.frame (
    N = Q70_lenght,
    Total_Income_USD = round(income_summary$X70_hh_average_income, 1),
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
  
  HH_avg_income$Fishing_related <-
    HH_avg_income$Artisinal_Fishing + 
    HH_avg_income$Industrial_Fishing +
    HH_avg_income$Buying_Trading +
    HH_avg_income$Processing_Fish
  
  # HH_avg_income <-
  #   rbind(
  #     HH_avg_income,
  #     "Mean ± SE" = c(
  #       sum(HH_avg_income$N),
  #       compute_summary_line(HH_avg_income$Total_Income_USD, 1),
  #       compute_summary_line(HH_avg_income$Farming, 1),
  #       compute_summary_line(HH_avg_income$Harvesting, 1),
  #       compute_summary_line(HH_avg_income$Artisinal_Fishing, 1),
  #       compute_summary_line(HH_avg_income$Industrial_Fishing, 1),
  #       compute_summary_line(HH_avg_income$Buying_Trading, 1),
  #       compute_summary_line(HH_avg_income$Processing_Fish, 1),
  #       compute_summary_line(HH_avg_income$Aquaculture, 1),
  #       compute_summary_line(HH_avg_income$Extraction, 1),
  #       compute_summary_line(HH_avg_income$Tourism, 1),
  #       compute_summary_line(HH_avg_income$Other, 1),
  #       compute_summary_line(HH_avg_income$Fishing_related)
  #     )
  #   )
  
  Q70_summary <-
    tibble::rownames_to_column(HH_avg_income[, c(1:2, 13)], "MA name")
  colnames(Q70_summary) <-
    c("MA name", "N", "Proportion (%)", "Fishing_related")
  
  Q70b <- Q70_summary[, c(1:2)]
  Q70b[["Proportion (%)"]] <- round(
    as.numeric(Q70_summary$`Proportion (%)`) * as.numeric(as.character(Q70_summary$Fishing_related)) * conversion /
      100,1)
  # 
  # Q70b <-
  #   cbind (Q70_summary[, c(1:2)],
  #          "Proportion (%)" = round(
  #            as.numeric(Q70_summary$`Proportion (%)`) * as.numeric(as.character(Q70_summary$Fishing_related)) * conversion /
  #              100,
  #            1
  #          ))
  
  Q70b <- clean_plot_data(Q70b)
  colnames(Q70b) <- c("MA name", "N", "Average")
  Q70b
}

plot_q70b_hh_income_fishing <- function(.data, ...){

         dots <- list(...)
         .data_plot <- prep_q70b_hh_income_fishing(.data, dots$iso3)
         
         plot_horiz_bar(
           .data_plot,
           title = "Average household income from fishing-related activities in USD",
           y_var = Average,
           y_title = "US Dollars ($)",
           limits = NULL,
           breaks = waiver()
         )
         #Plot
         # plot_Q70b <-
            # ggplot(.data_plot, aes(`MA name`, Average, N = N)) +
            # theme_rare() +
            # geom_col(alpha = 0.8, fill = "#005BBB") +
            # ggtitle("Average household income from fishing-related activities in USD") +
            # xlab (NULL) + ylab ("Average income in USD") +
            # coord_flip(clip = "on")
         # 
         # ggplotly(plot_Q70b, height = 750)
}