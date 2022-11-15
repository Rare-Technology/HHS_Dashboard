prep_q14_income <- function(.data){

  hhs_Q14 <- .data %>% 
    dplyr::select(
      maa, 
      `14a_income_farming`,
      `14b_income_harvesting`,
      `14c_income_fishing_artisanal`,
      `14d_income_fishing_aquaculture`,
      `14e_income_buying_trading`,
      `14f_income_processing`,
      `14g_income_extraction`,
      `14h_income_tourism`,
      # `14i_income_other_wage`,
      `14j_income_industrial`,
      `14k_income_other`
    ) %>%
    dplyr::filter(maa != "")
  
  income_source <- hhs_Q14
  
  Q14_length <- as.vector(tapply(
      income_source$maa,
      income_source$maa,
      length
    ))
  
  income_source[is.na(income_source)] <- 0
  income_source <- data.frame(income_source)
  income_summary <- aggregate(. ~ maa,
              FUN = mean,
              na.rm = TRUE,
              data = income_source)
  
  HH_avg_income <- tibble::tibble(
    maa = income_summary$maa,
    N = Q14_length,
    Farming = round(
      income_summary$X14a_income_farming / rowSums(income_summary[, c(2:11)], 
                                                   na.rm =  TRUE) * 100,
      1
    ),
    Harvesting = round(
      income_summary$X14b_income_harvesting / rowSums(income_summary[, c(2:11)], 
                                                      na.rm = TRUE) * 100,
      1
    ),
    `Artisinal Fishing` = round(
      income_summary$X14c_income_fishing_artisanal / rowSums(income_summary[, c(2:11)], 
                                                             na.rm = TRUE) * 100,
      1
    ),
    `Industrial Fishing` = round(
      income_summary$X14j_income_industrial / rowSums(income_summary[, c(2:11)], 
                                                      na.rm = TRUE) * 100,
      1
    ),
    Aquaculture = round(
      income_summary$X14d_income_fishing_aquaculture / rowSums(income_summary[, c(2:11)], 
                                                              na.rm = TRUE) * 100,
      1
    ),
    `Buying Trading` = round(
      income_summary$X14e_income_buying_trading / rowSums(income_summary[, c(2:11)], 
                                                          na.rm = TRUE) * 100,
      1
    ),
    `Processing Fish` = round(
      income_summary$X14f_income_processing / rowSums(income_summary[, c(2:11)], 
                                                      na.rm = TRUE) * 100,
      1
    ),
    Extraction = round(
      income_summary$X14g_income_extraction / rowSums(income_summary[, c(2:11)], 
                                                      na.rm = TRUE) * 100,
      1
    ),
    Tourism = round(
      income_summary$X14h_income_tourism / rowSums(income_summary[, c(2:11)], 
                                                   na.rm = TRUE) * 100,
      1
    ),
    # Other_Wage_Labor = round(
    #   income_summary$X14i_income_other_wage / rowSums(income_summary[, c(2:11)],
    #                                                   na.rm = TRUE) * 100,
    #   1
    # ),
    Other = round(
      income_summary$X14k_income_other / rowSums(income_summary[, c(2:11)], 
                                                 na.rm = TRUE) * 100,
      1
    )
  )
  
  HH_avg_income_mean <- rbind(
    HH_avg_income,
    "Mean ± SE" = c(
      NA,
      sum(HH_avg_income$N),
      compute_summary_line(HH_avg_income$Farming, 1),
      compute_summary_line(HH_avg_income$Harvesting, 1),
      compute_summary_line(HH_avg_income$`Artisinal Fishing`, 1),
      compute_summary_line(HH_avg_income$`Industrial Fishing`, 1),
      compute_summary_line(HH_avg_income$Aquaculture, 1),
      compute_summary_line(HH_avg_income$`Buying Trading`, 1),
      compute_summary_line(HH_avg_income$`Processing Fish`, 1),
      compute_summary_line(HH_avg_income$Extraction, 1),
      compute_summary_line(HH_avg_income$Tourism, 1),
      # compute_summary_line(HH_avg_income$Other_Wage_Labor, 1),
      compute_summary_line(HH_avg_income$Other, 1)
    )
  )
  
  Q14_summary_long <-
    HH_avg_income_mean %>% tidyr::pivot_longer(
      cols = c(
        "Farming",
        "Harvesting",
        "Artisinal Fishing",
        "Industrial Fishing",
        "Buying Trading",
        "Processing Fish",
        "Aquaculture",
        "Extraction",
        "Tourism",
        # "Other_Wage_Labor",
        "Other"
      ),
      names_to = "source",
      values_to = "Proportion (%)"
    )
  Q14_summary_long$source <-
    factor(
      Q14_summary_long$source,
      levels = c(
        "Farming",
        "Harvesting",
        "Artisinal Fishing",
        "Industrial Fishing",
        "Buying Trading",
        "Processing Fish",
        "Aquaculture",
        "Extraction",
        "Tourism",
        # "Other_Wage_Labor",
        "Other"
      )
    )
  colnames(Q14_summary_long) <-
    c("MA name", "N", "Source", "Proportion (%)")
  
  Q14_summary_long %>% 
    dplyr::filter(`MA name` != "") %>%
    dplyr::mutate(`Proportion (%)` = as.numeric(`Proportion (%)`)) 
  

}

plot_q14_income <- function(.data, ...){
  .data_plot <- prep_q14_income(.data)
          
  p <- plot_bubble(
    .data_plot,
    title = "Household source income and \nproportional income contribution",
    x_var = Source
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}