prep_q11_income <- function(.data){

  hhs_Q11 <- .data %>% 
    dplyr::select(
      maa, 
      `11a_income_farming`,
      `11b_income_harvesting`,
      `11c_income_fishing_artisanal`,
      `11d_income_fishing_industrial`,
      `11e_income_buying_trading`,
      `11f_income_processing`,
      `11g_income_aquaculture`,
      `11h_income_extraction`,
      `11i_income_tourism`,
      `11k_income_other`
    ) %>%
    dplyr::filter(maa != "")
  
  income_source <- hhs_Q11
  
  Q11_length <- as.vector(tapply(
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
  
  HH_avg_income <- data.frame(
    maa = income_summary$maa,
    N = Q11_length,
    Farming = round(
      income_summary$X11a_income_farming / rowSums(income_summary[, c(2:11)], 
                                                   na.rm =  TRUE) * 100,
      1
    ),
    Harvesting = round(
      income_summary$X11b_income_harvesting / rowSums(income_summary[, c(2:11)], 
                                                      na.rm = TRUE) * 100,
      1
    ),
    Artisinal_Fishing = round(
      income_summary$X11c_income_fishing_artisanal / rowSums(income_summary[, c(2:11)], 
                                                             na.rm = TRUE) * 100,
      1
    ),
    Industrial_Fishing = round(
      income_summary$X11d_income_fishing_industrial / rowSums(income_summary[, c(2:11)], 
                                                              na.rm = TRUE) * 100,
      1
    ),
    Buying_Trading = round(
      income_summary$X11e_income_buying_trading / rowSums(income_summary[, c(2:11)], 
                                                          na.rm = TRUE) * 100,
      1
    ),
    Processing_Fish = round(
      income_summary$X11f_income_processing / rowSums(income_summary[, c(2:11)], 
                                                      na.rm = TRUE) * 100,
      1
    ),
    Aquaculture = round(
      income_summary$X11g_income_aquaculture / rowSums(income_summary[, c(2:11)], 
                                                       na.rm = TRUE) * 100,
      1
    ),
    Extraction = round(
      income_summary$X11h_income_extraction / rowSums(income_summary[, c(2:11)], 
                                                      na.rm = TRUE) * 100,
      1
    ),
    Tourism = round(
      income_summary$X11i_income_tourism / rowSums(income_summary[, c(2:11)], 
                                                   na.rm = TRUE) * 100,
      1
    ),
    Other = round(
      income_summary$X11k_income_other / rowSums(income_summary[, c(2:11)], 
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
  
  Q11_summary_long <-
    HH_avg_income_mean %>% tidyr::pivot_longer(
      cols = c(
        "Farming",
        "Harvesting",
        "Artisinal_Fishing",
        "Industrial_Fishing",
        "Buying_Trading",
        "Processing_Fish",
        "Aquaculture",
        "Extraction",
        "Tourism",
        "Other"
      ),
      names_to = "source",
      values_to = "Proportion (%)"
    )
  Q11_summary_long$source <-
    factor(
      Q11_summary_long$source,
      levels = c(
        "Farming",
        "Harvesting",
        "Artisinal_Fishing",
        "Industrial_Fishing",
        "Buying_Trading",
        "Processing_Fish",
        "Aquaculture",
        "Extraction",
        "Tourism",
        "Other"
      )
    )
  colnames(Q11_summary_long) <-
    c("MA name", "N", "Source", "Proportion (%)")
  
  Q11_summary_long %>% 
    dplyr::filter(`MA name` != "") %>%
    dplyr::mutate(`Proportion (%)` = as.numeric(`Proportion (%)`)) 
  

}

plot_q11_income <- function(.data, use_plotly = TRUE){

         .data_plot <- prep_q11_income(.data)
          
      
         plot_horiz_bar(
           .data_plot,
           title = "Household source inconme and \nproportional income contribution",
           palette = "Spectral",
           type = "stacked",
           stack_var = `Source`,
           guide_reverse = FALSE
         )

}