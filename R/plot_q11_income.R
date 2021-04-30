prep_q11_income <- function(.data){

}

plot_q11_income <- function(.data, use_plotly = TRUE){

         hhs_Q11 <- selectedData()[, c("ma_name",
                                       "11a_income_farming",
                                       "11b_income_harvesting",
                                       "11c_income_fishing_artisanal",
                                       "11d_income_fishing_industrial",
                                       '11e_income_buying_trading',
                                       '11f_income_processing',
                                       '11g_income_aquaculture',
                                       '11h_income_extraction',
                                       "11i_income_tourism",
                                       "11k_income_other"
                                    )] %>%
                      filter(ma_name != "")
         
         income_source <- hhs_Q11
           
         Q11_length <-
            as.vector(tapply(
               income_source$ma_name,
               income_source$ma_name,
               length
            ))
         
         income_source[is.na(income_source)] <- 0
         income_source <- data.frame(income_source)
         income_summary <-
            aggregate(. ~ ma_name,
                      FUN = mean,
                      na.rm = TRUE,
                      data = income_source)
         
         HH_avg_income <- data.frame (
            ma_name = income_summary$ma_name,
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
               mean_sem(HH_avg_income$Farming, 1),
               mean_sem(HH_avg_income$Harvesting, 1),
               mean_sem(HH_avg_income$Artisinal_Fishing, 1),
               mean_sem(HH_avg_income$Industrial_Fishing, 1),
               mean_sem(HH_avg_income$Buying_Trading, 1),
               mean_sem(HH_avg_income$Processing_Fish, 1),
               mean_sem(HH_avg_income$Aquaculture, 1),
               mean_sem(HH_avg_income$Extraction, 1),
               mean_sem(HH_avg_income$Tourism, 1),
               mean_sem(HH_avg_income$Other, 1)
            )
         )
         
         Q11_summary_long <-
            HH_avg_income_mean %>% pivot_longer(
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
         
        Q11 <- Q11_summary_long %>% 
                filter(`MA name` != "") %>%
                    dplyr::mutate(`Proportion (%)` = as.numeric(`Proportion (%)`)) 
           
         #Plot
         plot_Q11 <-
            ggplot(Q11, aes(
               x = `MA name`,
               y = `Proportion (%)`,
               fill = Source,
               N = N
            )) +
            theme_rare + theme(legend.position = "right",
                               legend.text = element_text(size = 10)) +
            geom_bar(
               position = position_stack(reverse = TRUE),
               stat = "identity",
               alpha = 0.8
            ) +
            scale_fill_brewer(palette = "Spectral", 
                              direction = -1) +
            guides(fill = guide_legend(reverse = FALSE)) +
            ggtitle("Household source inconme and \nproportional income contribution") +
            xlab (NULL) + ylab ("Proportion (%)") +
            coord_flip(clip = "on")
         
         ggplotly(plot_Q11, height = 750)
}