
prep_q17_fishing_low_profit <- function(.data){

}

plot_q17_fishing_low_profit <- function(.data, use_plotly = TRUE){

         hhs_Q17 <- selectedData()[,c("ma_name", "17_fishing_low_profit")] %>%
            filter(`17_fishing_low_profit` != "") %>%
               droplevels()
         
         hhs_Q17$`17_fishing_low_profit` <- 
            recode_factor(hhs_Q17$`17_fishing_low_profit`,
                           "7 per week" = "Everyday",
                           "5-6 per week" = "Five or six times per week",
                           "3-4 per week" = "Three or four times per week",
                           "1-2 per week" = "One or two times per week",
                           "More than 1-2 times per week" = "More than few times per week")
         
         Q17_summary <- proportion(hhs_Q17[[2]],
                                   hhs_Q17[[1]],
                                   3, type = length(unique(hhs_Q17[[2]])))
            Q17_summary_long <-
               Q17_summary %>% pivot_longer(
                  cols = c(3:ncol(Q17_summary)),
                  names_to = "Frequency",
                  values_to = "Proportion (%)")
            
            Q17_summary_long$Frequency <- str_replace_all(Q17_summary_long$Frequency, "[.]", " ")
            
            Q17_summary_long$Frequency <- factor(Q17_summary_long$Frequency,
                                          levels = c("Once or never",
                                                     "A few times",
                                                     "A few times per month",
                                                     "One or two times per week",
                                                     "More than few times per week",
                                                     "Three or four times per week",
                                                     "Five or six times per week",
                                                     "Everyday"))
            Q17 <- data_4plot(Q17_summary_long)
  
            plot_Q17 <-
               ggplot(Q17, aes(`MA name`, `Proportion (%)`, N = N)) +
               geom_bar(aes(fill = Frequency),
                        position = position_stack(reverse = TRUE),
                        stat = "identity",
                        alpha = 0.8 ) +
               theme_rare + 
               scale_fill_brewer(palette = "Blues", direction = -1) +
               ggtitle("Frequency that the main fisher go fishing \nduring less profitable fishing season"
               ) +
               scale_y_continuous(limits = c(0, 105),
                                  breaks = seq(0, 100, 25)) +
               xlab (NULL) + 
               ylab ("Proportion (%)") + 
               coord_flip(clip = "on") +
               theme(legend.position = "right") +
               guides(fill = guide_legend(reverse = TRUE)) 
            
            ggplotly(plot_Q17, height = 750)
}