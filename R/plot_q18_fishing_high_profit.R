
prep_q18_fishing_high_profit <- function(.data){

}

plot_q18_fishing_high_profit <- function(.data, use_plotly = TRUE){

         hhs_Q18 <- selectedData()[,c("ma_name", "18_fishing_high_profit")] %>%
            filter(`18_fishing_high_profit` != "") %>%
               droplevels()
         
         hhs_Q18$`18_fishing_high_profit` <- 
            recode_factor(hhs_Q18$`18_fishing_high_profit`,
                          "7 per week" = "Everyday",
                          "7 times per week" = "Everyday",
                          "6 times per week" = "Five or six times per week",
                          "5-6 per week" = "Five or six times per week",
                          "3-4 per week" = "Three or four times per week",
                          "3-4 times per week" = "Three or four times per week",
                          "1-2 per week" = "One or two times per week",
                          "More than 1-2 times per week" = "More than few times per week")
         
         Q18_summary <- proportion(hhs_Q18[[2]],
                                   hhs_Q18[[1]],
                                   3, type = length(unique(hhs_Q18[[2]])))
         Q18_summary_long <-
            Q18_summary %>% pivot_longer(
               cols = c(3:ncol(Q18_summary)),
               names_to = "Frequency",
               values_to = "Proportion (%)")
         
         Q18_summary_long$Frequency <- str_replace_all(Q18_summary_long$Frequency, "[.]", " ")
         
         Q18_summary_long$Frequency <- factor(Q18_summary_long$Frequency,
                                              levels = c("Once or never",
                                                         "A few times",
                                                         "A few times per month",
                                                         "One or two times per week",
                                                         "More than few times per week",
                                                         "Three or four times per week",
                                                         "Five or six times per week",
                                                         "Everyday"))
         Q18 <- data_4plot(Q18_summary_long)
         
         plot_Q18 <-
            ggplot(Q18, aes(`MA name`, `Proportion (%)`, N=N)) +
            geom_bar(aes(fill = Frequency),
                     position = position_stack(reverse = TRUE),
                     stat = "identity",
                     alpha = 0.8 ) +
            theme_rare + 
            scale_fill_brewer(palette = "Blues", direction = -1) +
            ggtitle("Frequency that the main fisher go fishing \nduring the high profitable fishing season"
            ) +
            scale_y_continuous(limits = c(0, 105),
                               breaks = seq(0, 100, 25)) +
            xlab (NULL) + 
            ylab ("Proportion (%)") + 
            coord_flip(clip = "on") +
            theme(legend.position = "right") +
            guides(fill = guide_legend(reverse = TRUE)) 
         
         ggplotly(plot_Q18, height = 750)
}