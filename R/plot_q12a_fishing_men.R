
prep_q12a_fishing_men <- function(.data){

}

plot_q12a_fishing_men <- function(.data, use_plotly = TRUE){

         hhs_Q12 <- selectedData()[,c("ma_name", "12a_fishing_men", 
                                      "12b_fishing_women", "12c_fishing_children")] %>%
            filter(`12a_fishing_men` < 10 & `12a_fishing_men` != "") %>%
            filter(`12b_fishing_women` < 10 & `12b_fishing_women` != "") %>%
            filter(`12c_fishing_children` < 10 & `12c_fishing_children` != "")
         
         Q12_summary <- hhs_Q12 %>%
            group_by(ma_name) %>%
            summarise(
               "N" = n(),
               "fisher men" = round(mean(`12a_fishing_men`, na.rm =
                                            TRUE), 1),
               "fisher women" = round(mean(`12b_fishing_women`, na.rm =
                                              TRUE), 1),
               "fisher children" = round(mean(`12c_fishing_children`, na.rm =
                                                 TRUE), 1)
            )
         
         Q12_summary <- rbind(Q12_summary,
                              c(
                                 NA,
                                 sum(Q12_summary$N),
                                 mean_sem(Q12_summary$`fisher men`, 1),
                                 mean_sem(Q12_summary$`fisher women`, 1),
                                 mean_sem(Q12_summary$`fisher children`, 1)
                              ))
        
         #plot
         Q12_summary_long <-
            Q12_summary %>% pivot_longer(
               cols = c("fisher men", "fisher women", "fisher children"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q12_summary_long$key <-
            factor(
               Q12_summary_long$key,
               levels = c("fisher men", "fisher women", "fisher children")
            )
         
         colnames(Q12_summary_long) <- c("MA name", "N", "Fishers", "Proportion (%)")
         
         Q12 <- data_4plot(Q12_summary_long)
         
         colnames(Q12) <- c("MA name", "N", "Fishers", "Average")
         
         plot_Q12 <-
            ggplot(Q12, aes(`MA name`, `Average`, N = N)) +
            facet_wrap( ~ Fishers) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Average number of household members \nthat go fishing regularly") +
            scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
            labs (x= NULL, y = "\nNumber of people") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q12, height = 750)
}