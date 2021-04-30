prep_q13_processing <- function(.data){

}

plot_q13_processing <- function(.data, use_plotly = TRUE){

         hhs_Q13 <- selectedData()[,c("ma_name", 
                                     "13a_processing_men",
                                     "13b_processing_women",
                                     "13c_processing_children")]
         
         Q13_summary <- hhs_Q13 %>%
            group_by(ma_name) %>%
            summarise(
               "N" = n(),
               "men" = round(mean(`13a_processing_men`, na.rm =
                                            TRUE), 1),
               "women" = round(mean(`13b_processing_women`, na.rm =
                                              TRUE), 1),
               "children" = round(mean(`13c_processing_children`, na.rm =
                                                 TRUE), 1)
            )
         
         Q13_summary <- rbind(Q13_summary,
                              c(
                                 NA,
                                 sum(Q13_summary$N),
                                 mean_sem(Q13_summary$`men`, 1),
                                 mean_sem(Q13_summary$`women`, 1),
                                 mean_sem(Q13_summary$`children`, 1)
                              ))
         #plot
         Q13_summary_long <-
            Q13_summary %>% pivot_longer(
               cols = c("men", "women", "children"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q13_summary_long$key <-
            factor(
               Q13_summary_long$key,
               levels = c("men", "women", "children")
            )
         
         colnames(Q13_summary_long) <- c("MA name", "N", "Processors", "Proportion (%)")
         
         Q13 <- data_4plot(Q13_summary_long)
         
         colnames(Q13) <- c("MA name", "N", "Processors", "Average")
         
         plot_Q13 <-
            ggplot(Q13, aes(`MA name`, `Average`, N = N)) +
            facet_wrap( ~ Processors) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Average number of household members \nthat participate in post-procesing activities") +
            scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
            labs (x= NULL, y = "\nNumber of people") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q13, height = 750)
}