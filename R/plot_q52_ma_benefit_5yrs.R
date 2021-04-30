
prep_q52_ma_benefit_5yrs <- function(.data){

}

plot_q52_ma_benefit_5yrs <- function(.data, use_plotly = TRUE){
hhs_Q52 <- selectedData()[,c("ma_name", "52_ma_benefit_5yrs")] %>%
                        filter(`52_ma_benefit_5yrs` %in% c("Yes", "Unsure", "No")) %>%
                           rbind (tibble(ma_name = c(NA,NA,NA),
                                         `52_ma_benefit_5yrs` = c("Yes", 
                                                                  "Unsure", 
                                                                  "No"))) %>%
                        droplevels()
            
         Q52_summary <- proportion(hhs_Q52[[2]],
                                    hhs_Q52[[1]],
                                   3,3)
         #pivot table
         Q52_summary_long <-
            Q52_summary %>% pivot_longer(
               cols = c("No", "Unsure", "Yes"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q52 <- data_4plot(Q52_summary_long)
         
         #Plot
         plot_Q52 <-
            ggplot(Q52, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, scale = input$x_axis) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of participants who are confident they will continue to \nbenefit from community management of the fishery for the next 5 years"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q52, height = 750)
}