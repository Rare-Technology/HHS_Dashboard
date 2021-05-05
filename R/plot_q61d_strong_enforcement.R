
prep_q61d_strong_enforcement <- function(.data){

}

plot_q61d_strong_enforcement <- function(.data, use_plotly = TRUE){
hhs_Q61d <- .data[,c("maa", "61d_strong_enforcement")] %>%
                           dplyr::filter (`61d_strong_enforcement` %in% c(1:5)) %>%
                              rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
          
          Q61d_summary <- proportion ( hhs_Q61d$`61d_strong_enforcement`,
                                       hhs_Q61d$maa,
                                       3,5)
         
           colnames(Q61d_summary) <- c("MA name",
                                      "N",
                                      "Strongly disagree",
                                      "Disagree",
                                      "Neither agree nor disagree (%)",
                                      "Agree",
                                      "Strongly agree")
          
          Q61d_summary_grouped <- Q61d_summary %>%
                                       dplyr::filter(`MA name` != "Mean ± SE")
          
          Q61d_summary_grouped$`Agree (%)` <- as.numeric(Q61d_summary_grouped$Agree) + 
                                             as.numeric(Q61d_summary_grouped$`Strongly agree`)
          
          Q61d_summary_grouped$`Neither agree nor disagree (%)` <- 
             as.numeric(Q61d_summary_grouped$`Neither agree nor disagree`)
          
          Q61d_summary_grouped$`Disagree (%)` <- as.numeric(Q61d_summary_grouped$Disagree) + 
             as.numeric(Q61d_summary_grouped$`Strongly disagree`)
          
         #pivot table
         Q61d_summary_long <-
            as.data.frame(
               Q61d_summary_grouped[, c("MA name", "N",
                               "Disagree (%)",
                               "Neither agree nor disagree (%)",
                               "Agree (%)" )] %>% 
                  pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
               )
         Q61d_summary_long$key <-
            factor(
               Q61d_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         Q61d <- clean_plot_data(Q61d_summary_long)
         
         #Plot
         plot_Q61d <-
            ggplot(Q61d, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(25)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of fishers who believe that \nit is important to have a strong enforcement system"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61d, height = 750)
}