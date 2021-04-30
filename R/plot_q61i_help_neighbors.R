
prep_q61i_help_neighbors <- function(.data){

}

plot_q61i_help_neighbors <- function(.data, use_plotly = TRUE){
hhs_Q61i <- selectedData()[,c("ma_name", "61i_help_neighbors")] %>%
                        filter(`61i_help_neighbors` %in% c(1:5)) %>%
                           rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
         
         Q61i_summary <- proportion(hhs_Q61i$`61i_help_neighbors`,
                                    hhs_Q61i$ma_name,
                                    rounding = 3,
                                    type = 5)
         
         colnames(Q61i_summary) <-
            c( "MA name",
               "N",
               "Strongly disagree",
               "Disagree",
               "Neither agree nor disagree",
               "Agree",
               "Strongly agree"
            )
         
         Q61i_summary_grouped <- Q61i_summary %>% 
            filter (`MA name` != "Mean ± SE")
         
         Q61i_summary_grouped$`Agree (%)` <- as.numeric(Q61i_summary_grouped$Agree) + 
            as.numeric(Q61i_summary_grouped$`Strongly agree`)
         
         Q61i_summary_grouped$`Neither agree nor disagree (%)` <- 
            as.numeric(Q61i_summary_grouped$`Neither agree nor disagree`)
         
         Q61i_summary_grouped$`Disagree (%)` <- as.numeric(Q61i_summary_grouped$Disagree) + 
            as.numeric(Q61i_summary_grouped$`Strongly disagree`)
         
         #pivot table
         Q61i_summary_long <-
            as.data.frame(
               Q61i_summary_grouped[, c("MA name", "N",
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
         Q61i_summary_long$key <-
            factor(
               Q61i_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         
         Q61i <- data_4plot(Q61i_summary_long)
         
         #Plot
         plot_Q61i <-
            ggplot(Q61i, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(20)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who feel that it is important for them \nto be able to help their neighbors in times of need"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61i, height = 750)
}