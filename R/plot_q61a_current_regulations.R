
prep_q61a_current_regulations <- function(.data){

}

plot_q61a_current_regulations <- function(.data, use_plotly = TRUE){
hhs_Q61a <- selectedData()[,c("ma_name", "61a_current_regulations")] %>%
                           filter(`61a_current_regulations` %in% c(1:5)) %>%
                              rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
        
         Q61a_summary <- proportion (hhs_Q61a$`61a_current_regulations`,
                                      hhs_Q61a$ma_name,
                                      3,5)
         
         Q61a_summary_grouped <- Q61a_summary %>%
                                   filter (`MA name` != "Mean ± SE")
         #grouped
         Q61a_summary_grouped$Disagree <-  as.numeric(Q61a_summary_grouped$X1) +
                                           as.numeric(Q61a_summary_grouped$X2)
         
         Q61a_summary_grouped$Neither <-  
            as.numeric(Q61a_summary_grouped$X3) 
         
         Q61a_summary_grouped$Agree <-  as.numeric(Q61a_summary_grouped$X4) +
                                        as.numeric(Q61a_summary_grouped$X5)
         
         Q61a_summary <-
            rbind(
               Q61a_summary_grouped[,c("MA name", "N", 
                                       "Disagree",
                                       "Neither",
                                       "Agree")],
                c(NA,
                  sum(as.numeric(Q61a_summary_grouped$N)),
                  mean_sem(Q61a_summary_grouped$Disagree, 1),
                  mean_sem(Q61a_summary_grouped$Neither, 1),
                  mean_sem(Q61a_summary_grouped$Agree, 1)
               )
            )
         
         colnames(Q61a_summary) <-
            c("MA name",
              "N",
              "Disagree (%)",
              "Neither (%)",
              "Agree (%)")
         #pivot table
         Q61a_summary_long <-
            Q61a_summary %>% pivot_longer(
               cols = c(
                  "Disagree (%)",
                  "Neither (%)",
                  "Agree (%)"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q61a_summary_long$key <-
            factor(
               Q61a_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither (%)",
                  "Agree (%)"
               )
            )
         
         Q61a <- data_4plot(Q61a_summary_long)
            
         #Plot
         plot_Q61a <-
            ggplot(Q61a, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        labeller = label_wrap_gen(20)) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members who feel that current fishing regulations \nare effective at managing the fishery and at ensuring catches remain stable"
            ) +
            xlab (NULL) + ylab ("\nProportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61a, height = 750)
}