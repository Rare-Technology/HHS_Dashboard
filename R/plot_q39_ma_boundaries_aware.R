
prep_q39_ma_boundaries_aware <- function(.data){

}

plot_q39_ma_boundaries_aware <- function(.data, use_plotly = TRUE){
hhs_Q39 <- selectedData()[,c("ma_name", "39_ma_boundaries_aware")] %>%
            subset(`39_ma_boundaries_aware` %in% c("Agree", "Neither", 
                                                   "Disagree",
                                                   "Strongly agree",
                                                   "Strongly disagree")) %>%
            rbind(c(NA, "Agree"), 
                  c(NA, "Neither"), 
                  c(NA, "Disagree"), 
                  c(NA, "Strongly agree"), 
                  c(NA, "Strongly disagree")) %>%
            droplevels()
         
         #proportion
         Q39_summary <-
            proportion(hhs_Q39[[2]],
                       hhs_Q39[[1]],
                       3,5)
         #rename
         colnames(Q39_summary) <-
            c(
               "MA name",
               "N",
               "Agree",
               "Disagree",
               "Neither",
               "Strongly agree",
               "Strongly disagree"
            )
         
         #pivot table
         Q39_summary_long <-
            Q39_summary %>% pivot_longer(
               cols = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q39_summary_long$key <-
            factor(
               Q39_summary_long$key,
               levels = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               )
            )
         
         Q39 <- data_4plot (Q39_summary_long)
         
         #Plot
         plot_Q39 <-
            ggplot(Q39, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap(
               ~ key,
               labeller = label_wrap_gen(20),
               ncol = 5
            ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of fishers aware of the boundaries \nof the fisheries management/managed access area"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q39, height = 750)
}