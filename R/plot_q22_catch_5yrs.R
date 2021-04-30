
prep_q22_catch_5yrs <- function(.data){

}

plot_q22_catch_5yrs <- function(.data, use_plotly = TRUE){

         hhs_Q22 <-selectedData()[,c("ma_name", "22_catch_5yrs")] %>%
            filter(`22_catch_5yrs` != '') %>%
               rbind(c(NA,"Declines a lot"), c(NA,"Declines slightly"), 
                     c(NA,"Stays the same"), c(NA,"Improves slightly"),
                     c(NA,"Improves heavily")) %>%
                  droplevels()
         #proportion
         Q22_summary <-
            proportion(hhs_Q22[[2]], 
                       hhs_Q22[[1]], 
                       3,5)
         #remove summary row
         Q22_summary_bind <- Q22_summary %>%
                              filter(`MA name` != "Mean ± SE") %>%
                                 droplevels()
         #Combine categories
         Q22_summary_bind$Declines <-
            as.numeric(Q22_summary_bind$Declines.a.lot) + 
            as.numeric(Q22_summary_bind$Declines.slightly)
         
         Q22_summary_bind$Improves <-
            as.numeric(Q22_summary_bind$Improves.slightly) + 
            as.numeric(Q22_summary_bind$Improves.heavily)
         
         #Format table for Rmarkdown report
         Q22_summary <-
            rbind(
               Q22_summary_bind[, c("MA name",
                                       "N",
                                       "Declines",
                                       "Stays.the.same",
                                       "Improves")],
                  `Mean ± SE` = c('',
                     sum(as.numeric(Q22_summary_bind$N)),
                     mean_sem(Q22_summary_bind$Declines, 1),
                     mean_sem(Q22_summary_bind$Stays.the.same, 1),
                     mean_sem(Q22_summary_bind$Improves), 1)
               )
         
         Q22_summary <- Q22_summary %>% filter (`MA name` != "")
         #rename colums
         colnames(Q22_summary) <-
            c("MA name",
              "N",
              "Decline (%)",
              "Stay the same (%)",
              "Improve (%)")
                     
         #pivot table
         Q22_summary_long <-
            Q22_summary %>% pivot_longer(
               cols = c("Decline (%)", "Stay the same (%)", "Improve (%)"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q22_summary_long$key <-
            factor(
               Q22_summary_long$key,
               levels = c("Decline (%)", "Stay the same (%)", "Improve (%)")
            )
         
         Q22 <- data_4plot (Q22_summary_long) 
         
         #Plot
         plot_Q22 <-
            ggplot(Q22, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of fishers who perceived that their catch \nwill remain stable or increase over the next 5 years"
            ) +
            xlab (NULL) + ylab ("\n\nProportion (%)") + coord_flip(clip = "on")
         ggplotly(plot_Q22, height = 750)
}