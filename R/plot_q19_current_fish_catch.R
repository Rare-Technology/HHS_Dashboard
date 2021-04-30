
prep_q19_current_fish_catch <- function(.data){

}

plot_q19_current_fish_catch <- function(.data, use_plotly = TRUE){
hhs_Q19 <- selectedData()[,c("ma_name", "19_current_fish_catch")] %>%
            filter(`19_current_fish_catch` != "") %>%
               droplevels()
         #proportion
         Q19_summary <- proportion(hhs_Q19[[2]], 
                                   hhs_Q19[[1]], 
                                   3, type = "5")
         Q19 <- Q19_summary %>% filter (`MA name` != "Mean ± SE")
         
         #Combine categories in 3 choices
         Q19$Declined <- as.numeric(Q19$Declined.a.lot) + as.numeric(Q19$Declined.slightly)
         Q19$Improved <- as.numeric(Q19$Improved.slightly) + as.numeric(Q19$Improved.heavily)
         
         #Format table for Rmarkdown report
         Q19_summary <-
            rbind(Q19[, c("MA name",
                          "N",
                          "Declined",
                          "Stayed.the.same",
                          "Improved")],
                  `Mean ± SE` = c('',
                     sum(as.numeric(Q19$N)),
                     mean_sem(Q19$Declined, 1),
                     mean_sem(Q19$Stayed.the.same, 1),
                     mean_sem(Q19$Improved, 1)
                  ))
         colnames(Q19_summary) <-
            c("MA name",
              "N",
              "Declined",
              "Stayed the same",
              "Improved")
          
         #plot
         Q19_summary_long <-
            Q19_summary %>% pivot_longer(
               cols = c("Declined", "Stayed the same", "Improved"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q19_summary_long$key <-
            factor(
               Q19_summary_long$key,
               levels = c("Declined", "Stayed the same", "Improved")
            )
         
         Q19 <- data_4plot(Q19_summary_long)
         
         plot_Q19 <-
            ggplot(Q19, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap( ~ key) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            ggtitle("Proportion of fishers who perceived that their catch \nremained stable or increased over the past 2 years"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(clip = "on")
         ggplotly(plot_Q19, height = 750)
}