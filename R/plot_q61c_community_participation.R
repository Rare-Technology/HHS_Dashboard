
prep_q61c_community_participation <- function(.data){

}

plot_q61c_community_participation <- function(.data, ...){
hhs_Q61c <- .data[,c("maa", "61c_community_participation")] %>%
                        dplyr::filter (`61c_community_participation` %in% c(1:5)) %>%
                           rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5))
         
         Q61c_summary <- proportion (hhs_Q61c$`61c_community_participation`,
                                     hhs_Q61c$maa,
                                     3,5)
         
         colnames(Q61c_summary) <- c("MA name",
                                      "N",
                                      "Strongly disagree",
                                      "Disagree",
                                      "Neither agree nor disagree (%)",
                                      "Agree",
                                      "Strongly agree")
         
         Q61c_summary_grouped <- Q61c_summary %>%
                                       dplyr::filter (`MA name` != "Mean ± SE")
          
         Q61c_summary_grouped$`Agree (%)` <- as.numeric(Q61c_summary_grouped$Agree) + 
                                     as.numeric(Q61c_summary_grouped$`Strongly agree`)
         
         Q61c_summary_grouped$`Neither agree nor disagree (%)` <- 
                                    as.numeric(Q61c_summary_grouped$`Neither agree nor disagree`)
         
         Q61c_summary_grouped$`Disagree (%)` <- as.numeric(Q61c_summary_grouped$Disagree) + 
                                 as.numeric(Q61c_summary_grouped$`Strongly disagree`)
          #pivot table
         Q61c_summary_long <-
            as.data.frame(
               Q61c_summary_grouped %>% 
                  tidyr::pivot_longer(
                     cols = c(
                        "Disagree (%)",
                        "Neither agree nor disagree (%)",
                        "Agree (%)"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
               )
         Q61c_summary_long$key <-
            factor(
               Q61c_summary_long$key,
               levels = c(
                  "Disagree (%)",
                  "Neither agree nor disagree (%)",
                  "Agree (%)"
               )
            )
         Q61c <- clean_plot_data(Q61c_summary_long)
         #Plot
         plot_Q61c <-
            ggplot(Q61c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        labeller = label_wrap_gen(25)) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of fishers who believe that participation in management \nwill help to maintain or improve fish catch"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q61c, height = 750)
}