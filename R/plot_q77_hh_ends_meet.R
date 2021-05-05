
prep_q77_hh_ends_meet <- function(.data){

}

plot_q77_hh_ends_meet <- function(.data, ...){
hhs_Q77 <- .data[,c("maa", "77_hh_ends_meet")] %>%
                        dplyr::filter(`77_hh_ends_meet` != "") %>%
                           droplevels()
         
      Q77_summary <- proportion(hhs_Q77$`77_hh_ends_meet`,
                                   hhs_Q77$maa,
                                   3, type=5)
      
      colnames(Q77_summary) <- c("MA name", "N", 
                                 "Easy", "Fairly easy", 
                                 "Very easy", "With difficulty",
                                 "With great difficulty")
       Q77_summary_long <-
            as.data.frame(
               Q77_summary %>% 
                  tidyr::pivot_longer(
                     cols = c(
                        "With great difficulty",
                        "With difficulty",
                        "Fairly easy",
                        "Easy",
                        "Very easy"
                       ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
            )
         
         Q77_summary_long$key <-
            factor(
               Q77_summary_long$key,
               levels = c(
                  "With great difficulty",
                  "With difficulty",
                  "Fairly easy",
                  "Easy",
                  "Very easy"
                )
            )
         
         Q77 <- clean_plot_data(Q77_summary_long)
         
          
        #Plot
        plot_Q77 <-
           ggplot(Q77, aes(`MA name`, `Proportion (%)`, N = N)) +
           theme_rare + 
           geom_col(fill = "#005BBB", alpha = 0.8) +
           facet_wrap( ~ key, ncol=5,
                       labeller = label_wrap_gen(25)) +
           scale_y_continuous(limits = c(0, 110),
                              breaks = seq(0, 100, 25)) +
           ggtitle("Proportion of household able to make ends meet") +
           xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip = "on")
        
         ggplotly(plot_Q77, height = 750)
}