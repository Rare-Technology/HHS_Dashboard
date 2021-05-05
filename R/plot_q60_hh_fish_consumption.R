
prep_q60_hh_fish_consumption <- function(.data){

}

plot_q60_hh_fish_consumption <- function(.data, ...){
hhs_Q60 <- .data[,c("maa", "60_hh_fish_consumption")] %>%
                        dplyr::filter(`60_hh_fish_consumption` != "") %>%
                           droplevels()
         
          Q60_summary <- proportion(hhs_Q60$`60_hh_fish_consumption`,
                                    hhs_Q60$maa,
                                    rounding = 3, type = 5)
          
          colnames(Q60_summary) <- c("MA name", "N", "Few",
                                     "Few per month", "Few per week",
                                     "More than few per week", "Rarely")
          
          Q60_summary_long <-
             Q60_summary %>% tidyr::pivot_longer(
                cols = c("Few",
                         "Few per month", 
                         "Few per week",
                         "More than few per week", 
                         "Rarely"),
                names_to = "key",
                values_to = "Proportion (%)"
             )
          
          Q60_summary_long$key <-
             factor(Q60_summary_long$key,
                    levels = c("Rarely", 
                               "Few",
                               "Few per month", 
                               "Few per week",
                               "More than few per week")
             )
          
          Q60 <- clean_plot_data(Q60_summary_long)
          
         #plot
         plot_Q60 <-
            ggplot(Q60, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap (~key, 
                        ncol=5, 
                        labeller = label_wrap_gen(15))+
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 50)) +
            ggtitle("Proportion of households that consume fish \nmore than few times per week") +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip = "on")
         
         ggplotly(plot_Q60, height = 750)
}