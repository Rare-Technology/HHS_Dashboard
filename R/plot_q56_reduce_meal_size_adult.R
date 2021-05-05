
prep_q56_reduce_meal_size_adult <- function(.data){

}

plot_q56_reduce_meal_size_adult <- function(.data, use_plotly = TRUE){
hhs_Q56 <- .data[c("maa", "56_reduce_meal_size_adult")] %>%
                        dplyr::filter (`56_reduce_meal_size_adult` != "") %>%
                           droplevels()
         
         Q56_summary <- proportion (hhs_Q56$`56_reduce_meal_size_adult`,
                                      hhs_Q56$maa,
                                      3,2)[,-3]
         
         colnames(Q56_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q56 <- clean_plot_data(Q56_summary)
         
         #Plot
         plot_Q56 <-
            ggplot(Q56, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of fisher households that had to reduce meal size \ndue to not having enough food in the last 12 months"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q56, height = 750)
}