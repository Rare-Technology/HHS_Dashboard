
prep_q54_food_availability <- function(.data){

}

plot_q54_food_availability <- function(.data, use_plotly = TRUE){
hhs_Q54 <- .data[,c("maa", "54_food_availability")] %>%
                        dplyr::filter(`54_food_availability` != "") %>%
                           droplevels()
         
         Q54_summary_bind <- proportion (hhs_Q54$`54_food_availability` ,
                                     hhs_Q54$maa,
                                     3,5)
         Q54_summary_bind <-
            Q54_summary_bind[-dim(Q54_summary_bind)[1], ]
         Q54_summary_bind$Good_ <-
            as.numeric(Q54_summary_bind$Good) + as.numeric(Q54_summary_bind$Very.good)
         Q54_summary_bind$Bad <-
            as.numeric(Q54_summary_bind$Rather.bad) + as.numeric(Q54_summary_bind$Very.bad)
         Q54_summary_bind$OK <- as.numeric(Q54_summary_bind$OK)
         Q54_summary <-
            Q54_summary_bind[, c("MA name", "N", "Bad", "OK", "Good_")]
         colnames(Q54_summary) <-
            c("MA name", "N", "Bad", "Normal", "Good")
         #pivot
         Q54_summary_long <-
            Q54_summary %>% pivot_longer(
               cols = c(`Good`, `Bad`, `Normal`),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q54_summary_long$key <-
            factor(Q54_summary_long$key,
                   levels = c("Bad", "Normal", "Good"))
         
         Q54 <- clean_plot_data(Q54_summary_long)
         #Plot
         plot_Q54 <-
            ggplot(Q54, aes(x = `MA name`, y = `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, scale = input$x_axis, ncol = 3) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of households who think that \nfood availability was good in the past year"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q54, height = 750)
}