
prep_q66_response_fishing_reserve <- function(.data){

}

plot_q66_response_fishing_reserve <- function(.data, ...){
hhs_Q66 <- .data[,c("maa", "66_response_fishing_reserve")] %>%
            dplyr::filter(`66_response_fishing_reserve` %in% c(0,1)) %>%
               droplevels()
         
         Q66_summary <- proportion(hhs_Q66$`66_response_fishing_reserve`,
                                    hhs_Q66$maa,
                                    3,
                                   length(unique(hhs_Q66$`66_response_fishing_reserve`))
                                   )[,-3]
         
        Q66_summary[Q66_summary == "NaN"] <- 0
        Q66_summary[Q66_summary == ""] <- 0
         
         colnames(Q66_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q66 <- clean_plot_data(Q66_summary)
         
         #plot
         plot_Q66 <-
            ggplot(Q66, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of households believing that if a fisher was fishing \nin the reserve, they would say or do anything in response") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q66, height = 750)
}