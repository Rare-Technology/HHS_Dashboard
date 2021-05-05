
prep_q67_reserve_boundry <- function(.data){

}

plot_q67_reserve_boundry <- function(.data, use_plotly = TRUE){
hhs_Q67 <- .data[,c("maa", "67_reserve_boundry")] %>%
                           dplyr::filter(`67_reserve_boundry` != "")
          
          Q67_summary <- proportion( hhs_Q67$`67_reserve_boundry`,
                                     hhs_Q67$maa,
                                     3,2)[,-3]
          
          Q67_summary[Q67_summary == "NaN"] <- 0
          colnames(Q67_summary) <- c("MA name", "N", "Proportion (%)")
          
          Q67 <- clean_plot_data(Q67_summary)
         
                   #plot
         plot_Q67 <-
            ggplot(Q67, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who \nare aware of the reserve boundary") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q67, height = 750)
}