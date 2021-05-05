
prep_q38_reserve_fishing_allowed <- function(.data){

}

plot_q38_reserve_fishing_allowed <- function(.data, use_plotly = TRUE){
hhs_Q38 <- .data[,c("maa", "38_reserve_fishing_allowed")] %>%
                     dplyr::filter(`38_reserve_fishing_allowed` != "" &
                           `38_reserve_fishing_allowed` != -1) %>%
                        rbind(c(NA,0), c(NA,1)) %>%
                           droplevels()
         #proportion
         Q38_summary <-
            proportion(hhs_Q38[[2]],
                       hhs_Q38[[1]],
                       3,2)[,-4]
         #rename
         colnames(Q38_summary) <-
            c("MA name", "N", "Proportion (%)")
         #data for plot
         Q38 <- clean_plot_data (Q38_summary)
        
         #Plot
         plot_Q38 <-
            ggplot(Q38, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 125),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of community members who are aware \nthat fishing is not allowed in the reserve area") +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(clip = "on")
         
         ggplotly(plot_Q38, height = 700)
}