
prep_q42c_problem_undersize <- function(.data){

}

plot_q42c_problem_undersize <- function(.data, use_plotly = TRUE){
hhs_Q42 <- .data[,c("maa", "42c_problem_undersize")] %>%
                           dplyr::filter (`42c_problem_undersize` %in% c(0,1)) %>%
                              rbind(c(NA,0), c(NA,1))
                                 
         Q42c_summary <- proportion(hhs_Q42[[2]],
                                    hhs_Q42[[1]],
                                    3,2)[,-3]
         
         colnames(Q42c_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q42c <- clean_plot_data(Q42c_summary)
         
         #Plot
         plot_Q42c <-
            ggplot(Q42c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that \nknow how fishing undersize fish affect the fishery"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42c, height = 750)
}