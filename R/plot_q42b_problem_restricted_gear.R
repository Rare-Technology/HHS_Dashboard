
prep_q42b_problem_restricted_gear <- function(.data){

}

plot_q42b_problem_restricted_gear <- function(.data, use_plotly = TRUE){

         hhs_Q42 <- selectedData()[,c("ma_name", "42b_problem_restricted_gear")] %>%
                        filter(`42b_problem_restricted_gear` %in% c(0,1)) %>%
                           rbind(c(NA,0), c(NA,1))
            
         Q42b_summary <- proportion( hhs_Q42[[2]],
                                     hhs_Q42[[1]],
                                     3,2)[,-3]
                                     
         colnames(Q42b_summary) <-  c("MA name", "N", "Proportion (%)")
         
         Q42b <- data_4plot (Q42b_summary)
         
            #Plot
         plot_Q42 <-
            ggplot(Q42b, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that \nknow how fishing with restricted gear affect the fishery"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42, height = 750)
}