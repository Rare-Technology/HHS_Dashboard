
prep_q42d_problem_inside_reserve <- function(.data){

}

plot_q42d_problem_inside_reserve <- function(.data, use_plotly = TRUE){
hhs_Q42 <- .data[,c("maa", "42d_problem_inside_reserve")] %>%
                        dplyr::filter(`42d_problem_inside_reserve` %in% c(0,1)) %>%
                           rbind(c(NA,0), c(NA,1))
            
         Q42d_summary <- proportion(hhs_Q42[[2]],
                                    hhs_Q42[[1]],
                                    3,2)[,-3]
        
         colnames(Q42d_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q42d <- clean_plot_data(Q42d_summary)
         
         #Plot
         plot_Q42d <-
            ggplot(Q42d, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            #facet_wrap(~key, scale = input$x_axis, labeller = label_wrap_gen(20), ncol=5)+
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that \nknow how fishing inside the reserve affect the fishery"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42d, height = 750)
}