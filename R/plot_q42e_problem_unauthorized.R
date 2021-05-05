
prep_q42e_problem_unauthorized <- function(.data){

}

plot_q42e_problem_unauthorized <- function(.data, use_plotly = TRUE){
hhs_Q42 <- .data[,c("maa","42e_problem_unauthorized")] %>%
                        dplyr::filter(`42e_problem_unauthorized` %in% c(0,1))
             
         Q42e_summary <- proportion(hhs_Q42[[2]],
                                    hhs_Q42[[1]],
                                    3,2)[,-3]
         
         colnames(Q42e_summary) <- c("MA name", "N", "Proportion (%)")

         Q42e <- clean_plot_data (Q42e_summary)
         
         #Plot
         plot_Q42e <-
            ggplot(Q42e, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that know how \nunauthorized fishers fishing inside the managed access area affect the fishery?"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q42e, height = 750)
}