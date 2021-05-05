
prep_q53_encourage_regulations <- function(.data){

}

plot_q53_encourage_regulations <- function(.data, use_plotly = TRUE){
hhs_Q53 <- .data[,c("maa", "53_encourage_regulations")] %>%
                        dplyr::filter(`53_encourage_regulations` != "" &
                               `53_encourage_regulations` != 'No regulations') %>%
                        droplevels()
         
         Q53_summary <- proportion(hhs_Q53$`53_encourage_regulations`,
                                    hhs_Q53$maa,
                                    3,5)
         colnames(Q53_summary) <-
            c(
               "MA name",
               "N",
               "Never",
               "Rarely",
               "Sometimes",
               "Often",
               "Very often"
            )
         
         #pivot table
         Q53_summary_long <-
            as.data.frame(
               Q53_summary %>% pivot_longer(
                  cols = c(
                     "Never",
                     "Rarely",
                     "Sometimes",
                     "Often",
                     "Very often"
                  ),
                  names_to = "key",
                  values_to = "Proportion (%)"
               )
            )
         Q53_summary_long$key <-
            factor(
               Q53_summary_long$key,
               levels = c(
                  "Never",
                  "Rarely",
                  "Sometimes",
                  "Often",
                  "Very often"
               )
            )
         
         Q53 <-  clean_plot_data(Q53_summary_long) 
         #Plot
         plot_Q53 <-
            ggplot(Q53, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap(
               ~ key,
               scale = input$x_axis,
               labeller = label_wrap_gen(20),
               ncol = 5
            ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 50)) +
            ggtitle(
               "Proportion of fishers that encourage others \nto participate in sustainable/responsible activity"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q53, height = 750)
}