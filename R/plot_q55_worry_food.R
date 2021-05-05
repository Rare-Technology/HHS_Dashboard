
prep_q55_worry_food <- function(.data){

}

plot_q55_worry_food <- function(.data, ...){
hhs_Q55 <- .data[,c("maa", "55_worry_food")] %>%
                        dplyr::filter(`55_worry_food` != "") %>% 
                           droplevels()
         
         Q55_summary <- 
            proportion(hhs_Q55$`55_worry_food`,
                        hhs_Q55$maa,
                        3,3
            )
         
         Q55_summary_long <-
            Q55_summary %>% tidyr::pivot_longer(
               cols = c(`Never`, `Often`, `Sometimes`),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         
         Q55_summary_long$key <-
            factor(Q55_summary_long$key,
                   levels = c("Never", "Sometimes", "Often")
            )
         
         Q55 <- clean_plot_data(Q55_summary_long)
         
         #Plot
         plot_Q55 <-
            ggplot(Q55, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap( ~ key) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            #
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of fisher households who often worry about \nnot having enough food for everyone in the household"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
                     coord_flip(clip = "on")
         
         ggplotly(plot_Q55, height = 750)
}