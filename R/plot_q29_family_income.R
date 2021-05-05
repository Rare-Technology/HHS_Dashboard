
prep_q29_family_income <- function(.data){

}

plot_q29_family_income <- function(.data, use_plotly = TRUE){
hhs_Q29 <- .data[,c("maa", "29_family_income")] %>%
                        dplyr::filter(`29_family_income` %in% c("Sufficient",
                                                         "Insufficient",
                                                         "Tight")) %>%
                           rbind(c(NA, "Sufficient"), 
                                 c(NA, "Insufficient"),
                                 c(NA, "Tight")) %>%
                            droplevels()
         #proportion
         Q29_summary <-
            proportion(hhs_Q29[[2]], hhs_Q29[[1]], 3, 3)
         #rename column
         colnames(Q29_summary) <-
            c("MA name",
              "N",
              "Insufficient (%)",
              "Sufficient (%)",
              "Tight (%)")
        
          #plot set up
         Q29_longer <-
            Q29_summary %>% pivot_longer (
               cols = c("Insufficient (%)", "Sufficient (%)", "Tight (%)"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q29_longer$key <-
            factor(Q29_longer$key,
                   levels = c("Insufficient (%)", "Tight (%)", "Sufficient (%)"))
         
         Q29 <- clean_plot_data(Q29_longer)

         #Plot
         plot_Q29 <-
            ggplot(Q29, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") + 
            facet_wrap( ~ key) +
            ggtitle(
               "Proportion of community members that have \nsufficient income to cover their family's needs"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") +
            coord_flip(clip = "on")
         
         ggplotly(plot_Q29, height = 750)
}