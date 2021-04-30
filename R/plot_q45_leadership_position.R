
prep_q45_leadership_position <- function(.data){

}

plot_q45_leadership_position <- function(.data, use_plotly = TRUE){
hhs_Q45 <- selectedData_Q45()[,c("ma_name", "45_leadership_position")] %>%
            filter(`45_leadership_position`!= "") %>%
               filter(`45_leadership_position`!= "na") %>%
                  droplevels()
          
         #hhs_Q45$`45_leadership_position`[hhs_Q45$`45_leadership_position` == "No management"] <- "No"
         #hhs_Q45$`45_leadership_position`[hhs_Q45$`45_leadership_position` == ""] <- "No"
         #hhs_Q45$`45_leadership_position`[hhs_Q45$`45_leadership_position` == "na"] <- "No"
         
         Q45_summary <- proportion(hhs_Q45[[2]],
                                    hhs_Q45[[1]],
                                    3,
                                    length(unique(hhs_Q45[[2]])))
         
         #colnames(Q45_summary) <- c("MA name", "N", "No",  "Not sure", "Yes female", "Yes male")
         #rownames to columns
         Q45_summary_long <-
            Q45_summary %>% pivot_longer(
               cols = names(Q45_summary[-c(1:2)]),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q45_summary_long$key <- str_replace (Q45_summary_long$key, "[.]", " ")
         Q45 <- data_4plot(Q45_summary_long)
         
         #Plot
         plot_Q45 <-
            ggplot(Q45, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, ncol=5) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members that \nhold leadership positions in the management body"
            ) +
            xlab (NULL) + ylab (NULL) + 
               coord_flip(clip ="on")
         
         ggplotly(plot_Q45, height = 750)
}