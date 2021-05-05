
prep_q46_represent_interests <- function(.data){

}

plot_q46_represent_interests <- function(.data, ...){
hhs_Q46 <- .data[,c("maa","46_represent_interests")] %>%
                        dplyr::filter (`46_represent_interests` != "") %>%
                           dplyr::filter (`46_represent_interests` != "na") %>%
                                droplevels()
          #recode no managemnt answers 
         #hhs_Q46$`46_represent_interests`[hhs_Q46$`46_represent_interests` == ""] <- "Disagree"
         #hhs_Q46$`46_represent_interests` <- recode_factor(hhs_Q46$`46_represent_interests`,
         #                                                   "No management" = "Disagree",
         #                                                   "na" = "Disagree")
      
         Q46_summary <- proportion (hhs_Q46[[2]],
                                    hhs_Q46[[1]],
                                    3, 
                                    type = length(unique(hhs_Q46[[2]])))
         #pivot table
         Q46_summary_long <-
            Q46_summary %>% tidyr::pivot_longer(
               cols = names(Q46_summary)[-c(1:2)],
               names_to = "key",
               values_to = "Proportion (%)"
            )
        
          #Q46_summary_long$key <- factor(Q46_summary_long$key,
          #         levels = c("Disagree", "Neither", "Agree"))
         Q46_summary_long$key <- str_replace (Q46_summary_long$key, "[.]", " ")
         Q46 <- clean_plot_data(Q46_summary_long)
          
         #Plot
         plot_Q46 <-
            ggplot(Q46, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, ncol = 4) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members who agree that the \nfisheries management body represents their interests to the fishery"
            ) +
            xlab (NULL) + ylab (NULL) + 
               coord_flip(clip ="on")
         
         ggplotly(plot_Q46, height = 750)
}