
prep_q14_responsibilities <- function(.data){

}

plot_q14_responsibilities <- function(.data, use_plotly = TRUE){

         hhs_Q14 <- selectedData_Q14() [,c("submissionid", "ma_name", "14_responsibility")] 
   
         hhs_Q14$`14_responsibility` <- 
            recode_factor(hhs_Q14$`14_responsibility`,
                          "Prepare fishing gear" = 'Preparing gear for fishing',
                          "caring for house" = 'Keeping the house')
         
         Q14_summary <- proportion_Q14(hhs_Q14$submissionid, 
                                       hhs_Q14$`14_responsibility`,
                                       hhs_Q14$ma_name,
                                   rounding = 3, 
                                   type = length(unique(hhs_Q14$`14_responsibility`)))
          
         Q14_summary_long <-
            Q14_summary %>% pivot_longer(
               cols = c(3:ncol(Q14_summary)),
               names_to = "Activity",
               values_to = "Proportion (%)"
            )
         
         Q14 <- data_4plot(Q14_summary_long)
         Q14$Activity <- str_replace_all(Q14$Activity, "[.]", " ")
         
       
         plot_Q14 <-
            ggplot(Q14, aes(x =`MA name`, 
                            y = `Proportion (%)`, 
                            N = N,
                            fill = Activity)) +
            geom_bar(
               position = position_stack(reverse = TRUE),
               stat = "identity",
               alpha = 0.8 ) +
            theme_rare +
            scale_fill_brewer(palette = "Blues", 
                              direction = -1)+
            ggtitle(
               "Proportion of activities that are the responsibility \nof the women in the household in a typical week"
            ) +
            scale_y_continuous(limits = c(0, 105),
                               breaks = seq(0, 100, 25)) +
            xlab (NULL) + 
            ylab ("Proportion (%)") + 
            coord_flip(clip = "on") +
            theme(legend.position = "right",
                  legend.text = element_text(size = 10))+
            guides(fill = guide_legend(reverse = TRUE)) 
         ggplotly(plot_Q14, height = 750)
}