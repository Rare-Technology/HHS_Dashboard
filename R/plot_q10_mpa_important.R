
prep_q10_mpa_important <- function(.data){

}

plot_q10_mpa_important <- function(.data, use_plotly = TRUE){

         hhs_Q10 <- selectedData()[ ,c("ma_name", "10_mpa_important")] %>%
            filter(`10_mpa_important` != "") %>%
            rbind(c(NA,1), c(NA,0), c(NA,-1))
         #proportion
         Q10_summary <-
            proportion(hhs_Q10$`10_mpa_important`, 
                       hhs_Q10$ma_name, 
                       3, 
                       type= 3)
         
         colnames(Q10_summary) <- c("MA name", "N", "Neutral", "No", "Yes")
         
         Q10_summary_long <-
            Q10_summary %>% pivot_longer(
               cols = c("Neutral", "No", "Yes"),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         
         Q10 <- data_4plot (Q10_summary_long)
         
         #plot 
         plot_Q10 <-
            ggplot(Q10, aes(`MA name`, `Proportion (%)`, N = N)) +
            facet_wrap (~ key) + 
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +  
            ggtitle(
               "Proportion of households that believe is important \nthat the region is managed and protected",
               subtitle = "A") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
      ggplotly(plot_Q10, height = 700)
}