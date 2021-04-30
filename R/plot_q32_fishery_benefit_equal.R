
prep_q32_fishery_benefit_equal <- function(.data){

}

plot_q32_fishery_benefit_equal <- function(.data, use_plotly = TRUE){
hhs_Q32 <- selectedData()[,c("ma_name", "32_fishery_benefit_equal")] %>%
            filter(`32_fishery_benefit_equal` %in% c(0,1)) %>%
               rbind(c(NA,0), c(NA,1)) %>%
                  droplevels()
         
         #proportion
         Q32_summary <-
            proportion(hhs_Q32$`32_fishery_benefit_equal`,
                       hhs_Q32$ma_name,
                       3, type = 2)[, -3]
         #summary for table
         colnames(Q32_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q32 <- data_4plot(Q32_summary)
         
         #Plot
         plot_Q32 <-
            ggplot(Q32, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community that believes \nthey benefit equally from fishery as other households"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q32, height = 750)
}