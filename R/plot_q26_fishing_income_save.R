
prep_q26_fishing_income_save <- function(.data){

}

plot_q26_fishing_income_save <- function(.data, use_plotly = TRUE){
hhs_Q26 <- .data[,c("maa", "26_fishing_income_save")] %>%
            dplyr::filter(`26_fishing_income_save` != "") 
         
         #Convert to 0 and 1
         hhs_Q26$X26_fishing_income_save <-
            ifelse(hhs_Q26$`26_fishing_income_save` > 0, 1, 0)
         
         hhs_Q26 <- rbind(hhs_Q26, c(NA,0), c(NA,1))
         
         #proportion
         Q26_summary <-
            proportion(hhs_Q26[[2]],
                       hhs_Q26[[1]],
                       3,2)[, -3]
         #rename
         colnames(Q26_summary) <- c("MA name", "N", "Proportion (%)")
         #summary
         Q26 <- clean_plot_data(Q26_summary)
          
         #Plot
         plot_Q26 <-
            ggplot(Q26, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of househols with enough income to save") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q26, height = 750)
}