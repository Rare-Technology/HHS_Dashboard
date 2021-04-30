
prep_q9_region_member <- function(.data){

}

plot_q9_region_member <- function(.data, use_plotly = TRUE){

         hhs_Q9 <- selectedData()[,c("ma_name", "9_region_member")] %>%
            filter(`9_region_member` != "") %>%
            droplevels()
         
         Q9_summary <- proportion(hhs_Q9[[2]],
                                  hhs_Q9[[1]],
                                  3, type = length(unique(hhs_Q9[[2]])))
         colnames(Q9_summary) <- c("MA name", "N", "No", "Yes")
         
         Q9_summary_yes <- Q9_summary[c("MA name", "N", "Yes")]
         colnames(Q9_summary_yes) <- c("MA name", "N", "Proportion (%)")
         Q9 <- data_4plot(Q9_summary_yes)
         
         plot_Q9 <-
            ggplot(Q9, aes(`MA name`, `Proportion (%)`, N = N)) +
            geom_col(fill = "#005BBB", alpha = 0.8) +
            theme_rare + 
            ggtitle("Proportion of households that identify themselves \nas member of a specific region"
            ) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")+
            theme(legend.position = "right") 
            
         ggplotly(plot_Q9, height = 750)
}