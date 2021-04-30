
prep_q33_ma_familiar <- function(.data){

}

plot_q33_ma_familiar <- function(.data, use_plotly = TRUE){
hhs_Q33 <- selectedData()[,c("ma_name", "33_ma_familiar")] %>%
            filter(`33_ma_familiar` %in% c(0,1)) %>%
               rbind(c(NA,0), c(NA,1)) %>%
                  droplevels()
         
          #proportion
         Q33_summary <-
            proportion(hhs_Q33[[2]], 
                       hhs_Q33[[1]], 
                       3, type = 2)[, -3]
         #rename
         colnames(Q33_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q33 <- data_4plot (Q33_summary)
         
         #Plot
         plot_Q33 <-
            ggplot(Q33, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community familiar with the \nmanaged access and reserve area management approach"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip = "on")
           
         ggplotly(plot_Q33, height = 750)
}