
prep_q34_gear_restrictions <- function(.data){

}

plot_q34_gear_restrictions <- function(.data, use_plotly = TRUE){
hhs_Q34 <- selectedData()[,c("ma_name", "34_gear_restrictions")] %>%
                        filter(`34_gear_restrictions` %in% c(0,1,-1)) %>%
                           rbind(c(NA,0), c(NA,1)) %>%
                            droplevels()
         #No MA as 0 answer
         hhs_Q34$`34_gear_restrictions`[hhs_Q34$`34_gear_restrictions` == -1] <- 0
          
         Q34_summary <-
               proportion(hhs_Q34[[2]], 
                          hhs_Q34[[1]], 
                          3,2)[,-3]
         #rename
         colnames(Q34_summary) <-
            c("MA name", "N", "Proportion (%)")
         
         Q34 <- data_4plot (Q34_summary)
         
         #Plot
         plot_Q34 <-
            ggplot(Q34, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 125),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of community members who are aware \nof gear restrictions in the managed access area") +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(clip = "on")
         
         ggplotly(plot_Q34, height = 700)
}