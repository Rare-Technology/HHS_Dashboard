
prep_q36_fish_size_restriction <- function(.data){

}

plot_q36_fish_size_restriction <- function(.data, use_plotly = TRUE){
hhs_Q36 <- selectedData()[,c("ma_name", "36_fish_size_restriction")] %>%
                        filter(`36_fish_size_restriction` != "") %>%
                           rbind(c(NA,0), c(NA,1)) %>%
                              droplevels()
         #No MA as 0 answer
         hhs_Q36$`36_fish_size_restriction`[hhs_Q36$`36_fish_size_restriction` == -1] <- 0
         
         #proportion
         Q36_summary <-
            proportion(hhs_Q36[[2]],
                       hhs_Q36[[1]],
                       3,2)[,-3]
         #rename
         colnames(Q36_summary) <-
            c("MA name", "N", "Proportion (%)")
         
         Q36 <- data_4plot (Q36_summary)

         #Plot
         plot_Q36 <-
            ggplot(Q36, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 125),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of community members who are aware \nof fish size restrictions in the managed access area") +
            xlab (NULL) + 
            ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q36, height = 700)
}