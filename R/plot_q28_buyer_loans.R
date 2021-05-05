
prep_q28_buyer_loans <- function(.data){

}

plot_q28_buyer_loans <- function(.data, use_plotly = TRUE){
hhs_Q28 <- .data[,c("maa", "28_buyer_loans")] %>%
            dplyr::filter(`28_buyer_loans` != "") %>%
               rbind(c(NA,0), c(NA,1))
         #recode to 0 and 1
         hhs_Q28$`28_buyer_loans` <-
            ifelse(hhs_Q28$`28_buyer_loans` > 0, 1, 0)
         #proportion
         Q28_summary <-
            proportion(hhs_Q28[[2]],
                       hhs_Q28[[1]], 
                       3, 2)[, -3] #drop the No
         #rename
         colnames(Q28_summary) <-
            c("MA name", "N", "Proportion (%)") #keep the yes
         #summary
         Q28 <- clean_plot_data(Q28_summary)
         
         #Plot
         plot_Q28 <-
            ggplot(Q28, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(alpha = 0.8, fill = "#005BBB") +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of household that \ntake out loans from fish buyers or traders") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q28, height = 750)
}