
prep_q30_trust_local_decision <- function(.data){

}

plot_q30_trust_local_decision <- function(.data, use_plotly = TRUE){
hhs_Q30a <- .data[,c("maa", "30_trust_local_decision")] %>%
            dplyr::filter(`30_trust_local_decision` %in% c(1:5)) %>%
               rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                  droplevels()
            
         #propoortion
         Q30a_summary <-
            proportion(hhs_Q30a$`30_trust_local_decision`,
                       hhs_Q30a$maa,
                       3, type=5)
         
         colnames(Q30a_summary) <- c("MA name", 
                                     "N", 
                                     "Strongly disagree", 
                                     "Disagree",
                                     "Neither agree nor disagree",
                                     "Agree",
                                     "Strongly agree")
         
         Q30a <- Q30a_summary %>%
                     dplyr::filter(`MA name` != "Mean ± SE")
         #combine answer
         Q30a$`Proportion (%)` <- as.numeric(Q30a$Agree) + as.numeric(Q30a$`Strongly agree`)
         
         Q30a <- clean_plot_data(Q30a)
         #Plot
         plot_Q30a <-
            ggplot(Q30a, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who trust in the local goverment \nto make decisions that benefit the community over their own interests"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
        
         ggplotly(plot_Q30a, height = 750)
}