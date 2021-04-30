
prep_q30_trust_community <- function(.data){

}

plot_q30_trust_community <- function(.data, use_plotly = TRUE){

hhs_Q30c <- selectedData()[,c("ma_name", "30_trust_community")] %>%
            filter(`30_trust_community` %in% c(1:5)) %>%
               rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                  droplevels()
         
         #propoortion
         Q30c_summary<-
            proportion(hhs_Q30c[[2]], 
                       hhs_Q30c[[1]], 
                       3, type = 5)
         colnames(Q30c_summary) <- c("MA name", 
                                     "N", 
                                     "Strongly disagree", 
                                     "Disagree",
                                     "Neither agree nor disagree",
                                     "Agree",
                                     "Strongly agree")
         
         Q30c <- Q30c_summary %>%
            filter(`MA name` != "Mean ± SE")
         
         #sum agree and strongly agree
         Q30c$`Social Cohesion` <- as.numeric(Q30c$Agree) + as.numeric(Q30c$`Strongly agree`)
         Q30c_summary <- Q30c[, c("MA name", "N", "Social Cohesion")]
         
         #rename columns for shiny app
         colnames(Q30c_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q30c <- data_4plot(Q30c_summary)
         
         #Plot
         plot_Q30c <-
            ggplot(Q30c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who trust in \ntheir fellow community members"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         ggplotly(plot_Q30c, height = 750)
         }