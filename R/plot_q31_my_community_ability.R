
prep_q31_my_community_ability <- function(.data){

}

plot_q31_my_community_ability <- function(.data, use_plotly = TRUE){
hhs_Q31 <- selectedData()[,c("ma_name", "31_my_community_ability")] %>%
            filter(`31_my_community_ability` != "") %>%
            filter(`31_my_community_ability` != "No dependance") %>%
               rbind(c(NA, "Agree"), 
                     c(NA, "Neither"), 
                     c(NA, "Strongly agree"),
                     c(NA, "Disagree"),
                     c(NA, "Strongly disagree")) %>%
               droplevels()
         #proportion
         Q31_summary <-
            proportion(hhs_Q31[[2]],
                       hhs_Q31[[1]],
                       3, type=5)
         
         Q31 <- Q31_summary %>%
            filter(`MA name` != "Mean ± SE")
         
         colnames(Q31) <- c("MA name", "N", 
                                    "Agree", 
                                    "Disagree",
                                    "Neither",
                                    "Strongly agree",
                                    "Strongly disagree")
         
         #combine agree answer
         Q31$`Collective Efficacy` <- as.numeric(Q31$Agree) + as.numeric(Q31$`Strongly agree`)
         
         #summary
         Q31_summary <- Q31[, c("MA name", "N", "Collective Efficacy")]
         
         colnames(Q31_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q31 <- data_4plot(Q31_summary)
            
         #Plot
         plot_Q31 <-
            ggplot(Q31, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who believe that the community \nhas the ability to manage the fishery effectively to maximize food and profits"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         ggplotly(plot_Q31, height = 750)
}