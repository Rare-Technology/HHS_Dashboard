
prep_q30_trust_village_alert <- function(.data){

}

plot_q30_trust_village_alert <- function(.data, use_plotly = TRUE){
hhs_Q30d <- .data[,c("maa", "30_trust_village_alert")] %>%
            dplyr::filter(`30_trust_village_alert` %in% c(1:5)) %>%
               rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
                  droplevels()
         #proportion
         Q30d_summary <-
            proportion(hhs_Q30d[[2]],
                       hhs_Q30d[[1]],
                       3, type= 5)
         
         colnames(Q30d_summary) <- c("MA name", 
                                     "N", 
                                     "Strongly disagree", 
                                     "Disagree",
                                     "Neither agree nor disagree",
                                     "Agree",
                                     "Strongly agree")
         Q30d <- Q30d_summary %>%
            dplyr::filter(`MA name` != "Mean ± SE")
         
         #sum agree and strongly agree
         Q30d$`Social Trust` <- as.numeric(Q30d$Agree) + as.numeric(Q30d$`Strongly agree`)
         Q30d_summary <- Q30d[, c("MA name", "N", "Social Trust")]
         
         #rename columns for shiny app
         colnames(Q30d_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q30d <- clean_plot_data(Q30d_summary)
         
         #Plot
         plot_Q30d <-
            ggplot(Q30d, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members who believe that \nyou have that to be alert to someone taking advantage of you"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q30d, height = 750)
}