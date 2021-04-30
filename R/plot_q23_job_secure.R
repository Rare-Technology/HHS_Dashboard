
prep_q23_job_secure <- function(.data){

}

plot_q23_job_secure <- function(.data, use_plotly = TRUE){
hhs_Q23 <- hhs[,c("ma_name", "23_job_secure")] %>%
                     filter(`23_job_secure` %in% c(0,1)) %>%
                      rbind(c(NA,0), c(NA,1)) %>%
                        droplevels()
         #proportion
         Q23_summary <- proportion(hhs_Q23[[2]], 
                           hhs_Q23[[1]], 
                           rounding = 3, 
                           type = 2)#[, -3]
         #rename columns
         colnames(Q23_summary) <- c("MA name", "N", "Proportion (%)")
         ## add total and mean
         Q23 <- data_4plot(Q23_summary)
          #Plot
         plot_Q23 <-
            ggplot(Q23, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of fishers who are confident \nthat their jobs are secure") +
            xlab (NULL) + ylab ("Proportion (%)") + 
               coord_flip(clip ="on")
         
         ggplotly(plot_Q23, height = 750)
}