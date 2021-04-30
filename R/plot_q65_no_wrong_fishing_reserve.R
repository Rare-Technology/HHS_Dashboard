
prep_q65_no_wrong_fishing_reserve <- function(.data){

}

plot_q65_no_wrong_fishing_reserve <- function(.data, use_plotly = TRUE){
hhs_Q65 <- selectedData()[,c("ma_name", "65_no_wrong_fishing_reserve")] %>%
                        dplyr::filter(`65_no_wrong_fishing_reserve` %in% c(0:10))
          
         Q65_length <-
            tapply(hhs_Q65$`65_no_wrong_fishing_reserve`,
                   hhs_Q65$ma_name,
                   length)
         Q65_length <- as.vector(Q65_length)
         Q65_mean <-
            data.frame(avg = tapply(as.numeric(
               as.character(hhs_Q65$`65_no_wrong_fishing_reserve`)
            ),
            hhs_Q65$ma_name, mean) / 10)
         Q65_summary_bind <- cbind(N = Q65_length, round(Q65_mean, 3) * 100)
         Q65_summary <-
            rbind(Q65_summary_bind, "Mean ± SE" = c(
               sum(Q65_summary_bind$N),
               mean_sem(Q65_summary_bind$avg, 1)
            ))
         colnames(Q65_summary) <- c("N", "Proportion (%)")
         Q65_summary <- rownames_to_column(Q65_summary, "MA name")
         
         Q65 <- data_4plot(Q65_summary)
         
         #plot
         plot_Q65 <-
            ggplot(Q65, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who think \nis wrong to fish in the reserve") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q65, height = 750)
}