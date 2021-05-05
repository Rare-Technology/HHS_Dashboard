
prep_q68_fish_sold <- function(.data){

}

plot_q68_fish_sold <- function(.data, use_plotly = TRUE){
hhs_Q68 <- .data[,c("maa", "68_fish_eaten", "68_fish_sold")] %>%
                        dplyr::filter(!is.na(`68_fish_sold`)) %>%
                              dplyr::filter(`68_fish_eaten` < 800000)
         Q68_fish_eaten <-
            tapply(hhs_Q68$`68_fish_eaten`, hhs_Q68$maa, mean, na.rm = TRUE)
         Q68_fish_sold <-
            tapply(hhs_Q68$`68_fish_sold`, hhs_Q68$maa, mean, na.rm = TRUE)
         Q68_summary_bind <-
            as.data.frame(cbind(as.vector(summary(
               hhs_Q68$maa
            )),
            round(
               Q68_fish_sold / (Q68_fish_eaten + Q68_fish_sold), 3
            ) * 100))
         colnames(Q68_summary_bind) <- c("N", "Fish for subsistence")
         Q68_summary <-
            rbind(Q68_summary_bind, "Mean ± SE" = c(
               sum(Q68_summary_bind$N),
               compute_summary_line(Q68_summary_bind$`Fish for subsistence`, 1)
            ))
         
         Q68_summary <-tibble::rownames_to_column(Q68_summary_bind, "MA name")
         colnames(Q68_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q68 <- clean_plot_data(Q68_summary)
         
         #Plot
         plot_Q68 <-
            ggplot(Q68, aes(x = `MA name`, y = `Proportion (%)`, N = N)) +
            theme_rare + 
            geom_col(fill = "#005BBB", alpha = 0.8) +
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of catch sold versus eaten in the household") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip ="on")
         
         ggplotly(plot_Q68, height = 750)
}