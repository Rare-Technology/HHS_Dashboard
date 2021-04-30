
prep_q16_hh_main_fisher <- function(.data){

}

plot_q16_hh_main_fisher <- function(.data, use_plotly = TRUE){

         hhs_Q16 <- selectedData()[,c("ma_name", "16_hh_main_fisher")] %>%
            filter(`16_hh_main_fisher` != "") %>%
               droplevels()
         
         Q16_summary <- proportion(hhs_Q16[[2]],
                                   hhs_Q16[[1]],
                                   3, type = length(unique(hhs_Q16[[2]])))
         Q16_summary_long <-
            Q16_summary %>% pivot_longer(
               cols = c(3:ncol(Q16_summary)),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q16 <- data_4plot(Q16_summary_long)
         Q16$key <- str_replace_all(Q16$key, "[.]", " ")
         
         plot_Q16 <-
            ggplot(Q16, aes(`MA name`, `Proportion (%)`, N = N)) +
            geom_bar(aes(fill = key),
                     position = position_stack(reverse = TRUE),
                     stat = "identity",
                     alpha = 0.8 ) +
            theme_rare + 
            scale_fill_brewer(palette = "Blues", direction = -1) +
            ggtitle("Proportion of main fishers in the household \nthat fish as member of a group"
            ) +
            scale_y_continuous(limits = c(0, 105),
                               breaks = seq(0, 100, 20)) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")+
            theme(legend.position = "right") +
            guides(fill = guide_legend(reverse = TRUE)) 
         
         ggplotly(plot_Q16, height = 750)
}