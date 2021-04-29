plot_q8 <- function(.data){
  hhs_Q8 <- .data[,c("maa", "8_religion")] %>%
    filter(`8_religion` != "") %>%
    droplevels()
  
  Q8_summary <- proportion(hhs_Q8[[2]],
                           hhs_Q8[[1]],
                           3, type = length(unique(hhs_Q8[[2]])))
  Q8_summary_long <-
    Q8_summary %>% pivot_longer(
      cols = c(3:ncol(Q8_summary)),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  Q8 <- data_4plot(Q8_summary_long)
  Q8$key <- stringr::str_replace_all(Q8$key, "[.]", " ")
  
  plot_Q8 <-
    ggplot(Q8, aes(`MA name`, `Proportion (%)`, N = N)) +
    geom_bar(aes(fill = key),
             position = position_stack(reverse = TRUE),
             stat = "identity",
             alpha = 0.8 ) +
    theme_rare + 
    scale_fill_brewer(palette = "Blues", direction = -1) +
    ggtitle("Religion of the head of the Household"
    ) +
    scale_y_continuous(limits = c(0, 105),
                       breaks = seq(0, 100, 20)) +
    xlab (NULL) + ylab ("Proportion (%)") + 
    coord_flip(clip = "on")+
    theme(legend.position = "right") +
    guides(fill = guide_legend(reverse = TRUE)) 
  
  ggplotly(plot_Q8, height = 750)
}
