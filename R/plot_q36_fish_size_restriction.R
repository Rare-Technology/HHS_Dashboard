# 
# prep_q36_fish_size_restriction <- function(.data){
# 
# }

plot_q36_fish_size_restriction <- function(.data, ...){
  
  
  .data_plot <- .data %>%
    dplyr::filter(`36_fish_size_restriction` %in% c(0, 1, -1)) %>%
    dplyr::mutate(
      `36_fish_size_restriction` = ifelse(`36_fish_size_restriction` == -1, 0, `36_fish_size_restriction`)
    ) %>%
    prep_data_for_plot(
      `36_fish_size_restriction`,
      type = "bar",
      bar_column = `1`
    )
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who are aware \nof fish size restrictions in the managed access area"
  )
  
  
# hhs_Q36 <- .data[,c("maa", "36_fish_size_restriction")] %>%
#                         dplyr::filter(`36_fish_size_restriction` != "") %>%
#                            rbind(c(NA,0), c(NA,1)) %>%
#                               droplevels()
#          #No MA as 0 answer
#          hhs_Q36$`36_fish_size_restriction`[hhs_Q36$`36_fish_size_restriction` == -1] <- 0
#          
#          #proportion
#          Q36_summary <-
#             proportion(hhs_Q36[[2]],
#                        hhs_Q36[[1]],
#                        3,2)[,-3]
#          #rename
#          colnames(Q36_summary) <-
#             c("MA name", "N", "Proportion (%)")
#          
#          Q36 <- clean_plot_data (Q36_summary)
# 
#          #Plot
#          plot_Q36 <-
#             ggplot(Q36, aes(`MA name`, `Proportion (%)`, N = N)) +
#             theme_rare + 
#             geom_col(fill = "#005BBB", alpha = 0.8) +
#             scale_y_continuous(limits = c(0, 125),
#                                breaks = seq(0, 100, 25)) +
#             ggtitle("Proportion of community members who are aware \nof fish size restrictions in the managed access area") +
#             xlab (NULL) + 
#             ylab ("Proportion (%)") + 
#             coord_flip(clip = "on")
#          
#          ggplotly(plot_Q36, height = 700)
}