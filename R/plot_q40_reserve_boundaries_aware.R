
prep_q40_reserve_boundaries_aware <- function(.data){

}

plot_q40_reserve_boundaries_aware <- function(.data, use_plotly = TRUE){

         if (input$Country == "MOZ") {
            
            hhs_Q40_moz <- .data[,c("maa", "40_reserve_boundaries_aware")] %>%
                              dplyr::filter (`40_reserve_boundaries_aware` != "" &
                                      `40_reserve_boundaries_aware` != "No reserve") %>%
                                          droplevels()
          Q40_summary_moz <-
            proportion(
               question = hhs_Q40_moz[[2]],
               grouping = hhs_Q40_moz[[1]],
               rounding = 3, 
               type = 5
            )
         colnames(Q40_summary_moz) <-
            c(
               "MA name",
               "N",
               "Agree",
               "Disagree",
               "Neither",
               "Strongly agree",
               "Strongly disagree"
            )
         
         #pivot table
         Q40_summary_moz_long <-
            Q40_summary_moz %>% pivot_longer(
               cols = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q40_summary_moz_long$key <-
            factor(
               Q40_summary_moz_long$key,
               levels = c(
                  "Strongly disagree",
                  "Disagree",
                  "Neither",
                  "Agree",
                  "Strongly agree"
               )
            )
         
         Q40_moz <- clean_plot_data(Q40_summary_moz_long) 
         
         #Plot
         plot_Q40_moz <-
            ggplot(Q40_moz, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, #scale = input$x_axis,
                        labeller = label_wrap_gen(20), ncol = 5) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle(
               "Proportion of community members that think that \nmost fishers are aware of the boundaries of the reserve area"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q40_moz, height = 750)
         
         }
      
      
      ### all countries except Moz
         else if (input$Country != "MOZ") {
            
         hhs_Q40 <- .data[,c("maa", "40_reserve_boundaries_aware")] %>% 
                        dplyr::filter(`40_reserve_boundaries_aware` %in% c(0:10)) %>%
                           droplevels()
         
         Q40_length <-
            tapply(hhs_Q40[[2]],
                   hhs_Q40[[1]],
                   length)
         Q40_length <- as.vector(Q40_length)
         Q40_mean <-
            data.frame(avg = tapply(as.numeric(
               as.character(hhs_Q40$`40_reserve_boundaries_aware`)
            ),
            hhs_Q40$maa, mean) / 10)
         Q40_summary_bind <-
            cbind(N = Q40_length, round(Q40_mean, 3) * 100)
         Q40_summary <-
            rbind(Q40_summary_bind, "Mean ± SE" = c(
               sum(Q40_summary_bind$N),
               compute_summary_line(Q40_summary_bind$avg, 1)
            ))
         Q40_summary <-tibble::rownames_to_column(Q40_summary, "MA name")
         colnames(Q40_summary) <- c("MA name", "N", "Proportion (%)")
         
         Q40 <- clean_plot_data (Q40_summary)
         
         #Plot
         plot_Q40 <-
            ggplot(Q40, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle(
               "Proportion of community members that think that most fishers \nare aware of the boundaries of the reserve area"
            ) +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q40, height = 750)
         
         }
}