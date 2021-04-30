prep_q51_fishers_caught <- function(.data){

}

prep_q51_fishers_caught <- function(.data, use_plotly = TRUE){
hhs_Q51d <- selectedData()[,c("ma_name", "51d_fishers_violate_fish_size")] %>%
                        filter(`51d_fishers_violate_fish_size` %in% c(0:10)) %>%
                           droplevels()
         Q51d_length <-
            tapply(hhs_Q51d$`51d_fishers_violate_fish_size`,
                   hhs_Q51d$ma_name,
                   length)
         Q51d_length <- as.vector(Q51d_length)
         Q51d_mean <-
            data.frame(
               freq = tapply(
                  hhs_Q51d$`51d_fishers_violate_fish_size`,
                  hhs_Q51d$ma_name,
                  mean
               ) / 10
            )
         ### Proportions
         Q51d_summary_bind <-
            cbind(N = Q51d_length, round(Q51d_mean, 3) * 100)
         Q51d_summary <-
            rbind(Q51d_summary_bind, "Mean ± SE" = c(
               sum(Q51d_summary_bind$N, na.rm = TRUE),
               mean_sem(Q51d_summary_bind$freq, 1)
            ))
         colnames(Q51d_summary) <- c("N", "Fish Size Violations (%)")
         Q51d_summary <- rownames_to_column(Q51d_summary, "MA name")
         
         hhs_Q51e <- selectedData()[,c("ma_name", "51e_fishers_caught")] %>%
                        filter (`51e_fishers_caught` != "") %>%
                           droplevels()
            
         Q51e_length <-
            tapply(hhs_Q51e$`51e_fishers_caught`,
                   hhs_Q51e$ma_name,
                   length)
         Q51e_length <- as.vector(Q51e_length)
         Q51e_mean <-
            data.frame(freq = tapply(
               hhs_Q51e$`51e_fishers_caught`,
               hhs_Q51e$ma_name,
               mean
            ) / 10)
         Q51e_summary_bind <-
            cbind(N = Q51e_length, round(Q51e_mean, 3) * 100)
         Q51e_summary <-
            rbind(Q51e_summary_bind, "Mean ± SE" = c(
               sum(Q51e_summary_bind$N, na.rm = TRUE),
               mean_sem(Q51e_summary_bind$freq, 1)
            ))
         ##rownames to column
         Q51e_summary <- rownames_to_column(Q51e_summary, "MA name")
         colnames(Q51e_summary) <-
            c("MA name", "N", "Seasonal Closures Violations (%)")
         
         ### Combine Q51d and Q51e
         Q51d_51e_summary <-
            plyr::join_all(list(Q51d_summary, Q51e_summary),
                           by = "MA name",
                           type = "right")
         #pivot table
         Q51d_e_summary_long <-
            as.data.frame(
               Q51d_51e_summary %>% pivot_longer(
                  cols = c(
                     "Fish Size Violations (%)",
                     "Seasonal Closures Violations (%)"
                  ),
                  names_to = "key",
                  values_to = "Proportion (%)"
               )
            )
         #Fix Ns
         Q51d_e_summary_long$N <-
            as.data.frame(
               pivot_longer(
                  Q51d_e_summary_long,
                  cols = c("N", "N"),
                  names_repair = "unique",
                  names_to = "No",
                  values_to = "N"
               )
            )$N
         
         Q51d_e <- data_4plot(Q51d_e_summary_long)
         
         #Plot
         plot_Q51d_e <-
            ggplot(Q51d_e, aes(`MA name`,`Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(20)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Perceived frequency of getting caught for violating regulations") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q51d_e, height = 750)
}