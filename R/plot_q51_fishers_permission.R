prep_q51_fishers_permission <- function(.data){

}

plot_q51_fishers_permission <- function(.data, use_plotly = TRUE){
hhs_Q51a <- selectedData()[,c("ma_name", "51a_fishers_gear_not_permitted")] %>%
                        filter(`51a_fishers_gear_not_permitted` %in% c(0:10)) %>% 
                           droplevels()
         ## Summary 
         Q51a_length <-
            tapply(hhs_Q51a$`51a_fishers_gear_not_permitted`,
                   hhs_Q51a$ma_name,
                   length)
         Q51a_length <- as.vector(Q51a_length)
         Q51a_mean <-
            as.data.frame(
               tapply(
                  hhs_Q51a$`51a_fishers_gear_not_permitted`,
                  hhs_Q51a$ma_name,
                  mean
               ) / 10
            )
         Q51a_summary_bind <-
            cbind(N = Q51a_length, round(Q51a_mean, 3) * 100)
         colnames(Q51a_summary_bind) <- c("N", "Unapproved gear")
         Q51a_summary <- rbind(Q51a_summary_bind,
                               "Mean ± SE" = c(
                                  sum(Q51a_summary_bind$N),
                                  mean_sem(Q51a_summary_bind[[2]], 1)
                               ))
         Q51a_summary <- rownames_to_column(Q51a_summary, "MA name")
         
         ### Q51b Frequency of observed fishing in reserve ####
         hhs_Q51b <- selectedData()[,c("ma_name", "51b_fishers_reserves")] %>%
                        filter(`51b_fishers_reserves` %in% c(0:10)) %>%
                           droplevels()
         Q51b_length <-
            tapply(hhs_Q51b$`51b_fishers_reserves`,
                   hhs_Q51b$ma_name,
                   length)
         Q51b_length <- as.vector(Q51b_length)
         Q51b_mean <-
            data.frame(avg = tapply(
               hhs_Q51b$`51b_fishers_reserves`,
               hhs_Q51b$ma_name,
               mean
            ) / 10)
         Q51b_summary_bind <-
            cbind(N = Q51b_length, round(Q51b_mean, 3) * 100)
         Q51b_summary <-
            rbind(Q51b_summary_bind, "Mean ± SE" = c(
               sum(Q51b_summary_bind$N),
               mean_sem(Q51b_summary_bind$avg, 1)
            ))
         colnames(Q51b_summary) <- c("N", "Fishing in reserve")
         Q51b_summary <- rownames_to_column(Q51b_summary, "MA name")
         
         ### Q51c Frequency of observed unpermitted fishing in MA ####
         hhs_Q51c <- selectedData()[,c("ma_name", "51c_fishers_ma_area")] %>%
                        filter(`51c_fishers_ma_area` %in% c(0:10)) %>%
                           droplevels()
          Q51c_length <-
            tapply(hhs_Q51c$`51c_fishers_ma_area`,
                   hhs_Q51c$ma_name,
                   length)
         Q51c_length <- as.vector(Q51c_length)
         Q51c_mean <-
            data.frame(avg = tapply(
               hhs_Q51c$`51c_fishers_ma_area`,
               hhs_Q51c$ma_name,
               mean
            ) / 10)
         Q51c_summary_bind <-
            cbind(N = Q51c_length, round(Q51c_mean, 3) * 100)
         colnames(Q51c_summary_bind) <-
            c("N", "Fishing without permission")
         Q51c_summary <-
            rbind(Q51c_summary_bind, "Mean ± SE" = c(
               sum(Q51c_summary_bind$N),
               mean_sem(Q51c_summary_bind[[2]], 1)
            ))
         Q51c_summary <- rownames_to_column(Q51c_summary, "MA name")
         
         ### Combine Q51a, Q51b and Q51c ####
         Q51a_c_summary <-
            plyr::join_all(
               list(Q51a_summary, Q51b_summary, Q51c_summary),
               by = "MA name",
               type = "left"
            )
         
         #pivot table
         Q51a_c_summary_long <-
            as.data.frame(
               Q51a_c_summary %>% pivot_longer(
                  cols = c(
                     "Unapproved gear",
                     "Fishing in reserve",
                     "Fishing without permission"
                  ),
                  names_to = "key",
                  values_to = "Proportion (%)"
               )
            )
         #Fix Ns
         Q51a_c_summary_long$N <-
            as.data.frame(
               pivot_longer(
                  Q51a_c_summary,
                  cols = c("N", "N", "N"),
                  names_repair = "unique",
                  names_to = "No",
                  values_to = "N"
               )
            )$N
         
         Q51a_c <- data_4plot(Q51a_c_summary_long)
         
         #Plot
         plot_Q51a_c <-
            ggplot(Q51a_c, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key,
                        scale = input$x_axis,
                        labeller = label_wrap_gen(25)) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Perceived frequency of observing others violating regulations ") +
            xlab (NULL) + ylab ("Proportion (%)") + coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q51a_c, height = 750)
}