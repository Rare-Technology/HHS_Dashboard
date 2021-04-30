
prep_q41_ma_fishers_allowed <- function(.data){

}

plot_q41_ma_fishers_allowed <- function(.data, use_plotly = TRUE){
hhs_Q41 <- selectedData()[,c("ma_name", "41_ma_fishers_allowed")] %>%
                        filter(`41_ma_fishers_allowed` %in% 
                                  c("Community only",
                                    "Don't know",
                                    "No managed access",
                                    "No restrictions",
                                    "With authorization",
                                    "Without authorization")) %>%
                              rbind(c(NA, "Community only"),
                                    c(NA, "Don't know"),
                                    c(NA, "No managed access"),
                                    c(NA, "No restrictions"),
                                    c(NA, "With authorization"),
                                    c(NA, "Without authorization")) %>%
                              droplevels()
                        
         Q41_summary <- proportion (hhs_Q41[[2]],
                                    hhs_Q41[[1]],
                                    3,6)
         
         colnames(Q41_summary) <- c("MA name", "N", 
                                    "Community only",
                                    "Don't know",
                                    "No managed access",
                                    "No restrictions",
                                    "With authorization",
                                    "Without authorization")
         #pivot table
         Q41_summary_long <-
            Q41_summary %>% pivot_longer(
               cols = c(
                  "Community only",
                  "Don't know",
                  "No managed access",
                  "No restrictions",
                  "With authorization",
                  "Without authorization"
               ),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q41_summary_long$key <-
            factor(
               Q41_summary_long$key,
               levels = c(
                  "Community only",
                  "With authorization",
                  "Without authorization",
                  "No restrictions",
                  "Don't know",
                  "No managed access"
                  )
            )
         
         Q41 <- data_4plot(Q41_summary_long)
         
         #Plot
         plot_Q41 <-
            ggplot(Q41, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            theme(strip.text.x = element_text(margin = margin(0.25,0,0.25,0, "cm")))+
            facet_wrap(
               ~ key,
               labeller = label_wrap_gen(10),
               ncol = 6 ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 30)) +
            ggtitle(
               "Proportion of households that are aware of who is allowed \nto fish in the fisheries management/managed access area?") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(ylim = c(0, 119))
         
         ggplotly(plot_Q41, height = 750)
}