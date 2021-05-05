
prep_q64_wrong_fishing_reserve <- function(.data){

}

plot_q64_wrong_fishing_reserve <- function(.data, use_plotly = TRUE){
hhs_Q64 <- .data[,c("maa", "64_wrong_fishing_reserve")] %>%
                        dplyr::filter (`64_wrong_fishing_reserve` != "") %>%
                           droplevels()
         
         hhs_Q64$`64_wrong_fishing_reserve` <- 
               dplyr::recode_factor(
                  hhs_Q64$`64_wrong_fishing_reserve`,
                        "1. Nada malo" = "not at all",
                        "Un poquito malo" = "slightly",
                        "3. Moderadamente malo" = "moderately",
                        "4. Muy malo" = "very wrong",
                        "5. Extremadamente malo" = "extremely wrong"
                  )
                                                                    
         Q64_summary <- proportion (hhs_Q64$`64_wrong_fishing_reserve`,
                                    hhs_Q64$maa,
                                    3,5)
         colnames(Q64_summary) <-
            c("MA name",
               "N",
               "Not wrong at all",
               "Slightly wrong",
               "Moderately wrong",
               "Very wrong",
               "Extremely wrong"
              )
         
         Q64_summary_long <-
            as.data.frame(
               Q64_summary %>% 
                  pivot_longer(
                     cols = c(
                        "Not wrong at all",
                        "Slightly wrong",
                        "Moderately wrong",
                        "Very wrong",
                        "Extremely wrong"
                     ),
                     names_to = "key",
                     values_to = "Proportion (%)"
                  )
               )
         
         Q64_summary_long$key <-
            factor(
               Q64_summary_long$key,
               levels = c(
                  "Not wrong at all",
                  "Slightly wrong",
                  "Moderately wrong",
                  "Very wrong",
                  "Extremely wrong"
               )
            )
         
         Q64 <- clean_plot_data(Q64_summary_long)
         
         #plot
         plot_Q64 <-
            ggplot(Q64, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare +
            geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap(
               ~ key,
               scale = input$x_axis,
               labeller = label_wrap_gen(25),
               ncol = 5
            ) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 20)) +
            ggtitle("Proportion of community members who think is wrong to fish in the reserve") +
            xlab (NULL) + ylab ("Proportion (%)") + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q64, height = 750)
}