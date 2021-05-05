prep_q44b_meeting_attendance_alt <- function(.data){

}

plot_q44b_meeting_attendance_alt <- function(.data, ...){
hhs_fishers <- .data[,c("submissionid","maa", 
                                          "12a_fishing_men", "12b_fishing_women",
                                          "12c_fishing_children")] %>%
                              dplyr::filter(`12a_fishing_men` > 0 |
                                     `12b_fishing_women` > 0 | 
                                     `12c_fishing_children` > 0) %>%
                                 droplevels()
        hhs_Q44f <-
            dplyr::left_join(
               hhs_fishers[, c("submissionid")],
               selectedData_Q44()[,c("submissionid",
                                     "maa", 
                                     "44_meeting_attendance")],
               by = "submissionid") %>%
           dplyr::filter(`44_meeting_attendance` != "") %>% 
            dplyr::filter(`44_meeting_attendance` != "na") %>%
               droplevels()
         
        # replace no respond and no management with NO
        #hhs_Q44f$`44_meeting_attendance`[hhs_Q44f$`44_meeting_attendance` == ""] <- "No"
        #hhs_Q44f$`44_meeting_attendance`[hhs_Q44f$`44_meeting_attendance` == "No management"] <- "No"
              
        Q44f_summary <- proportion(hhs_Q44f[[3]],
                                   hhs_Q44f[[2]],
                                   3,
                                   length(unique(hhs_Q44f[[3]])))
         
         Q44f_summary_long <-
            Q44f_summary %>% tidyr::pivot_longer(
               cols = names(Q44f_summary[-c(1:2)]),
               names_to = "key",
               values_to = "Proportion (%)"
            )
         Q44f_summary_long$key <- str_replace (Q44f_summary_long$key, "[.]", " ")
         Q44f <- clean_plot_data(Q44f_summary_long)
         
         #Plot
         plot_Q44f <-
            ggplot(Q44f, aes(`MA name`, `Proportion (%)`, N = N)) +
            theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
            facet_wrap( ~ key, scale = input$x_axis, ncol = 6) +
            
            scale_y_continuous(limits = c(0, 110),
                               breaks = seq(0, 100, 25)) +
            ggtitle("Proportion of fishers who \nattend management body meetings regularly") +
            xlab (NULL) + ylab (NULL) + 
            coord_flip(clip = "on")
         
         ggplotly(plot_Q44f, height = 750)
}