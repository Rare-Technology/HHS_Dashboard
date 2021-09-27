prep_q44b_meeting_attendance_alt <- function(.data){


        .data <- .data[,c("submissionid","maa", 
                          "12a_fishing_men", "12b_fishing_women",
                          "12c_fishing_children", "44_meeting_attendance")] %>% 
                tidyr::unnest(cols = `44_meeting_attendance`)
        
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
                        .data[,c("submissionid",
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
        Q44f_summary_long$key <- stringr::str_replace (Q44f_summary_long$key, "[.]", " ")
        Q44f <- clean_plot_data(Q44f_summary_long)
        
        Q44f
        
}

plot_q44b_meeting_attendance_alt <- function(.data, ...){

.data_plot <- prep_q44b_meeting_attendance_alt(.data)

p <- plot_horiz_bar(
        .data_plot,
        title = "Proportion of fishers who \nattend management body meetings regularly",
        facet_var = key
)

result <- list(
        plot = p,
        data = .data_plot
)
}