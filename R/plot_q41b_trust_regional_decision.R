prep_q41b_trust_regional_decision <- function(.data){
  hhs_Q41b <- .data %>% 
    dplyr::select(maa, `41b_trust_regional_decision`) %>% 
    dplyr::mutate(
      `41b_trust_regional_decision` = dplyr::recode(
        `41b_trust_regional_decision`,
        "Strongly disagree" = 1,
        "Disagree" = 2,
        "Neither" = 3,
        "Agree" = 4,
        "Strongly agree" = 5,
        "Not Answered" = as.double(NA)
      )
    ) %>% 
    dplyr::filter(`41b_trust_regional_decision` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
    droplevels()
  
  Q41b_summary <-
    proportion (hhs_Q41b[[2]],
                hhs_Q41b[[1]],
                3, type = 5)
  colnames(Q41b_summary) <- c("MA name", 
                              "N", 
                              "Strongly disagree", 
                              "Disagree",
                              "Neither agree nor disagree",
                              "Agree",
                              "Strongly agree")
  Q41b <- Q41b_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  #sum agree and strongly agree
  Q41b$`Political Trust` <- as.numeric(Q41b$Agree) + as.numeric(Q41b$`Strongly agree`)
  Q41b_summary <- Q41b[, c("MA name", "N", "Political Trust")]
  
  #rename columns for shiny app
  colnames(Q41b_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q41b <- clean_plot_data(Q41b_summary)
  Q41b
}

plot_q41b_trust_regional_decision <- function(.data, ...){

  .data_plot <- prep_q41b_trust_regional_decision(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who trust in \nregional goverment to make decisions that benefit \nthe community over their own interests"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )

}