prep_q41a_trust_local_decision <- function(.data){
  hhs_Q41a <- .data %>% 
    dplyr::select(maa, `41a_trust_local_decision`) %>% 
    dplyr::mutate(
      `41a_trust_local_decision` = dplyr::recode(
        `41a_trust_local_decision`,
        "Strongly disagree" = 1,
        "Disagree" = 2,
        "Neither" = 3,
        "Agree" = 4,
        "Strongly agree" = 5,
        "Not Answered" = as.double(NA)
      )
    ) %>% 
    dplyr::filter(`41a_trust_local_decision` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
    droplevels()
  
  Q41a_summary <-
    proportion(hhs_Q41a$`41a_trust_local_decision`,
               hhs_Q41a$maa,
               3, type=5)
  
  colnames(Q41a_summary) <- c("MA name", 
                              "N", 
                              "Strongly disagree", 
                              "Disagree",
                              "Neither agree nor disagree",
                              "Agree",
                              "Strongly agree")
  
  Q41a <- Q41a_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  #combine answer
  Q41a$`Proportion (%)` <- as.numeric(Q41a$Agree) + as.numeric(Q41a$`Strongly agree`)
  
  Q41a <- clean_plot_data(Q41a)
  
  Q41a
}

plot_q41a_trust_local_decision <- function(.data, ...){

  .data_plot <- prep_q41a_trust_local_decision(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who trust in the local goverment \nto make decisions that benefit the community over their own interests"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}