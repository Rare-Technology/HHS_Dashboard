prep_q41c_trust_community <- function(.data){
  hhs_Q41c <- .data %>% 
    dplyr::select(maa, `41c_trust_community`) %>% 
    dplyr::mutate(
      `41c_trust_community` = dplyr::recode(
        `41c_trust_community`,
        "Strongly disagree" = 1,
        "Disagree" = 2,
        "Neither" = 3,
        "Agree" = 4,
        "Strongly agree" = 5,
        "Not Answered" = as.double(NA)
      )
    ) %>% 
    dplyr::filter(`41c_trust_community` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
    droplevels()
  
  Q41c_summary<-
    proportion(hhs_Q41c[[2]], 
               hhs_Q41c[[1]], 
               3, type = 5)
  colnames(Q41c_summary) <- c("MA name", 
                              "N", 
                              "Strongly disagree", 
                              "Disagree",
                              "Neither agree nor disagree",
                              "Agree",
                              "Strongly agree")
  
  Q41c <- Q41c_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  #sum agree and strongly agree
  Q41c$`Social Cohesion` <- as.numeric(Q41c$Agree) + as.numeric(Q41c$`Strongly agree`)
  Q41c_summary <- Q41c[, c("MA name", "N", "Social Cohesion")]
  
  #rename columns for shiny app
  colnames(Q41c_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q41c <- clean_plot_data(Q41c_summary)
}

plot_q41c_trust_community <- function(.data, ...){

  .data_plot <- prep_q41c_trust_community(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who trust in \ntheir fellow community members"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}