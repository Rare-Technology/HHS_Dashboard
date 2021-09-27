
prep_q30a_trust_local_decision <- function(.data){
  hhs_Q30a <- .data[,c("maa", "30_trust_local_decision")] %>%
    dplyr::filter(`30_trust_local_decision` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
    droplevels()
  
  #propoortion
  Q30a_summary <-
    proportion(hhs_Q30a$`30_trust_local_decision`,
               hhs_Q30a$maa,
               3, type=5)
  
  colnames(Q30a_summary) <- c("MA name", 
                              "N", 
                              "Strongly disagree", 
                              "Disagree",
                              "Neither agree nor disagree",
                              "Agree",
                              "Strongly agree")
  
  Q30a <- Q30a_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  #combine answer
  Q30a$`Proportion (%)` <- as.numeric(Q30a$Agree) + as.numeric(Q30a$`Strongly agree`)
  
  Q30a <- clean_plot_data(Q30a)
  
  Q30a
}

plot_q30a_trust_local_decision <- function(.data, ...){

  .data_plot <- prep_q30a_trust_local_decision(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who trust in the local goverment \nto make decisions that benefit the community over their own interests"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}