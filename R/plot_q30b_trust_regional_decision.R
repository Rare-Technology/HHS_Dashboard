
prep_q30b_trust_regional_decision <- function(.data){
  hhs_Q30b <- .data[,c("maa", "30_trust_regional_decision")] %>%
    dplyr::filter(`30_trust_regional_decision` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
    droplevels()
  
  #proportion
  Q30b_summary <-
    proportion (hhs_Q30b[[2]],
                hhs_Q30b[[1]],
                3, type = 5)
  colnames(Q30b_summary) <- c("MA name", 
                              "N", 
                              "Strongly disagree", 
                              "Disagree",
                              "Neither agree nor disagree",
                              "Agree",
                              "Strongly agree")
  Q30b <- Q30b_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  #sum agree and strongly agree
  Q30b$`Political Trust` <- as.numeric(Q30b$Agree) + as.numeric(Q30b$`Strongly agree`)
  Q30b_summary <- Q30b[, c("MA name", "N", "Political Trust")]
  
  #rename columns for shiny app
  colnames(Q30b_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q30b <- clean_plot_data(Q30b_summary)
  Q30b
}

plot_q30b_trust_regional_decision <- function(.data, ...){

  .data_plot <- prep_q30b_trust_regional_decision(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who trust in \nregional goverment to make decisions that benefit \nthe community over their own interests"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )

}