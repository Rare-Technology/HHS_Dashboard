
prep_q30c_trust_community <- function(.data){
  
  hhs_Q30c <- .data[,c("maa", "30_trust_community")] %>%
    dplyr::filter(`30_trust_community` %in% c(1:5)) %>%
    rbind(c(NA,1),c(NA,2),c(NA,3),c(NA,4),c(NA,5)) %>%
    droplevels()
  
  #propoortion
  Q30c_summary<-
    proportion(hhs_Q30c[[2]], 
               hhs_Q30c[[1]], 
               3, type = 5)
  colnames(Q30c_summary) <- c("MA name", 
                              "N", 
                              "Strongly disagree", 
                              "Disagree",
                              "Neither agree nor disagree",
                              "Agree",
                              "Strongly agree")
  
  Q30c <- Q30c_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  #sum agree and strongly agree
  Q30c$`Social Cohesion` <- as.numeric(Q30c$Agree) + as.numeric(Q30c$`Strongly agree`)
  Q30c_summary <- Q30c[, c("MA name", "N", "Social Cohesion")]
  
  #rename columns for shiny app
  colnames(Q30c_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q30c <- clean_plot_data(Q30c_summary)
}

plot_q30c_trust_community <- function(.data, ...){

  .data_plot <- prep_q30c_trust_community(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who trust in \ntheir fellow community members"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}