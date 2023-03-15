prep_q42_my_community_ability <- function(.data){
  hhs_Q42 <- .data %>% 
    dplyr::select(maa, `42_my_community_ability`) %>%
    dplyr::filter(!(`42_my_community_ability` %in% c("No dependance", "Not Answered"))) %>%
    rbind(c(NA, "Agree"), 
          c(NA, "Neither"), 
          c(NA, "Strongly agree"),
          c(NA, "Disagree"),
          c(NA, "Strongly disagree")) %>%
    droplevels()
  
  #proportion
  Q42_summary <-
    proportion(hhs_Q42[[2]],
               hhs_Q42[[1]],
               3, type=5)
  
  Q42 <- Q42_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  colnames(Q42) <- c("MA name", "N", 
                     "Agree", 
                     "Disagree",
                     "Neither",
                     "Strongly agree",
                     "Strongly disagree")
  
  #combine agree answer
  Q42$`Collective Efficacy` <- as.numeric(Q42$Agree) + as.numeric(Q42$`Strongly agree`)
  
  #summary
  Q42_summary <- Q42[, c("MA name", "N", "Collective Efficacy")]
  
  colnames(Q42_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q42 <- clean_plot_data(Q42_summary)
  Q42
}

plot_q42_my_community_ability <- function(.data, ...){
  .data_plot <- prep_q42_my_community_ability(.data)
  
  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who believe that the community has the ability to manage the fishery effectively to maximize food and profits"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}