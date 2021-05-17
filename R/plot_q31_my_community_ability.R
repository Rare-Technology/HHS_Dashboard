
prep_q31_my_community_ability <- function(.data){
  hhs_Q31 <- .data[,c("maa", "31_my_community_ability")] %>%
    dplyr::filter(`31_my_community_ability` != "") %>%
    dplyr::filter(`31_my_community_ability` != "No dependance") %>%
    rbind(c(NA, "Agree"), 
          c(NA, "Neither"), 
          c(NA, "Strongly agree"),
          c(NA, "Disagree"),
          c(NA, "Strongly disagree")) %>%
    droplevels()
  #proportion
  Q31_summary <-
    proportion(hhs_Q31[[2]],
               hhs_Q31[[1]],
               3, type=5)
  
  Q31 <- Q31_summary %>%
    dplyr::filter(`MA name` != "Mean ± SE")
  
  colnames(Q31) <- c("MA name", "N", 
                     "Agree", 
                     "Disagree",
                     "Neither",
                     "Strongly agree",
                     "Strongly disagree")
  
  #combine agree answer
  Q31$`Collective Efficacy` <- as.numeric(Q31$Agree) + as.numeric(Q31$`Strongly agree`)
  
  #summary
  Q31_summary <- Q31[, c("MA name", "N", "Collective Efficacy")]
  
  colnames(Q31_summary) <- c("MA name", "N", "Proportion (%)")
  
  Q31 <- clean_plot_data(Q31_summary)
  Q31
}

plot_q31_my_community_ability <- function(.data, ...){


  .data_plot <- prep_q31_my_community_ability(.data)
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who believe that the community has the ability to manage the fishery effectively to maximize food and profits"
  )

}