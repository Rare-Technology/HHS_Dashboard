
prep_q27_emergency_funds <- function(.data){

  hhs_Q27 <-
    .data[, c(
      "maa",
      "27a_emergency_personal",
      "27b_emergency_family",
      "27c_emergency_friend",
      "27d_emergency_entrepreneur",
      "27e_emergency_savings_club",
      "27f_emergency_lender",
      "27g_emergency_commercial_ind",
      "27h_emergency_commercial_group",
      "27i_emergency_microfinance_ind",
      "27j_emergency_microfinance_group",
      "27k_emergency_ngo_ind",
      "27l_emergency_ngo_group",
      "27m_emergency_insurance",
      "27n_emergency_other"
    )] %>%
    dplyr::filter(maa != '')
  #sum in obe variable
  hhs_Q27$`27n_emergency_other` <- as.numeric(as.character(hhs_Q27$`27n_emergency_other`))
  hhs_Q27[is.na(hhs_Q27)] <- 0
  hhs_Q27$X27_emergency_fund <-
    hhs_Q27$`27a_emergency_personal` +
    hhs_Q27$`27b_emergency_family` +
    hhs_Q27$`27c_emergency_friend` +
    hhs_Q27$`27d_emergency_entrepreneur` +
    hhs_Q27$`27e_emergency_savings_club` +
    hhs_Q27$`27f_emergency_lender` +
    hhs_Q27$`27g_emergency_commercial_ind` +
    hhs_Q27$`27h_emergency_commercial_group` +
    hhs_Q27$`27i_emergency_microfinance_ind` +
    hhs_Q27$`27j_emergency_microfinance_group` +
    hhs_Q27$`27k_emergency_ngo_ind` +
    hhs_Q27$`27l_emergency_ngo_group` +
    hhs_Q27$`27m_emergency_insurance` +
    hhs_Q27$`27n_emergency_other`
  
  #Convert to 0 and 1
  hhs_Q27$X27_emergency_fund_0_1 <-
    ifelse(hhs_Q27$X27_emergency_fund > 0, 1, 0) 
  ##addboth responses always
  
  hhs_Q27 <- rbind(hhs_Q27, c(NA,0), c(NA,1))
  
  #proportion
  Q27_summary <-
    proportion(hhs_Q27$X27_emergency_fund_0_1, 
               hhs_Q27$maa, 3, 2)[, -3]
  #col rename
  colnames(Q27_summary) <- c("MA name", "N", "Proportion (%)")
  #summary
  Q27 <- clean_plot_data(Q27_summary)
  
  Q27
}

plot_q27_emergency_funds <- function(.data, ...){

  .data_plot <- prep_q27_emergency_funds(.data)
  
  p <- plot_horiz_bar(
   .data_plot,
   title = "Proportion of households with access to emergency funds"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}