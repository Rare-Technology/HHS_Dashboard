prep_q31_emergency_funds <- function(.data){
  Q31_cols <- stringr::str_subset(names(hhs_data), "^(?!.*specify)31")
  hhs_Q31 <- .data %>%
    dplyr::select(maa, Q31_cols) %>% 
    dplyr::mutate(
      dplyr::across(
        Q31_cols,
        ~ dplyr::recode(
          .x,
          "No" = 0,
          "Yes" = 1,
          .default = as.double(NA)
        )
      )
    )
    
  #sum in obe variable
  hhs_Q31$`31o_emergency_other` <- as.numeric(as.character(hhs_Q31$`31o_emergency_other`))
  hhs_Q31[is.na(hhs_Q31)] <- 0
  hhs_Q31$X31_emergency_fund <-
    hhs_Q31$`31a_emergency_personal` +
    hhs_Q31$`31b_emergency_family` +
    hhs_Q31$`31c_emergency_friend` +
    hhs_Q31$`31d_emergency_entrepreneur` +
    hhs_Q31$`31e_emergency_savings_club` +
    hhs_Q31$`31f_emergency_lender` +
    hhs_Q31$`31g_emergency_commercial_ind` +
    hhs_Q31$`31h_emergency_commercial_group` +
    hhs_Q31$`31i_emergency_microfinance_ind` +
    hhs_Q31$`31j_emergency_microfinance_group` +
    hhs_Q31$`31k_emergency_ngo_ind` +
    hhs_Q31$`31l_emergency_ngo_group` +
    hhs_Q31$`31m_emergency_insurance` +
    hhs_Q31$`31n_buyers_traders` +
    hhs_Q31$`31o_emergency_other`
  
  #Convert to 0 and 1
  hhs_Q31$X31_emergency_fund_0_1 <-
    ifelse(hhs_Q31$X31_emergency_fund > 0, 1, 0) 
  ##addboth responses always
  
  hhs_Q31 <- rbind(hhs_Q31, c(NA,0), c(NA,1))
  
  #proportion
  Q31_summary <-
    proportion(hhs_Q31$X31_emergency_fund_0_1, 
               hhs_Q31$maa, 3, 2)[, -3]
  #col rename
  colnames(Q31_summary) <- c("MA name", "N", "Proportion (%)")
  #summary
  Q31 <- clean_plot_data(Q31_summary)
  
  Q31
}

plot_q31_emergency_funds <- function(.data, ...){

  .data_plot <- prep_q31_emergency_funds(.data)
  
  p <- plot_horiz_bar(
   .data_plot,
   title = "Proportion of households with access to emergency funds"
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}