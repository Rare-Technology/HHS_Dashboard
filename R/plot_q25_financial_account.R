
prep_q25_financial_account <- function(.data){
  
  hhs_Q25 <- .data[, c(
    "maa",
    "25a_financial_bank",
    "25b_financial_micro",
    "25c_financial_ngo",
    "25d_financial_lender",
    "25e_financial_insurance",
    "25f_financial_other"
  )] %>%
    dplyr::filter(maa != "")
  
  # added by Zev
  hhs_Q25$`25f_financial_other` <- ifelse(is.na(hhs_Q25$`25f_financial_other`), 0, 1)
  
  #replace NA with 0s
  hhs_Q25[is.na(hhs_Q25)] <- 0
  
  #Summary of financials accounts
  hhs_Q25$X25_financial_account <-
    as.numeric(hhs_Q25$`25a_financial_bank`) +
    as.numeric(hhs_Q25$`25b_financial_micro`) +
    as.numeric(hhs_Q25$`25c_financial_ngo`) +
    as.numeric(hhs_Q25$`25d_financial_lender`) +
    as.numeric(hhs_Q25$`25e_financial_insurance`) +
    as.numeric(hhs_Q25$`25f_financial_other`) #zev removed code
  
  #Convert to 0 and 1
  hhs_Q25$X25_financial_account_0_1 <-
    ifelse(hhs_Q25$X25_financial_account > 0, 1, 0)
  
  hhs_Q25 <- hhs_Q25[,c("maa", "X25_financial_account_0_1")] %>%
    rbind(c(NA,0), c(NA,1))
  
  #proportion
  Q25_summary <-
    proportion(question = hhs_Q25[[2]],
               grouping = hhs_Q25[[1]],
               rounding = 3,
               type = 2)[, -3]
  #rename columns
  colnames(Q25_summary) <- c("MA name", "N", "Proportion (%)")
  #summary
  Q25 <- clean_plot_data(Q25_summary)
  
  Q25
}

plot_q25_financial_account <- function(.data, use_plotly = TRUE){

  
  .data_plot <- prep_q25_financial_account(.data)
  
  plot_horiz_bar(
    .data_plot,
    title = "Proportion of households with \nactive accounts in financial institutions",
    
  ) 
         # #plot
         # plot_Q25 <-
         #    ggplot(Q25, aes(`MA name`, `Proportion (%)`, N = N)) +
         #    theme_rare + geom_col(fill = "#005BBB", alpha = 0.8) +
         #    ggtitle("Proportion of households with \nactive accounts in financial institutions") +
         #    scale_y_continuous(limits = c(0, 110),
         #                       breaks = seq(0, 100, 20)) +
         #    xlab (NULL) + ylab ("Proportion (%)") + 
         #    coord_flip(clip = "on")
         # 
         # ggplotly(plot_Q25, height = 750)
}