
prep_q78_financial_decisions <- function(.data){


  hhs_Q78 <- .data[,c("6_gender", "maa", "78_financial_decisions")] %>%
    dplyr::filter(`78_financial_decisions` != "") %>%
    droplevels()
  
  hhs_Q78_f <- (subset(hhs_Q78, `6_gender` == "F"))
  hhs_Q78_m <- (subset(hhs_Q78, `6_gender` == "M"))
  
  Q78_length <-
    tapply(hhs_Q78$`78_financial_decisions`,
           hhs_Q78$maa,
           length)
  Q78_length <- as.vector(Q78_length)
  Q78_count_f <-
    as.data.frame(tapply(
      hhs_Q78_f$`78_financial_decisions`,
      list(
        hhs_Q78_f$maa,
        hhs_Q78_f$`78_financial_decisions`
      ),
      length
    ))
  Q78_count_m <-
    as.data.frame(tapply(
      hhs_Q78_m$`78_financial_decisions`,
      list(
        hhs_Q78_m$maa,
        hhs_Q78_m$`78_financial_decisions`
      ),
      length
    ))
  Q78_count_f[is.na(Q78_count_f)] <- 0
  Q78_count_m[is.na(Q78_count_m)] <- 0
  Q78_count_both <-  (Q78_count_f$Both +  Q78_count_m$Both)
  Q78_count_f_total <-
    Q78_count_f$`Female partner` + Q78_count_f$Myself + Q78_count_m$`Female partner`
  Q78_count_m_total <-
    Q78_count_m$`Male partner` + Q78_count_m$Myself + Q78_count_f$`Male partner`
  
  ### Proportions
  Q78_summary_bind <-
    data.frame(
      "MA name" = levels(factor(hhs_Q78_f$maa)),
      N = as.numeric(Q78_length),
      Female = round(Q78_count_f_total / Q78_length, 3) *
        100,
      Male = round(Q78_count_m_total / Q78_length, 3) *
        100,
      Both = round(Q78_count_both / Q78_length, 3) *
        100
    )
  Q78_summary <-
    rbind(Q78_summary_bind,
          c(
            NA,
            sum(Q78_summary_bind$N),
            compute_summary_line(Q78_summary_bind$Female, 1),
            compute_summary_line(Q78_summary_bind$Male, 1),
            compute_summary_line(Q78_summary_bind$Both, 1)
          ))
  colnames(Q78_summary) <-
    c("MA name", "N", "Female (%)", "Male (%)", "Both (%)")
  
  #pivot table
  Q78_summary_long <-
    as.data.frame(
      Q78_summary %>% tidyr::pivot_longer(
        cols = c("Female (%)", "Male (%)", "Both (%)"),
        names_to = "key",
        values_to = "Proportion (%)"
      )
    )
  
  Q78 <- clean_plot_data(Q78_summary_long)
  Q78
  
}

plot_q78_financial_decisions <- function(.data, ...){
  
  .data_plot <- prep_q78_financial_decisions(.data)

  p <- plot_horiz_bar(
    .data_plot,
    title = "Proportion of community members who make financial decisions for the household",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}