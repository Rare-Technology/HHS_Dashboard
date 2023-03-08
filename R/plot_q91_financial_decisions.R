prep_q91_financial_decisions <- function(.data){
  hhs_Q91 <- .data %>% 
    dplyr::select(`6_gender`, maa, `91_financial_decisions`) %>%
    dplyr::mutate(
      `91_financial_decisions` = dplyr::recode(
        `91_financial_decisions`,
        "My male partner" = "Male partner",
        "My female partner" = "Female partner",
        "My nonbinary partner" = "Nonbinary partner"
      )
    ) %>% 
    dplyr::filter(`91_financial_decisions` != "Not Answered")
  
  hhs_Q91_f <- (subset(hhs_Q91, `6_gender` == "F"))
  hhs_Q91_m <- (subset(hhs_Q91, `6_gender` == "M"))
  hhs_Q91_nb <- (subset(hhs_Q91, `6_gender` == "NB"))
  
  Q91_length <-
    tapply(hhs_Q91$`91_financial_decisions`,
           hhs_Q91$maa,
           length)
  Q91_length <- as.vector(Q91_length)
  
  Q91_maa <- sort(unique(hhs_Q91$maa))
  zeros <- rep(0, length(Q91_maa))
  Q91_count_empty <- tibble::tibble(
    maa = Q91_maa,
    Both = zeros,
    `Male partner` = zeros,
    `Female partner` = zeros,
    `Nonbinary partner` = zeros,
    Myself = zeros
  )
  Q91_vals <- c("Both", "Male partner", "Female partner", "Nonbinary partner", "Myself")
  
  f_missing_cols <- setdiff(Q91_vals, unique(hhs_Q91_f$`91_financial_decisions`))
  Q91_count_f <- hhs_Q91_f %>% 
    dplyr::count(maa, `91_financial_decisions`) %>% 
    tidyr::pivot_wider(
      names_from=`91_financial_decisions`,
      values_from=n,
      values_fill=0
    ) %>% 
    # Right join in case the table so far is empty
    dplyr::right_join(Q91_count_empty[c("maa", f_missing_cols)], by="maa")
  
  m_missing_cols <- setdiff(Q91_vals, unique(hhs_Q91_m$`91_financial_decisions`))
  Q91_count_m <- hhs_Q91_m %>% 
    dplyr::count(maa, `91_financial_decisions`) %>% 
    tidyr::pivot_wider(
      names_from=`91_financial_decisions`,
      values_from=n,
      values_fill=0
    ) %>% 
    dplyr::right_join(Q91_count_empty[c("maa", m_missing_cols)], by="maa")
  
  nb_missing_cols <- setdiff(Q91_vals, unique(hhs_Q91_nb$`91_financial_decisions`))
  Q91_count_nb <- hhs_Q91_nb %>% 
    dplyr::count(maa, `91_financial_decisions`) %>% 
    tidyr::pivot_wider(
      names_from=`91_financial_decisions`,
      values_from=n,
      values_fill=0
    ) %>% 
    dplyr::right_join(Q91_count_empty[c("maa", nb_missing_cols)], by="maa")

  Q91_count_both <- (Q91_count_f$Both +  Q91_count_m$Both + Q91_count_nb$Both)
  Q91_count_f_total <-
    Q91_count_f$`Female partner` + Q91_count_f$Myself + 
    Q91_count_m$`Female partner` + Q91_count_nb$`Female partner`
  Q91_count_m_total <-
    Q91_count_m$`Male partner` + Q91_count_m$Myself +
    Q91_count_f$`Male partner` + Q91_count_nb$`Male partner`
  Q91_count_nb_total <- 
    Q91_count_nb$`Nonbinary partner` + Q91_count_nb$`Myself` + 
    Q91_count_m$`Nonbinary partner` + Q91_count_f$`Nonbinary partner`
  
  ### Proportions
  Q91_summary_bind <-
    data.frame(
      "MA name" = Q91_maa,
      N = as.numeric(Q91_length),
      Female = round(Q91_count_f_total / Q91_length, 3) * 100,
      Male = round(Q91_count_m_total / Q91_length, 3) * 100,
      Nonbinary = round(Q91_count_nb_total / Q91_length, 3) * 100,
      Both = round(Q91_count_both / Q91_length, 3) * 100
    )
  Q91_summary <-
    rbind(Q91_summary_bind,
          c(
            NA,
            sum(Q91_summary_bind$N),
            compute_summary_line(Q91_summary_bind$Female, 1),
            compute_summary_line(Q91_summary_bind$Male, 1),
            compute_summary_line(Q91_summary_bind$Both, 1)
          ))
  colnames(Q91_summary) <-
    c("MA name", "N", "Female (%)", "Male (%)", "Nonbinary (%)", "Both (%)")
  
  #pivot table
  Q91_summary_long <-
    as.data.frame(
      Q91_summary %>% tidyr::pivot_longer(
        cols = c("Female (%)", "Male (%)", "Nonbinary (%)", "Both (%)"),
        names_to = "key",
        values_to = "Proportion (%)"
      )
    )
  
  Q91 <- clean_plot_data(Q91_summary_long)
  Q91
}

plot_q91_financial_decisions <- function(.data, ...){
  
  .data_plot <- prep_q91_financial_decisions(.data)

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