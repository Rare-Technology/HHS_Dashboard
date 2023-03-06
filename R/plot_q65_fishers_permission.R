prep_q65_fishers_permission <- function(.data){
  hhs_Q65a <- .data %>% 
    dplyr::select(maa, `65a_fishers_gear_not_permitted`) %>%
    dplyr::mutate(
      `65a_fishers_gear_not_permitted` = as.double(`65a_fishers_gear_not_permitted`)
    ) %>% 
    dplyr::filter(`65a_fishers_gear_not_permitted` %in% c(0:10))
  
  ## Summary 
  Q65a_length <-
    tapply(hhs_Q65a$`65a_fishers_gear_not_permitted`,
           hhs_Q65a$maa,
           length)
  Q65a_length <- as.vector(Q65a_length)
  Q65a_mean <-
    as.data.frame(
      tapply(
        hhs_Q65a$`65a_fishers_gear_not_permitted`,
        hhs_Q65a$maa,
        mean
      ) / 10
    )
  Q65a_summary_bind <-
    cbind(N = Q65a_length, round(Q65a_mean, 3) * 100)
  colnames(Q65a_summary_bind) <- c("N", "Unapproved gear")
  Q65a_summary <- rbind(Q65a_summary_bind,
                        "Mean ± SE" = c(
                          sum(Q65a_summary_bind$N),
                          compute_summary_line(Q65a_summary_bind[[2]], 1)
                        ))
  Q65a_summary <-tibble::rownames_to_column(Q65a_summary, "MA name")
  
  ### Q65b Frequency of observed fishing in reserve ####
  hhs_Q65b <- .data %>% 
    dplyr::select(maa, `65b_fishers_reserves`) %>%
    dplyr::mutate(
      `65b_fishers_reserves` = as.double(`65b_fishers_reserves`)
    ) %>% 
    dplyr::filter(`65b_fishers_reserves` %in% c(0:10))
  
  Q65b_length <-
    tapply(hhs_Q65b$`65b_fishers_reserves`,
           hhs_Q65b$maa,
           length)
  Q65b_length <- as.vector(Q65b_length)
  Q65b_mean <-
    data.frame(avg = tapply(
      hhs_Q65b$`65b_fishers_reserves`,
      hhs_Q65b$maa,
      mean
    ) / 10)
  Q65b_summary_bind <-
    cbind(N = Q65b_length, round(Q65b_mean, 3) * 100)
  Q65b_summary <-
    rbind(Q65b_summary_bind, "Mean ± SE" = c(
      sum(Q65b_summary_bind$N),
      compute_summary_line(Q65b_summary_bind$avg, 1)
    ))
  colnames(Q65b_summary) <- c("N", "Fishing in reserve")
  Q65b_summary <-tibble::rownames_to_column(Q65b_summary, "MA name")
  
  ### Q65c Frequency of observed unpermitted fishing in MA ####
  hhs_Q65c <- .data %>% 
    dplyr::select(maa, `65c_fishers_ma_area`) %>%
    dplyr::mutate(
      `65c_fishers_ma_area` = as.double(`65c_fishers_ma_area`)
    ) %>% 
    dplyr::filter(`65c_fishers_ma_area` %in% c(0:10))
  
  Q65c_length <-
    tapply(hhs_Q65c$`65c_fishers_ma_area`,
           hhs_Q65c$maa,
           length)
  Q65c_length <- as.vector(Q65c_length)
  Q65c_mean <-
    data.frame(avg = tapply(
      hhs_Q65c$`65c_fishers_ma_area`,
      hhs_Q65c$maa,
      mean
    ) / 10)
  Q65c_summary_bind <-
    cbind(N = Q65c_length, round(Q65c_mean, 3) * 100)
  colnames(Q65c_summary_bind) <-
    c("N", "Fishing without permission")
  Q65c_summary <-
    rbind(Q65c_summary_bind, "Mean ± SE" = c(
      sum(Q65c_summary_bind$N),
      compute_summary_line(Q65c_summary_bind[[2]], 1)
    ))
  Q65c_summary <-tibble::rownames_to_column(Q65c_summary, "MA name")
  
  ### Combine Q65a, Q65b and Q65c ####
  Q65a_c_summary <-
    plyr::join_all(
      list(Q65a_summary, Q65b_summary, Q65c_summary),
      by = "MA name",
      type = "left"
    )
  
  #pivot table
  Q65a_c_summary_long <-
    as.data.frame(
      Q65a_c_summary %>% tidyr::pivot_longer(
        cols = c(
          "Unapproved gear",
          "Fishing in reserve",
          "Fishing without permission"
        ),
        names_to = "key",
        values_to = "Proportion (%)"
      )
    )
  #Fix Ns
  Q65a_c_summary_long$N <-
    as.data.frame(
      tidyr::pivot_longer(
        Q65a_c_summary,
        cols = c("N", "N", "N"),
        names_repair = "unique",
        names_to = "No",
        values_to = "N"
      )
    )$N
  
  Q65a_c <- clean_plot_data(Q65a_c_summary_long)
  Q65a_c
}

plot_q65_fishers_permission <- function(.data, ...){

  .data_plot <- prep_q65_fishers_permission(.data)
 
  p <- plot_horiz_bar(
    .data_plot,
    title = "Perceived frequency of observing others violating regulations ",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}