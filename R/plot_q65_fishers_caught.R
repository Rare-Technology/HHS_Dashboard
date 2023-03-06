prep_q65_fishers_caught <- function(.data){
  hhs_Q65d <- .data %>% 
    dplyr::select(maa, `65d_fishers_violate_fish_size`) %>%
    dplyr::filter(`65d_fishers_violate_fish_size` != "Not Answered") %>%
    dplyr::mutate(
      `65d_fishers_violate_fish_size` = as.double(`65d_fishers_violate_fish_size`)
    )
  
  Q65d_length <-
    tapply(hhs_Q65d$`65d_fishers_violate_fish_size`,
           hhs_Q65d$maa,
           length)
  
  Q65d_length <- as.vector(Q65d_length)
  Q65d_mean <-
    data.frame(
      freq = tapply(
        hhs_Q65d$`65d_fishers_violate_fish_size`,
        hhs_Q65d$maa,
        mean
      ) / 10
    )
  ### Proportions
  Q65d_summary_bind <-
    cbind(N = Q65d_length, round(Q65d_mean, 3) * 100)
  Q65d_summary <-
    rbind(Q65d_summary_bind, "Mean ± SE" = c(
      sum(Q65d_summary_bind$N, na.rm = TRUE),
      compute_summary_line(Q65d_summary_bind$freq, 1)
    ))
  colnames(Q65d_summary) <- c("N", "Fish Size Violations (%)")
  Q65d_summary <-tibble::rownames_to_column(Q65d_summary, "MA name")
  
  hhs_Q65e <- .data %>% 
    dplyr::select(maa, `65e_fishers_caught`) %>%
    dplyr::filter(`65e_fishers_caught` != "Not Answered") %>%
    dplyr::mutate(
      `65e_fishers_caught` = as.double(`65e_fishers_caught`)
    )
  
  Q65e_length <-
    tapply(hhs_Q65e$`65e_fishers_caught`,
           hhs_Q65e$maa,
           length)
  Q65e_length <- as.vector(Q65e_length)
  Q65e_mean <-
    data.frame(freq = tapply(
      hhs_Q65e$`65e_fishers_caught`,
      hhs_Q65e$maa,
      mean
    ) / 10)
  Q65e_summary_bind <-
    cbind(N = Q65e_length, round(Q65e_mean, 3) * 100)
  Q65e_summary <-
    rbind(Q65e_summary_bind, "Mean ± SE" = c(
      sum(Q65e_summary_bind$N, na.rm = TRUE),
      compute_summary_line(Q65e_summary_bind$freq, 1)
    ))
  ##rownames to column
  Q65e_summary <-tibble::rownames_to_column(Q65e_summary, "MA name")
  colnames(Q65e_summary) <-
    c("MA name", "N", "Reserve Violations (%)")
  
  ### Combine Q65d and Q65e
  Q65d_65e_summary <-
    plyr::join_all(list(Q65d_summary, Q65e_summary),
                   by = "MA name",
                   type = "right")
  #pivot table
  Q65d_e_summary_long <-
    as.data.frame(
      Q65d_65e_summary %>% tidyr::pivot_longer(
        cols = c(
          "Fish Size Violations (%)",
          "Reserve Violations (%)"
        ),
        names_to = "key",
        values_to = "Proportion (%)"
      )
    )
  #Fix Ns
  Q65d_e_summary_long$N <-
    as.data.frame(
      tidyr::pivot_longer(
        Q65d_e_summary_long,
        cols = c("N", "N"),
        names_repair = "unique",
        names_to = "No",
        values_to = "N"
      )
    )$N
  
  Q65d_e <- clean_plot_data(Q65d_e_summary_long)
  Q65d_e
}

plot_q65_fishers_caught <- function(.data, ...){
  .data_plot <- prep_q65_fishers_caught(.data)
  p <- plot_horiz_bar(
    .data_plot,
    title = "Perceived frequency of getting caught for violating regulations",
    facet_var = key
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}