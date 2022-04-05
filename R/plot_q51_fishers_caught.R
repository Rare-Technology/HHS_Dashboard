prep_q51_fishers_caught <- function(.data){
  hhs_Q51d <- .data[,c("maa", "51d_fishers_violate_fish_size")] %>%
    dplyr::filter(`51d_fishers_violate_fish_size` %in% c(0:10)) %>%
    droplevels()
  Q51d_length <-
    tapply(hhs_Q51d$`51d_fishers_violate_fish_size`,
           hhs_Q51d$maa,
           length)
  Q51d_length <- as.vector(Q51d_length)
  Q51d_mean <-
    data.frame(
      freq = tapply(
        hhs_Q51d$`51d_fishers_violate_fish_size`,
        hhs_Q51d$maa,
        mean
      ) / 10
    )
  ### Proportions
  Q51d_summary_bind <-
    cbind(N = Q51d_length, round(Q51d_mean, 3) * 100)
  Q51d_summary <-
    rbind(Q51d_summary_bind, "Mean ± SE" = c(
      sum(Q51d_summary_bind$N, na.rm = TRUE),
      compute_summary_line(Q51d_summary_bind$freq, 1)
    ))
  colnames(Q51d_summary) <- c("N", "Fish Size Violations (%)")
  Q51d_summary <-tibble::rownames_to_column(Q51d_summary, "MA name")
  
  hhs_Q51e <- .data[,c("maa", "51e_fishers_caught")] %>%
    dplyr::filter (`51e_fishers_caught` != "") %>%
    droplevels()
  
  Q51e_length <-
    tapply(hhs_Q51e$`51e_fishers_caught`,
           hhs_Q51e$maa,
           length)
  Q51e_length <- as.vector(Q51e_length)
  Q51e_mean <-
    data.frame(freq = tapply(
      hhs_Q51e$`51e_fishers_caught`,
      hhs_Q51e$maa,
      mean
    ) / 10)
  Q51e_summary_bind <-
    cbind(N = Q51e_length, round(Q51e_mean, 3) * 100)
  Q51e_summary <-
    rbind(Q51e_summary_bind, "Mean ± SE" = c(
      sum(Q51e_summary_bind$N, na.rm = TRUE),
      compute_summary_line(Q51e_summary_bind$freq, 1)
    ))
  ##rownames to column
  Q51e_summary <-tibble::rownames_to_column(Q51e_summary, "MA name")
  colnames(Q51e_summary) <-
    c("MA name", "N", "Reserve Violations (%)")
  
  ### Combine Q51d and Q51e
  Q51d_51e_summary <-
    plyr::join_all(list(Q51d_summary, Q51e_summary),
                   by = "MA name",
                   type = "right")
  #pivot table
  Q51d_e_summary_long <-
    as.data.frame(
      Q51d_51e_summary %>% tidyr::pivot_longer(
        cols = c(
          "Fish Size Violations (%)",
          "Reserve Violations (%)"
        ),
        names_to = "key",
        values_to = "Proportion (%)"
      )
    )
  #Fix Ns
  Q51d_e_summary_long$N <-
    as.data.frame(
      tidyr::pivot_longer(
        Q51d_e_summary_long,
        cols = c("N", "N"),
        names_repair = "unique",
        names_to = "No",
        values_to = "N"
      )
    )$N
  
  Q51d_e <- clean_plot_data(Q51d_e_summary_long)
  Q51d_e
}

plot_q51_fishers_caught <- function(.data, ...){

  .data_plot <- prep_q51_fishers_caught(.data)
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