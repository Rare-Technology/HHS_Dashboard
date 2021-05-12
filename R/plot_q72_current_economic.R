
prep_q72_current_economic <- function(.data){
  

  hhs_Q72 <- .data[,c("maa", "72_current_economic")] %>%
    dplyr::filter(`72_current_economic`!= "")
  
  Q72_length <-
    tapply(hhs_Q72$`72_current_economic` ,
           hhs_Q72$maa,
           length)
  hhs_Q72$`72_current_economic_no` <-
    as.numeric(as.character(
      dplyr::recode_factor(
        hhs_Q72$`72_current_economic`,
        "Much better" = "5",
        "Slightly better" = "4",
        "Neither" = "3",
        "Slightly worse" = "2",
        "Much worse" = "1"
      )
    ))
  Q72_summary_bind <-
    data.frame(N = Q72_length, AVG = round(
      tapply(
        hhs_Q72$`72_current_economic_no`,
        hhs_Q72$maa,
        mean
      ),
      2
    ))
  
  Q72_summary <-
    rbind(Q72_summary_bind, "Mean ± SE" = c(
      sum(Q72_summary_bind$N),
      compute_summary_line(Q72_summary_bind$AVG, 2)
    ))
  
  Q72_summary_rn <-tibble::rownames_to_column(Q72_summary, "MA name")
  colnames(Q72_summary_rn) <- c("MA name", "N", "Current")
  
  ### Q73 Future economic income ####
  
  hhs_Q73 <- .data[,c("maa", "73_future_economic")] %>%
    dplyr::filter(`73_future_economic` != "")
  
  Q73_length <-
    tapply(hhs_Q73$`73_future_economic` ,
           hhs_Q73$maa,
           length)
  hhs_Q73$`73_future_economic_no` <-
    as.numeric(as.character(
      dplyr::recode_factor(
        hhs_Q73$`73_future_economic`,
        "Much better" = "5",
        "Slightly better" = "4",
        "Neither" = "3",
        "Slightly worse" = "2",
        "Much worse" = "1"
      )
    ))
  Q73_summary_bind <- data.frame(N = Q73_length,
                                 AVG = round(
                                   tapply(
                                     hhs_Q73$`73_future_economic_no`,
                                     hhs_Q73$maa,
                                     mean
                                   ),
                                   2
                                 ))
  
  Q73_summary <-
    rbind(Q73_summary_bind, "Mean ± SE" = c(
      sum(Q73_summary_bind$N),
      compute_summary_line(Q73_summary_bind$AVG, 2)
    ))
  Q73_summary_rn <-tibble::rownames_to_column(Q73_summary, "MA name")
  colnames(Q73_summary_rn) <- c("MA name", "N", "Future")
  
  #combine Q72 and Q73
  Q72_73_summary <- cbind(Q72_summary_rn, Q73_summary_rn[, -1])
  
  Q72_73 <-
    Q72_73_summary %>% tidyr::pivot_longer (
      cols = c("Current", "Future"),
      names_to = "key",
      values_to = "Proportion (%)"
    )
  #Fix Ns
  Q72_73$N <-
    as.data.frame(
      tidyr::pivot_longer(
        Q72_73,
        cols = c("N", "N"),
        names_repair = "unique",
        names_to = "No",
        values_to = "N"
      )
    )$N
  
  Q72_73a <- clean_plot_data(Q72_73)
  
  colnames(Q72_73a) <- c("MA name", "N", "key", "Average")
  Q72_73a
}

plot_q72_current_economic <- function(.data, ...){

        .data_plot <- prep_q72_current_economic(.data)
        
        plot_horiz_bar(
          .data_plot,
          y_var = Average,
          title = "Average perception of current and future personal economic situation",
          y_title = "Average score (much worse = 1; worse = 2; neither = 3; better = 4; much better = 5)",
          limits = c(0, 5.5),
          breaks = 1:5,
          facet_var = key
        )
         #Plot
         # plot_Q72_73 <-
         #    ggplot(Q72_73a, aes(`MA name`, Average, N = N)) +
         #       theme_rare +
         #       geom_col(alpha = 0.8, fill = "#005BBB") +
         #       facet_wrap(~key) +
         #       ggtitle("Average perception of current and future personal economic situation") +
         #       scale_y_continuous(limits = c(0, 5.5), breaks = c(1:5)) +
         #       #scale_fill_distiller(palette = "Spectral")+
         #       xlab (NULL) +
         #       ylab ("Average score (much worse = 1; worse = 2; neither = 3; better = 4; much better = 5)") +
         #       coord_flip(clip = "on")
         # 
         # ggplotly(plot_Q72_73, height = 750)
}