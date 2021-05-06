plot_q45_leadership_position <- function(.data, ...){

  
          .data <- .data %>% 
            dplyr::select(maa, `45_leadership_position`) %>% 
            tidyr::unnest(cols = `45_leadership_position`)

          .data_plot <-prep_data_for_plot(
            .data, 
            focus_var = `45_leadership_position`, 
            type = "facet"
          )

          plot_horiz_bar(
            .data_plot,
            title = "Proportion of community members that \nhold leadership positions in the management body",
            facet_var = key
          )
          
}