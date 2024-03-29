plot_horiz_bar <- function(
  .data,
  x_var = `MA name`,
  y_var = `Proportion (%)`,
  n_var = `N`,
  facet_var = ` `, # TODO: use NULL instead
  stack_var = `key`,
  fill_var = `key`,
  type = "bar", #stacked
  title = "",
  x_title = NULL,
  y_title = "Percentage (%)",
  fill_col = "#005BBB",
  palette = "Blues",
  palette_direction = -1,
  limits = c(0, 110),
  breaks = seq(0, 100, 25),
  guide_reverse = TRUE,
  sort_by_value = FALSE, 
  reverse_maa_order = TRUE
){


  # TODO: must be a better way!
  facet_var_str <- rlang::quo_name(rlang::enquo(facet_var))
  y_var_str <- rlang::quo_name(rlang::enquo(y_var))
  
  title <- stringr::str_wrap(title, width = 65)

  .data <- .data %>% 
    dplyr::mutate(
      {{ x_var }} := factor({{ x_var }})
    )

  if(reverse_maa_order){
    .data <- .data %>% 
      dplyr::mutate(
        {{ x_var }} := forcats::fct_rev({{ x_var }})
      )
  }
  
  if(sort_by_value){
    .data <- .data %>% 
      dplyr::mutate(
        {{ x_var }} := forcats::fct_reorder({{ x_var }}, {{ y_var }})
      )

  }
  

  
  
  p <- .data %>% 
    ggplot(aes({{ x_var }}, {{ y_var }}, N = {{ n_var }}))
  
  if(type == 'bar'){
    nudgeval <- ggplot2::layer_scales(p)$y$get_limits()[2] * 0.01
    p <- p + geom_col(fill = fill_col, alpha = 0.8) +
      geom_text(aes(label = {{ y_var }}), hjust = 0, nudge_y = nudgeval, color = "grey40", size = 4)
  }
  
  if(type == 'grouped'){
    nudgeval <- ggplot2::layer_scales(p)$y$get_limits()[2] * 0.01
    p <- p + geom_col(aes(fill = {{fill_var}}), position=position_dodge(width=0.9), alpha=0.8) +
      geom_text(aes(label = {{y_var}}, fill = {{fill_var}}),
                hjust = -1, position = position_dodge(width=0.9),
                color = "grey40", size = 4)
  }
  
  if(type == 'stacked'){
    p <- p + geom_bar(
      aes(fill = {{stack_var}}),
      position = position_stack(reverse = guide_reverse),
      stat = "identity",
      alpha = 0.8
    ) + 
      scale_fill_brewer(palette = palette, direction = palette_direction)
  }
  
  
  p <- p + 
    labs(
      title = title,
      x = x_title,
      y = y_title
    ) +
    scale_y_continuous(
      limits = limits,
      breaks = breaks
      ) +
    coord_flip(clip = "on") +
    theme_rare() 
  

  if(facet_var_str != " "){
    
    label_wrap_val <- 20 # good example is knowledge/41
    l <- .data[[facet_var_str]] %>% unique() %>% length()
    
    if(l >= 6) label_wrap_val <- 14
    
    p <- p + facet_wrap( vars({{ facet_var }}), labeller = label_wrap_gen(label_wrap_val), nrow = 1)
  }
  
  
  
  p
}

