plot_horiz_bar <- function(
  .data,
  x_var = `MA name`,
  y_var = `Proportion (%)`,
  n_var = `N`,
  facet_var = ` `, # TODO: use NULL instead
  stack_var = `key`,
  type = "bar", #stacked
  title = "",
  x_title = NULL,
  y_title = "Proportion (%)",
  fill_col = "#005BBB",
  palette = "Blues",
  palette_direction = -1,
  limits = c(0, 110),
  breaks = seq(0, 100, 25),
  guide_reverse = TRUE,
  sort_by_value = FALSE
){

  # TODO: must be a better way!
  facet_var_str <- quo_name(enquo(facet_var))

  
  title <- stringr::str_wrap(title, width = 65)
  label_wrap_val <- 
  
  if(sort_by_value){
    .data <- .data %>% 
      dplyr::mutate(
        {{ x_var }} := forcats::fct_reorder(factor({{ x_var }}), {{ y_var }})
      )

  }
  
  
  p <- .data %>% 
    ggplot(aes({{ x_var }}, {{ y_var }}, N = {{ n_var }}))
  
  
  if(sort_by_value){

  }
  
  if(type == 'bar'){
    p <- p + geom_col(fill = fill_col, alpha = 0.8)
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
    
    if(l >= 6) label_wrap_val <- 15
    
    p <- p + facet_wrap( vars({{ facet_var }}), labeller = label_wrap_gen(label_wrap_val), nrow = 1)
  }
  
  p
}

