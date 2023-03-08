prep_q26_items_owned <- function(.data){
  Q26_cols <- stringr::str_subset(names(hhs_data), "^26[a-z]_item_.*_no")
  hhs_Q26 <- .data %>% 
    dplyr::select(maa, Q26_cols) %>% 
    dplyr::mutate(
      dplyr::across(
        Q26_cols,
        ~ dplyr::case_when(
          .x == "Not Answered" ~ as.double(NA),
          TRUE ~ as.double(.x)
        )
      )
    )
  
  #replace NA with 0s
  hhs_Q26[is.na(hhs_Q26)] <- 0
  hhs_Q26_01 <- hhs_Q26 %>% 
    dplyr::mutate(
      dplyr::across(
        Q26_cols,
        ~ dplyr::case_when(
          .x > 0 ~ 1,
          TRUE ~ 0
        )
      )
    ) %>% 
    dplyr::rename(ma_name = maa)
  
  assets_prop <- hhs_Q26_01 %>% 
    dplyr::group_by(ma_name) %>% 
    dplyr::summarize(
      dplyr::across(
        Q26_cols,
        ~ round(sum(.x) / length(.x) * 100, 1)
      )
    )
  
  #Rename columns
  colnames(assets_prop) <-
    c(
      "MA name",
      "Radio",
      "TV",
      "Satellite",
      "Phone",
      "Washing machine",
      "Generator",
      "Fridge",
      "Motorboat",
      "Sailboat",
      "Bicycle",
      "Motorcycle",
      "Car",
      "Internet",
      "Other"
    )
  
  df <- tidyr::pivot_longer(assets_prop, cols=colnames(assets_prop)[-1])
  df <- df %>% dplyr::rename(Item=name, Proportion=value)
  df$Item <- factor(
    df$Item,
    levels = c(
      'Internet', 'Phone', 'Radio', 'Satellite', 'TV', 'Fridge', 'Generator',
      'Washing machine', 'Motorboat', 'Sailboat', 'Bicycle', 'Car', 'Motorcycle', 'Other'
    )
  )
  df$color <- map_color(df$Proportion)
  
  df
}

map_color <- Vectorize(function(x) {
  if (x >= 50) {
    'black'
  } else {
    'white'
  }
})

plot_q26_items_owned <- function(.data, ...){
  .data_plot <- prep_q26_items_owned(.data)
  
  title <- 'Assets owned by households'
  title <- stringr::str_wrap(title, width = 65)
  subtitle <- 'Proportion of households (%)'
  
  .data_plot <- .data_plot %>% 
    dplyr::mutate(
      `MA name` := factor(`MA name`)
    )
  
  .data_plot <- .data_plot %>% 
    dplyr::mutate(
      `MA name` := forcats::fct_rev(`MA name`)
    )
  
  
  p <- ggplot(data = .data_plot,
       aes(x = Item, y = `MA name`, fill = Proportion)) +
  geom_tile() + 
  geom_text(aes(label = Proportion, color=Proportion >= 50),
            show.legend = c(TRUE, FALSE)) +
  scale_fill_viridis_c(option='magma') +
  scale_color_manual(values=c('white', 'black')) +
  labs(title = title, subtitle=subtitle) +
  theme_rare() +
  theme(
    axis.text.x = element_text(angle=30, hjust=1),
    axis.ticks.x = element_line(),
    panel.grid.major.x = element_blank(),
    axis.title.y = element_blank()
  )
  
  result <- list(
    plot = p,
    data = .data_plot
  )
}