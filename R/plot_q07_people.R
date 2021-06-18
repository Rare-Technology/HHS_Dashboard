prep_q07_people <- function(.data){
  gender <- prep_data_for_plot(.data, focus_var = `7_gender`, bar_column = "F")
  
  .data$age_cut <- cut(.data$`7_age`, 
                       c(0, 5, 20, 50, 10000), 
                       labels = c("<5", "5-<20", "20-<50", "50+"),
                       include.lowest = TRUE,
                       right = FALSE)
  
  age <- prep_data_for_plot(.data, focus_var = `age_cut`, type = "stacked")
  
  relationship <- prep_data_for_plot(.data, focus_var = `7_relationship`, type = "stacked")
  education <- prep_data_for_plot(.data, focus_var = `7_education`, type = "stacked")
  
  list(
    gender = gender,
    age = age,
    relationship = relationship,
    education = education
  )
}



plot_q07_people <- function(.data, ...){

  .data_plot <- prep_q07_people(.data)
  

  p_gender <- plot_horiz_bar(
    .data_plot$gender,
    title = "Percentage female",
    limits = NULL
  )
  
  p_age <- plot_horiz_bar(
    .data_plot$age,
    title = "Age",
    type = "stacked"
  )
  
  p_relationship <- plot_horiz_bar(
    .data_plot$relationship,
    title = "Relationship",
    type = "stacked"
  )
  
  p_education <- plot_horiz_bar(
    .data_plot$education,
    title = "Education",
    type = "stacked"
  )
  
  

  
  cowplot::plot_grid(p_gender, p_relationship, p_age, p_education)
}