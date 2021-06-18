prep_q07_people <- function(.data){
  gender <- prep_data_for_plot(.data, focus_var = `7_gender`, bar_column = "F")
  
  .data$age_cut <- cut(.data$`7_age`, 
                       c(0, 5, 20, 50, 10000), 
                       labels = c("<5", "5-<20", "20-<50", "50+"),
                       include.lowest = TRUE,
                       right = FALSE)
  
  age <- prep_data_for_plot(.data, focus_var = `age_cut`, type = "stacked")
  
  list(
    gender = gender,
    age = age
  )
}



plot_q07_people <- function(.data, ...){

  .data_plot <- prep_q07_people(.data)
  
  browser()
  
  iris1 <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
    geom_boxplot() + theme_bw()
  
  iris2 <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
    geom_density(alpha = 0.7) + theme_bw() +
    theme(legend.position = c(0.8, 0.8))
  
  cowplot::plot_grid(iris1, iris2, labels = "AUTO")
}