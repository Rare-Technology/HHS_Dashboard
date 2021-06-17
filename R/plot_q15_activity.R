# x<- hhs_data %>% 
#   select(`15_activity`)
# 
# tally_categorical(hhs_data, `15_activity`)
# 
# 
# tally_categorical <- function(.data, .var, unnest = FALSE){
#   browser()
#   
#   dat <- select(hhs_data, {{ .var }})
#   
#   if(unnest  == TRUE){
#     
#   }
#   
# }
# 
# 
# prep_q15_activity <- function(.data){
# 
#   prep_data_for_plot(hhs_data, `15_activity`, unnest = TRUE)
# 
# }
# 
# 
# plot_15_activity <- function(.data, ...) {
# 
#   
#   .data_plot <- prep_data_for_plot(
#     .data,
#     `26_fishing_income_save`,
#     include_summary_line = FALSE,
#     type = "bar",
#     bar_column = `1`
#   )
#   
#   plot_horiz_bar(
#     .data_plot,
#     title = "Proportion of househols with enough income to save"
#   )
# }

