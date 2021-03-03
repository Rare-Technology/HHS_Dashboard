get_ma_percent <- function(.fulldata, .partdata, newname = "percent"){

  ma_count <- .fulldata %>% 
    dplyr::count(ma_name)
  
  ma_count_hhs <- .partdata %>% 
    dplyr::count(ma_name)
  
  result <- dplyr::full_join(ma_count, ma_count_hhs, by = "ma_name", suffix = c("_full", "_part")) %>% 
    mutate(
      percent = round(100* n_part/n_full, 1)
    ) %>% 
    select(-n_part, -n_full)
  
  names(result)[names(result) == "percent"] <- newname
  result
}