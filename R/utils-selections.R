get_levelnames <- function(.data, .namevar, subnational = NULL, localgov = NULL){

    if(!is.null(subnational)) .data <- filter(.data, level1_name %in% subnational)
    if(!is.null(localgov)) .data <- filter(.data, level2_name %in% localgov)
    
    dplyr::pull(.data, .namevar) %>% 
      unique() %>% 
      sort()
}

