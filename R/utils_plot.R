# theme_rare <- function() {
#   
#   base_text <- 14
#   theme_bw() +
#     theme(
#       panel.grid = element_blank(),
#       plot.margin = margin(t=20, b= 20),
#       plot.title = element_text(hjust = 0.5, face = "bold", size = 16, vjust = 2),
#       axis.title.x = element_text(size = base_text, vjust = 0),
#       axis.title.y = element_text(size = base_text, hjust = 2, vjust = 5),
#       axis.text = element_text(size = base_text),
#       strip.text = element_text(size = base_text),
# 
#       legend.position = "right",
#       legend.title = element_blank()
#     )
# }


theme_rare <- function(rotate_x = FALSE, subtitle_color = "black") {
  
  # https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
  theme <- hrbrthemes::theme_ipsum_rc(
    axis_title_size = 14,
    axis_title_just = "cc",
    plot_margin = margin(5, 5, 5, 5),
    axis_text_size = 13
  ) +
    theme(
      legend.text = element_text(size = 14),
      plot.title = element_text(size = 22),
      plot.subtitle = element_text(size = 15, colour = subtitle_color)
    )
  
  if (rotate_x) {
    theme <- theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  theme
}


facet_strip_bigger <- function(gp, size){
  if(missing(gp)){
    #print("this function needs a facet_wrap ggplotly object")
  }
  if(missing(size)){
    #print("this function needs 'size' argument to be specified as integer. 80 will be introduced as default")
    size <- 80
  }
  
  n_facets <- c(1:length(gp[["x"]][["layout"]][["shapes"]]))
  
  for(i in n_facets){
    if(n_facets[i] %% 2 == 0){
      gp[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- + as.numeric(size)
      gp[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
    }
  }
  
  return(gp)
}

make_plotly <- function(.p){

  has_facets <- !"FacetNull" %in% class(.p$facet)
  
  .p <- .p %>% 
    plotly::ggplotly()
  
  if(has_facets){
    .p <- .p %>% 
      plotly::layout(title = list(y = 0.96,
                                  yanchor = "top",
                                  yef = "container"),
                     margin = list(t = 110) # gap between title and facets
      ) %>%
      facet_strip_bigger(size = 40) # size of stri
  }
  
  .p

}