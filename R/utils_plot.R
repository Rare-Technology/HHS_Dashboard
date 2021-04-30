theme_rare <- function(){
  theme_bw() +
    theme(panel.grid = element_blank(),
          plot.margin = margin(t=48, l=24, b=48, r= 24),
          plot.title = element_text(hjust=0.5, face = 'bold', size = 12, vjust = 2),
          axis.title.x = element_text(size=12, vjust = 0),
          axis.title.y = element_text(size=12, hjust = 2, vjust = 5),
          axis.text  = element_text(size=12),
          strip.text = element_text(size=12),
          legend.position = "none",
          legend.title = element_blank())
}