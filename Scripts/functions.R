
theme_walnut <- function(){
  theme_linedraw()+
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          legend.key.width = unit(15, "mm"),
          legend.key = element_rect(colour =  "transparent", fill = "white"))
  
}
