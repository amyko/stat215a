# Useful functions

Normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) 
}

# round to nearest given base
roundToNearestX <- function(x, base){
  
  return(base * round(x / base))
  
}


blankTheme <- function(){
  
  return(theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()))
  
}