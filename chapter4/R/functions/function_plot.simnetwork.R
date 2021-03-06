plot_simnetwork <- function(net){
  
  V(net)$color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$frame.color <- adjustcolor("khaki2", alpha.f = .8)
  
  V(net)$shape <- "circle"
  
  V(net)$size <- 25
  
  V(net)$label.color <- "black"
  
  V(net)$label.font <- 2
  
  V(net)$label.family <- "Helvetica"
  
  V(net)$label.cex <- 1.0
  
  E(net)$color <- adjustcolor("purple", alpha.f = .8)
  
  E(net)$width <- abs(E(net)$weight)*5
  
  
  return(net)
}
