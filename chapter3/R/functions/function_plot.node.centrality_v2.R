library(qgraph)
plot.node.centrality <- function(graph){
  
  
cen <- centrality_auto(graph)$node.centrality
cen$node <- row.names(cen)
cen$CC <- igraph::transitivity(graph, type = "local", isolates = "zero")
row.names(cen) <- NULL

cus_theme <-   theme_bw() + theme(
        panel.grid.major.x =  element_blank(),
        panel.grid.minor.x =  element_blank(),
#        panel.grid.major.y = element_line(color = "grey60", linetype = 3, size = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14))

p1 <- cen %>% ggplot(aes(y= Degree, x= node)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  coord_flip() +
  cus_theme + 
  ylab("Node degree") + 
  xlab("Node")    

p2 <-  cen %>% ggplot(aes(y = CC, x = node)) + 
  geom_bar(stat = "identity", fill ="dodgerblue4") +
  coord_flip() +
  cus_theme +theme(axis.title.y = element_blank()) +
  ylab("Clustering Coef") 

p3 <-  cen %>% ggplot(aes(y= Betweenness, x = node)) + 
  geom_bar(stat = "identity", fill = "firebrick4") +
  coord_flip() +
  cus_theme + theme(axis.title.y = element_blank()) +
  ylab("Betweenness")

plot_grid(p1, p2, p3, labels=c("A", "B", "C"), ncol = 3, nrow = 1 )

}