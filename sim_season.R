
library(igraph)
library(dplyr)
library(DiffCorr)
library(graph)
library(QuACN)
library(readr)
# list the dataset by country season

source("~/Documents/Github/skep2-network/chapter4/R/functions/function_compcorr.R")
source("~/Documents/Github/skep2-network/chapter4/R/functions/function_diff.corr.R")

source("~/Documents/Github/skep2-network/chapter4/R/functions/function_plot.network.R")
source("~/Documents/Github/skep2-network/chapter4/R/functions/function_plot.simnetwork.R")

load(file = "countryseasonnet.RData")

pair.IP.list <- as.data.frame(as.matrix(country.season.cor.mat[[1]][1:2]))


cor.mat <- list()
simnet <- list()

area <- c("Central_Plain", "Odisha", "Red_River_Delta", "Tamil_ Nadu", "West_Java")
#i <- 1
for (i in 1:5) {
  cor.mat[[2*(i -1) +1]] <- country.season.cor.mat[[2*(i -1) +1]] # dry season
  cor.mat[[2*(i -1) +2]] <- country.season.cor.mat[[2*(i -1) +2]] # wet season
  
  # = Dry season
  cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$p.value > 0.05 ] <- 0
  cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$rho < 0 ] <- 0
  cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$rho > 0 ] <- 1
  sim_dry <- cor.mat[[ 2*(i-1) + 1]] %>% dplyr::select(var1, var2, rho) %>% mutate(dry_occur = rho)
  sim_dry$rho <- NULL
  # = Wet season
  cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[2*(i-1) + 2]]$p.value > 0.05 ] <- 0
  cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[ 2*(i-1)+2]]$rho < 0 ] <- 0
  cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[ 2*(i-1)+2]]$rho > 0 ] <- 1
  sim_wet <- cor.mat[[ 2*(i-1) + 2]] %>% dplyr::select(var1, var2, rho) %>% mutate(wet_occur = rho)
  sim_wet$rho <- NULL
  # similarity analysis
  
  sim_dry_wet <- left_join(sim_dry, sim_wet, by = c("var1", "var2"))
  sim_dry_wet$sim <- 0
  sim_dry_wet$sim <- ifelse(sim_dry_wet$dry_occur == 1 & sim_dry_wet$wet_occur == 1, 1, 0)
  
  # = Differential network
  gsim <- graph.edgelist(as.matrix(sim_dry_wet[1:2]), directed = FALSE)
  E(gsim)$weight <- as.matrix(sim_dry_wet[,5])
  simnet[[i]] <- plot_sim.network(gsim)
  #plot(simnet)
  # ============================================================================
  # l <-layout.fruchterman.reingold(g1) 
  # png(file = paste("difnet", area[i], ".png", sep =""), width = 1500, res = 100)
  # layout(t(1:3))
  # plot(plot_network(g1), layout = l , main = "Dry season network") 
  # plot(difnet[[i]], layout = l, main = paste("Differential network of survey data in", area[i], sep = " "))
  # plot(plot_network(g2), layout = l, main = "Wet season network")
  # dev.off()
}

source("~/Documents/Github/skep2-network/chapter3/R/functions/function_node_net_stat.R")
re_simnet <- list()
for (i in 1:5) {
  re_simnet[[i]] <- delete.edges(simnet[[i]], which(E(simnet[[i]])$weight < 1))
  re_simnet[[i]] <- delete.vertices(re_simnet[[i]], which(igraph::degree(re_simnet[[i]]) < 1))
  pdf(file = paste("./chapter4/results/sim", area[i], ".pdf", sep =""), width = 9, height = 9)
  plot(re_simnet[[i]], vertex.size = 12, main = paste("Similarity network of survey data in", area[i], sep = " "))
  dev.off()
}

node_stat(re_simnet[[1]])
node_stat(re_simnet[[2]])
node_stat(re_simnet[[3]])
node_stat(re_simnet[[4]])
node_stat(re_simnet[[5]])


re_comnet <- list()
for (i in 1:5){
  
complex_net <- pair.IP.list
complex_net$sim <- E(simnet[[i]])$weight
complex_net$dif <- E(difnet[[i]])$weight
complex_net$complex <- 0
complex_net$complex <- ifelse(complex_net$sim == 1, 2,
                              ifelse(complex_net$sim == 0 & complex_net$dif == 1, 1,
                                     ifelse(complex_net$sim == 0 & complex_net$dif == -1, -1, 0)))
  gcomplex <- graph.edgelist(as.matrix(complex_net[1:2]), directed = FALSE)
  E(gcomplex)$weight <- as.matrix(complex_net[,5])
  comnet <- plot_complexnetwork(gcomplex)
  re_comnet[[i]] <- delete.edges(comnet, which(E(comnet)$weight == 0))
  re_comnet[[i]] <- delete.vertices(re_comnet[[i]], which(igraph::degree(re_comnet[[i]]) < 1))
  pdf(file = paste("./chapter4/results/complex", area[i], ".pdf", sep =""), width = 9, height = 9)
  plot(re_comnet[[i]], vertex.size = 12, main = paste("Similarity network of survey data in", area[i], sep = " "))
  dev.off()
}
