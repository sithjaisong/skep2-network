
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
source("~/Documents/Github/skep2-network/chapter4/R/functions/function_plot.difnetwork.R")

load(file = "countryseasonnet.RData")

plot(country.seapair.IP.list <- as.data.frame(as.matrix(country.season.cor.mat[[1]][1:2]))


cor.mat <- list()
difnet <- list()

area <- c("Central_Plain", "Odisha", "Red_River_Delta", "Tamil_ Nadu", "West_Java")
#i <- 1
for (i in 1:5) {
cor.mat[[2*(i -1) +1]] <- country.season.cor.mat[[2*(i -1) +1]] # dry season
cor.mat[[2*(i -1) +2]] <- country.season.cor.mat[[2*(i -1) +2]] # wet season

# = Dry season
cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$p.value > 0.05 ] <- 0
cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$rho < 0 ] <- 0
g1 <- graph.edgelist(as.matrix(cor.mat[[2*(i-1) + 1]][1:2]), directed = FALSE)
E(g1)$weight <- as.matrix(cor.mat[[2*(i-1) + 1]][,3])
E(g1)$color <- adjustcolor("black", alpha.f = .8)
E(g1)$frame.color <- adjustcolor("black", alpha.f = .8)

# = Wet season
cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[2*(i-1) + 2]]$p.value > 0.05 ] <- 0
cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[ 2*(i-1)+2]]$rho < 0 ] <- 0
g2 <- graph.edgelist(as.matrix(cor.mat[[2*(i-1) + 2]][1:2]), directed = FALSE)
E(g2)$weight <- as.matrix(cor.mat[[2*(i-1) + 2]][,3])
E(g2)$color <- adjustcolor("black", alpha.f = .8)
E(g2)$frame.color <- adjustcolor("black", alpha.f = .8)

# = Differential network
adj.mat1 <- as.matrix(as_adjacency_matrix(g1, attr = "weight"))
adj.mat2 <- as.matrix(as_adjacency_matrix(g2, attr = "weight"))
res <- diff.corr(adj.mat1 , adj.mat2)
diff_comb <- left_join(pair.IP.list, res[c("var1", "var2", "r1", "r2")])
diff_comb$index <- ifelse(abs(diff_comb$r1) - abs(diff_comb$r2) > 0, 1, -1)
gdif <- graph.edgelist(as.matrix(diff_comb[1:2]), directed = FALSE)
diff_comb[is.na(diff_comb)] <- 0
E(gdif)$weight <- as.matrix(diff_comb[,5])
difnet[[i]] <- plot_difnetwork(gdif)

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
re_difnet <- list()
for (i in 1:5) {
E(difnet[[i]])$weight <- (E(difnet[[i]])$weight)^2
re_difnet[[i]] <- delete.edges(difnet[[i]], which(E(difnet[[i]])$weight < 1))
re_difnet[[i]] <- delete.vertices(re_difnet[[i]], which(igraph::degree(re_difnet[[i]]) < 1))
pdf(file = paste("./chapter4/results/new.dif", area[i], ".pdf", sep =""), width = 9, height = 9)
plot(re_difnet[[i]], vertex.size = 12, main = paste("Differential network of survey data in", area[i], sep = " "))
dev.off()
}

names(re_difnet) <- c("CP","OD", "RR", "TM", "WJ") 
                            
re_difnet.node.stat.list <- sapply(re_difnet, node_stat, simplify = FALSE, 
                         USE.NAMES = TRUE)

# combine the node statistic from each network of country by season
re_difnet.node.stat.list.node.df <- do.call(rbind, re_difnet.node.stat.list)

# add coolumn to indicate the source of node stat
re_difnet.node.stat.list.node.df$country_season <- gsub("\\..*", "", row.names(re_difnet.node.stat.list.node.df))

# delete row name
row.names(re_difnet.node.stat.list.node.df) <- NULL

# reorder the colummn by moving column named country_season into the first
re_difnet.node.stat.list.node.df <- re_difnet.node.stat.list.node.df %>% dplyr::select(country_season, everything())



node_stat(re_difnet[[1]])
node_stat(re_difnet[[2]])
node_stat(re_difnet[[3]])
node_stat(re_difnet[[4]])
node_stat(re_difnet[[5]])
