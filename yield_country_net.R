#different network of yield in each country

library(dplyr)
library(ggplot2)
library(igraph)
library(dplyr)
library(DiffCorr)
library(graph)
library(QuACN)
library(readr)
library(cowplot)

#survey <- read_csv(file = "/Users/sithjaisong/Google Drive/4.SKEP2ProjectData/Farm_survey/SKEP2_survey.csv")
survey <- read_csv(file = "SKEP2_survey.csv")


yield_injuries <- survey %>% dplyr::select(prod_env, year, season, yield, RT, RH, SS, WH, PM, RB, DH, DP, FS, NB, SR, RTH, LF, LM, LS, WM, 
                                                   BLB, BLS, BS, LB, NBS, RS, BB, HB, GS, RGS, RTG, SHB, SHR, WA, WB) %>% 
  transform(prod_env = as.factor(prod_env),
            year = as.factor(year),
            season = as.factor(season))

# define yield level
yield_injuries <- as.data.frame(yield_injuries)
yield_injuries$yield_level <- "na"
yield_injuries[is.na(yield_injuries) ] <- 0
yield_injuries[yield_injuries$yield > 6000, ]$yield_level <- "high"
yield_injuries[yield_injuries$yield >= 4000 & yield_injuries$yield <= 6000,]$yield_level <- "medium"
yield_injuries[yield_injuries$yield < 4000,]$yield_level <- "low"

yield_injuries$yield_level <- as.factor(as.character(yield_injuries$yield_level))
yield_injuries$yield_level <- factor(yield_injuries$yield_level, levels = c("low", "medium", "high"))

levels(yield_injuries$prod_env)[levels(yield_injuries$prod_env) == "Central_Plain"] <- "Central Plain"
levels(yield_injuries$prod_env)[levels(yield_injuries$prod_env) == "Red_river_delta"] <- "Red River Delta"
levels(yield_injuries$prod_env)[levels(yield_injuries$prod_env) == "Tamil_Nadu"] <- "Tamil Nadu"
levels(yield_injuries$prod_env)[levels(yield_injuries$prod_env) == "West_Java"] <- "West Java"


# plot histogram
yield_level_bar <- yield_injuries %>% ggplot(aes(x = yield_level, fill = yield_level)) + geom_bar(position = "dodge") + scale_fill_grey(guide = FALSE) + facet_grid(~prod_env) + xlab("Yield level") + ylab("No of farmers' fields") +
  theme(axis.text=element_text(size=10), axis.title = element_text(size = 12), strip.text = element_text(size = 12))
ggsave(yield_level_bar, filename = "yield_level_bar.pdf", width = 15, height = 5)

# select the data set
# for yield diff un CP will high vs medium

# for yield diff of OD will low vs medium

# for yield diff of RR will high vs medim

# for yield diff of TM will low vs medium

# for yield diff of WJ will high vs medium


CP.high <- yield_injuries %>% filter(prod_env == "Central Plain" & yield_level == "high")
CP.med <- yield_injuries %>% filter(prod_env == "Central Plain" & yield_level == "medium")
OD.med <- yield_injuries %>% filter(prod_env == "Odisha" & yield_level == "medium")
OD.low <- yield_injuries %>% filter(prod_env == "Odisha" & yield_level == "low")
RR.high <- yield_injuries %>% filter(prod_env == "Red River Delta" & yield_level == "high")
RR.med <- yield_injuries %>% filter(prod_env == "Red River Delta" & yield_level == "medium")
TM.med <- yield_injuries %>% filter(prod_env == "Tamil Nadu" & yield_level == "medium")
TM.low <- yield_injuries %>% filter(prod_env == "Tamil Nadu" & yield_level == "low")
WJ.high <- yield_injuries %>% filter(prod_env == "West Java" & yield_level == "high")
WJ.med <- yield_injuries %>% filter(prod_env == "West Java" & yield_level == "medium")

yield.country.dataset <- list(CP.med, CP.high, OD.low, OD.med, RR.med, RR.high, TM.med, TM.low, WJ.med, WJ.high)

#source("~/Documents/Github/skep2-network/chapter3/R/functions/function_cooc_table.R")
source("chapter3/R/functions/function_cooc_table.R")

yield.country.cor.mat <- list()

for (i in 1:length(yield.country.dataset)) {
  
  # select out the country and season varible
  temp <- yield.country.dataset[[i]][!names(yield.country.dataset[[i]]) %in% c("prod_env", "year", "season", "yield", "yield_level")]
  
  # generate the corrlation matrix 
  
  yield.country.cor.mat[[i]] <- cooc_table(temp)
}

#source("~/Documents/Github/skep2-network/chapter3/R/functions/function_plot_network.R")
source("chapter3/R/functions/function_plot_network.R")
yield.country.net <- list()

for (i in 1:length(yield.country.cor.mat)) {
  
  # keep the correlation coefficient at p.value < 0.05
  cut.table <- yield.country.cor.mat[[i]] %>% filter(p.value < 0.05) %>% filter(rho > 0)
  
  # construct the netwotk object 
  yield.country.net[[i]] <- plot_network(cut.table)
  
}

names(yield.country.net) <- c("CP_med", "CP_high",  "OR_low", "OR_med","RR_med", "RR_high", "TM_low", "TM_med", "WJ_med", "WJ_high")

#for(i in 1: length(yield.country.net)){
  #pdf(file = paste("./chapter4/results/yield.network.", names(yield.country.net)[i] ,".pdf", sep =""), width = 15, height = 12)
  #plot(yield.country.net[[i]])
  #dev.off()
#}


# ========== The network properties display ============= # 
# plot dot plot

#source("~/Documents/Github/skep2-network/chapter3/R/functions/function_plot.node.centrality.R") 

source("chapter3/R/functions/function_plot.node.centrality.R") 



for(i in 1:length(yield.country.net)){
  dotgraph <- plot.node.centrality(yield.country.net[[i]]) 
  ggsave(dotgraph, file = paste("./chapter4/results/yield_nodeprop_new", names(yield.country.net)[i] ,".pdf", sep =""), width = 15, height = 12)
}



# list the dataset by country season

source("~/Documents/Github/skep2-network/chapter4/R/functions/function_compcorr.R")
source("~/Documents/Github/skep2-network/chapter4/R/functions/function_diff.corr.R")

source("~/Documents/Github/skep2-network/chapter4/R/functions/function_plot.network.R")
source("~/Documents/Github/skep2-network/chapter4/R/functions/function_plot.yield.difnetwork.R")


pair.IP.list <- as.data.frame(as.matrix(yield.country.cor.mat[[1]][1:2]))


cor.mat <- list()
yielddifnet <- list()

#area <- c("Central_Plain", "Odisha", "Red_River_Delta", "Tamil_ Nadu", "West_Java")
#i <- 1
for (i in 1:5) {
  cor.mat[[2*(i -1) +1]] <- yield.country.cor.mat[[2*(i -1) +1]] 
  cor.mat[[2*(i -1) +2]] <- yield.country.cor.mat[[2*(i -1) +2]]
  
  # = Dry season
  cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$p.value > 0.05 ] <- 0
  cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$rho < 0 ] <- 0
  g1 <- graph.edgelist(as.matrix(cor.mat[[2*(i-1) + 1]][1:2]), directed = FALSE)
  E(g1)$weight <- as.matrix(cor.mat[[2*(i-1) + 1]][,3])
  E(g1)$color <- adjustcolor("blue", alpha.f = .8)
  E(g1)$frame.color <- adjustcolor("blue", alpha.f = .8)
  
  # = Wet season
  cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[2*(i-1) + 2]]$p.value > 0.05 ] <- 0
  cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[ 2*(i-1)+2]]$rho < 0 ] <- 0
  g2 <- graph.edgelist(as.matrix(cor.mat[[2*(i-1) + 2]][1:2]), directed = FALSE)
  E(g2)$weight <- as.matrix(cor.mat[[2*(i-1) + 2]][,3])
  E(g2)$color <- adjustcolor("blue", alpha.f = .8)
  E(g2)$frame.color <- adjustcolor("blue", alpha.f = .8)
  
  # = Differential network
  adj.mat1 <- as.matrix(as_adjacency_matrix(g1, attr = "weight"))
  adj.mat2 <- as.matrix(as_adjacency_matrix(g2, attr = "weight"))
  res <- diff.corr(adj.mat1 , adj.mat2)
  diff_comb <- left_join(pair.IP.list, res[c("var1", "var2", "r1", "r2")])
  diff_comb$index <- ifelse(abs(diff_comb$r1) - abs(diff_comb$r2) > 0, 1, -1)
  gdif <- graph.edgelist(as.matrix(diff_comb[1:2]), directed = FALSE)
  diff_comb[is.na(diff_comb)] <- 0
  E(gdif)$weight <- as.matrix(diff_comb[,5])
  yielddifnet[[i]] <- plot_yield.difnetwork(gdif)
  
  # ============================================================================
  #l <-layout.fruchterman.reingold(g1) 
  #png(file = paste("./chapter4/results/yield_comp.difnet", area[i], ".png", sep =""), width = 1500, res = 100)
  #layout(t(1:3))
  #plot(plot_network(g1), layout = l , main = "Low season network") 
  #plot(difnet[[i]], layout = l, main = paste("Differential network of survey data in", area[i], sep = " "))
  #plot(plot_network(g2), layout = l, main = "High yield level network")
  #dev.off()
}

source("~/Documents/Github/skep2-network/chapter3/R/functions/function_node_net_stat.R")
re_yielddifnet <- list()

for (i in 1:5) {
  
  E(yielddifnet[[i]])[weight < 0]$weight <-0
  E(yielddifnet[[i]])$weight <- (E(yielddifnet[[i]])$weight)^2
  re_yielddifnet[[i]] <- delete.edges(yielddifnet[[i]], which(E(yielddifnet[[i]])$weight < 1))
  re_yielddifnet[[i]] <- delete.vertices(re_yielddifnet[[i]], which(igraph::degree(re_yielddifnet[[i]]) < 1))
 pdf(file = paste("./chapter4/results/new.yield_dif", area[i], ".pdf", sep =""), width = 9, height = 9)
  plot(re_yielddifnet[[i]], vertex.size = 12, main = paste("Differential network of survey data in", area[i], sep = " "))
  dev.off()
}

names(re_yielddifnet) <- c("CP","OD", "RR", "TM", "WJ") 

re_yielddifnet.node.stat.list <- sapply(re_yielddifnet, node_stat, simplify = FALSE, 
                                   USE.NAMES = TRUE)

# combine the node statistic from each network of country by season
re_yielddifnet.node.stat.list.node.df <- do.call(rbind, re_yielddifnet.node.stat.list)

# add coolumn to indicate the source of node stat
re_yielddifnet.node.stat.list.node.df$country_season <- gsub("\\..*", "", row.names(re_yielddifnet.node.stat.list.node.df))

# delete row name
row.names(re_yielddifnet.node.stat.list.node.df) <- NULL

# reorder the colummn by moving column named country_season into the first
re_yielddifnet.node.stat.list.node.df <- re_yielddifnet.node.stat.list.node.df %>% dplyr::select(country_season, everything())

#re_yielddifnet.node.stat.list.node.df

 node_stat(re_difnet[[1]])
 node_stat(re_difnet[[2]])
 node_stat(re_difnet[[3]])
 node_stat(re_difnet[[4]])
 node_stat(re_difnet[[5]])

 for(i in 1:length(re_difnet)){
   dotgraph <- plot.node.centrality(re_yielddifnet[[i]]) 
   ggsave(dotgraph, file = paste("./chapter4/results/yield_dif_newnodeprop", area[i] ,".pdf", sep =""), width = 15, height = 8)
 }
 
 
 # = discussion

 # =    CP, Thaialnd
 CP.yield.data <- yield_injuries %>% filter(prod_env == "Central_Plain") %>% filter(!yield_level == "low") %>% dplyr::select(-c(prod_env, year, season, yield)) %>% as.data.frame()
 #ylim1 = boxplot.stats(CP.yield.data$BS)$stats[c(1, 5)]
# m.CP.yield.data <- melt(CP.yield.data, id.vars = "yield_level")
 
 CP.varnames <- c("BS", "SHB", "DP")
 CP.unit.value <- c("%dsu", "%incidence", "%incidence")
 
 CP.out <- list()
 for(i in 1:length(CP.varnames)) {
#   y.data <- m.CP.yield.data %>% filter(variable == CP.varnames[i]) 
#   ylim1 <- boxplot.stats(y.data$value)$stats[c(1, 5)]
   CP.p <- m.CP.yield.data %>% filter(variable == CP.varnames[i]) %>% 
     ggplot(aes(y = value, x = variable, fill = yield_level)) + 
     geom_violin(fill = "khaki2") +
     geom_boxplot(fill = "white", width = 0.25) + 
#    scale_y_continuous(limits = ylim1)  + 
     facet_grid(yield_level~.) +
     xlab(paste(CP.varnames[i])) +
     ylab(paste(CP.unit.value[i])) +
     theme(legend.position = "none", axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(angle = 0))
   
   CP.p <- CP.p + coord_flip()
   CP.out[[i]] <- CP.p
 }
CP.yield.box <- grid.arrange(CP.out[[1]], CP.out[[2]], CP.out[[3]], ncol = 1, nrow = 3)

ggsave(CP.yield.box, file = "./chapter4/results/CP.yield.box.pdf")


 # = OD, India
 OD.yield.data <- yield_injuries %>% filter(prod_env == "Odisha") %>% dplyr::select(-c(prod_env, year, season, yield)) %>% as.data.frame()
 OD.yield.data$yield_level <- as.factor(as.character(OD.yield.data$yield_level))
 m.OD.yield.data <- melt(OD.yield.data, id.vars = "yield_level")
 
 OD.varnames <- c("FS", "SS", "WH")
 OD.unit.value <- c("%incidence", "%incidence", "%incidence")
 
 OD.out <- list()
 for(i in 1:length(OD.varnames)) {
   #y.data <- m.OD.yield.data %>% filter(variable == OD.varnames[i]) 
   #ylim1 <- boxplot.stats(y.data$value)$stats[c(1, 5)]
   OD.p <- m.OD.yield.data %>% filter(variable == OD.varnames[i]) %>% 
     ggplot(aes(y = value, x = variable, fill = yield_level)) + 
     geom_violin(fill = "khaki2") +
     geom_boxplot(fill = "white", width = 0.25) + 
     facet_grid(yield_level~.) +
     xlab(paste(OD.varnames[i])) +
     ylab(paste(OD.unit.value[i])) +
     theme(legend.position = "none", axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(angle = 0))
   
   OD.p <- OD.p + coord_flip()
   OD.out[[i]] <- OD.p
 }
 OD.yield.box <- grid.arrange(OD.out[[1]], OD.out[[2]], OD.out[[3]], ncol = 1, nrow = 3)
 
 ggsave(OD.yield.box, file = "./chapter4/results/OD.yield.box.pdf")
 
 # =================
 
 RR.yield.data <- yield_injuries %>% filter(prod_env == "Red_river_delta") %>% filter(!yield_level == "low") %>% dplyr::select(-c(prod_env, year, season, yield)) %>% as.data.frame()
 RR.yield.data$yield_level <- as.factor(as.character(RR.yield.data$yield_level))
 m.RR.yield.data <- melt(RR.yield.data, id.vars = "yield_level")
 
 RR.varnames <- c("WH", "WB", "GS")
 RR.unit.value <- c("%incidence", "%dsu", "%dus")
 
 RR.out <- list()
 for(i in 1:length(RR.varnames)) {
   #y.data <- m.RR.yield.data %>% filter(variable == RR.varnames[i]) 
   #ylim1 <- boxplot.stats(y.data$value)$stats[c(1, 5)]
   RR.p <- m.RR.yield.data %>% filter(variable == RR.varnames[i]) %>% 
     ggplot(aes(y = value, x = variable, fill = yield_level)) + 
     geom_violin(fill = "khaki2") +
     geom_boxplot(fill = "white", width = 0.25) + 
     #     scale_y_continuous(limits = ylim1)  + 
     facet_grid(yield_level~.) +
     xlab(paste(RR.varnames[i])) +
     ylab(paste(RR.unit.value[i])) +
     theme(legend.position = "none", axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(angle = 0))
   
   RR.p <- RR.p + coord_flip()
   RR.out[[i]] <- RR.p
 }
 RR.yield.box <- grid.arrange(RR.out[[1]], RR.out[[2]], RR.out[[3]], ncol = 1, nrow = 3)
 
 ggsave(RR.yield.box, file = "./chapter4/results/RR.yield.box.pdf")
 
 #================
 
 
 TM.yield.data <- yield_injuries %>% filter(prod_env == "Tamil_Nadu")  %>% dplyr::select(-c(prod_env, year, season, yield)) %>% as.data.frame()
 TM.yield.data$yield_level <- as.factor(as.character(TM.yield.data$yield_level))
 m.TM.yield.data <- melt(TM.yield.data, id.vars = "yield_level")
 
 TM.varnames <- c("SHB","WH", "WM")
 TM.unit.value <-c("%incidence", "%incidence", "%dsu")
 TM.out <- list()
 for(i in 1:length(TM.varnames)) {
   #y.data <- m.TM.yield.data %>% filter(variable == TM.varnames[i]) 
   #ylim1 <- boxplot.stats(y.data$value)$stats[c(1, 5)]
   TM.p <- m.TM.yield.data %>% filter(variable == TM.varnames[i]) %>% 
     ggplot(aes(y = value, x = variable, fill = yield_level)) + 
     geom_violin(fill = "khaki2") +
     geom_boxplot(fill = "white", width = 0.25) + 
     #     scale_y_continuous(limits = ylim1)  + 
     facet_grid(yield_level~.) +
     xlab(paste(TM.varnames[i])) +
     ylab(paste(TM.unit.value[i])) +
     theme(legend.position = "none", axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(angle = 0))
   
   TM.p <- TM.p + coord_flip()
   TM.out[[i]] <- TM.p
 }
 TM.yield.box <- grid.arrange(TM.out[[1]], TM.out[[2]], TM.out[[3]], ncol = 1, nrow = 3)
 
 ggsave(TM.yield.box, file = "./chapter4/results/TM.yield.box.pdf")
 
 #==============
 
 
 WJ.yield.data <- yield_injuries %>% filter(prod_env == "West_Java") %>% filter(!yield_level == "low") %>% dplyr::select(-c(prod_env, year, season, yield)) %>% as.data.frame()
 WJ.yield.data$yield_level <- as.factor(as.character(WJ.yield.data$yield_level))
 WJ.yield.data$yield_level <- factor(WJ.yield.data$yield_level, levels = c("medium", "high"))
 
 m.WJ.yield.data <- melt(WJ.yield.data, id.vars = "yield_level")
 
 WJ.varnames <- c("RTG", "RS", "RT")
 WJ.unit.value <- c("%incidence", "%dsu", "%incidence")
 
 WJ.out <- list()
 for(i in 1:length(WJ.varnames)) {
  # y.data <- m.WJ.yield.data %>% filter(variable == WJ.varnames[i]) 
  # ylim1 <- boxplot.stats(y.data$value)$stats[c(1, 5)]
   WJ.p <- m.WJ.yield.data %>% filter(variable == WJ.varnames[i]) %>% 
     ggplot(aes(y = value, x = variable, fill = yield_level)) + 
     geom_violin(fill = "khaki2") +
     geom_boxplot(fill = "white", width = 0.25) + 
     #     scale_y_continuous(limits = ylim1)  + 
     facet_grid(yield_level~.) +
     xlab(paste(WJ.varnames[i])) +
     ylab(paste(WJ.unit.value[i])) +
     theme(legend.position = "none", axis.ticks = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(angle = 0))
   
   WJ.p <- WJ.p + coord_flip()
   WJ.out[[i]] <- WJ.p
 }
 WJ.yield.box <- grid.arrange(WJ.out[[1]], WJ.out[[2]], WJ.out[[3]], ncol = 1, nrow = 3)
 
 ggsave(WJ.yield.box, file = "./chapter4/results/WJ.yield.box.pdf")

 
 #=========
 
yield_injuries %>% filter(season ==  "wet_season") %>% ggplot(aes(x= prod_env, y = yield)) + geom_boxplot()
 
