# continue from the chapter 3
library(igraph)
library(dplyr)
library(DiffCorr)
library(graph)
library(QuACN)
library(readr)

#load(file = "~/Google Drive/surveySKEP1/chapter3netdata.RData")

##### 1.Load survey data  ######
# The data are in the shared folder named "surveySKEP1"
# You can download or folk from Github:[Crop_Survey_Database](https://github.com/sithjaisong/Crop_Survey_Database.git) from my fork.

#survey <- read_csv(file = "/Users/sithjaisong/Google Drive/4.SKEP2ProjectData/Farm_survey/SKEP2_survey.csv")

diff.injury.profiles <- survey %>% dplyr::select(prod_env, year, season, farmer_type, SNL, RT, RH, SS, WH, PM, RB, DP, FS, NB, RGB, SR, RTH, LF, LM, LS, WM, 
                                            BLB, BLS, BS, LB, NBS, RS, BB, GS, RGS, RTG, YSD, OSD, STV)

# central plain dry season
CP.ds.adopter <- diff.injury.profiles %>% filter(prod_env == "Central_Plain" & season == "dry_season" & farmer_type == "adopter")
CP.ds.majority <-  diff.injury.profiles %>% filter(prod_env == "Central_Plain" & season == "dry_season" & farmer_type == "majority")

#= Central plain wet season
CP.ws.adopter <- diff.injury.profiles %>% filter(prod_env == "Central_Plain" & season == "wet_season" & farmer_type == "adopter")
CP.ws.majority <- diff.injury.profiles %>% filter(prod_env == "Central_Plain" & season == "wet_season" & farmer_type == "majority")

# = Odisha dry season
OD.ds.mojority <- diff.injury.profiles %>% filter(prod_env == "Odisha" & season == "dry_season" & farmer_type == "majority")
OD.ds.drifter <- diff.injury.profiles %>% filter(prod_env == "Odisha" & season == "dry_season" & farmer_type == "drifter")

# = Odisha wet season
OD.ws.majority <- diff.injury.profiles %>% filter(prod_env == "Odisha" & season == "wet_season" & farmer_type == "majority")
OD.ws.drifter <- diff.injury.profiles %>% filter(prod_env == "Odisha" & season == "wet_season" & farmer_type == "drifter")

# Red river dry season
RR.ds.adopter <- diff.injury.profiles %>% filter(prod_env == "Red_river_delta" & season == "dry_season" & farmer_type == "adopter")
RR.ds.majority <- diff.injury.profiles %>% filter(prod_env == "Red_river_delta" & season == "dry_season" & farmer_type == "majority")

# = Red river wet season
RR.ws.adopter <- diff.injury.profiles %>% filter(prod_env == "Red_river_delta" & season == "wet_season" & farmer_type == "adopter")
RR.ws.majority <- diff.injury.profiles %>% filter(prod_env == "Red_river_delta" & season == "wet_season" & farmer_type == "majority")

# = TM dry season
TM.ds.majority <- diff.injury.profiles %>% filter(prod_env == "Tamil_Nadu" & season == "dry_season" & farmer_type == "majority")
TM.ds.drifter <- diff.injury.profiles %>% filter(prod_env == "Tamil_Nadu" & season == "dry_season" & farmer_type == "drifter")

# = TM wet season
TM.ws.majority <- diff.injury.profiles %>% filter(prod_env == "Tamil_Nadu" & season == "wet_season" & farmer_type == "majority")
TM.ws.drifter <- diff.injury.profiles %>% filter(prod_env == "Tamil_Nadu" & season == "wet_season" & farmer_type == "drifter")

# WJ dry season
WJ.ds.adopter <- diff.injury.profiles %>% filter(prod_env == "West_Java" & season == "dry_season" & farmer_type == "adopter")
WJ.ds.majority <- diff.injury.profiles %>% filter(prod_env == "West_Java" & season == "dry_season" & farmer_type == "majority")
WJ.ds.drifter <- diff.injury.profiles %>% filter(prod_env == "West_Java" & season == "dry_season" & farmer_type == "drifter")

# WJ wet season
WJ.ws.adopter <- diff.injury.profiles %>% filter(prod_env == "West_Java" & season == "wet_season" & farmer_type == "adopter")
WJ.ws.majority <- diff.injury.profiles %>% filter(prod_env == "West_Java" & season == "wet_season" & farmer_type == "majority")
WJ.ws.drifter <- diff.injury.profiles %>% filter(prod_env == "West_Java" & season == "wet_season" & farmer_type == "drifter")


# list the dataset by country season

#source("~/Documents/Github/skep2-network/chapter4/R/functions/function_compcorr.R")
source("chapter4/R/functions/function_compcorr.R")
source("~/Documents/Github/skep2-network/chapter4/R/functions/function_diff.corr.R")

source("~/Documents/Github/skep2-network/chapter4/R/functions/function_plot.network.R")
source("~/Documents/Github/skep2-network/chapter4/R/functions/function_plot.difnetwork.R")

pair.IP.list <- as.data.frame(as.matrix(country.season.cor.mat[[1]][1:2]))
#difnet <- list()
#==================================================================================
#area <- c("TMN", "WJV", "LAG", "SUP", "MKD")


# = Differtial network

data.list <- list(CP.ds.adopter, CP.ds.majority, CP.ws.adopter, CP.ws.majority, 
                  OD.ds.mojority, OD.ds.drifter, OD.ws.majority, OD.ws.drifter,
                  RR.ds.adopter, RR.ds.majority, RR.ws.adopter, RR.ws.majority,
                  TM.ds.majority, TM.ds.drifter, TM.ws.majority, TM.ws.drifter,
                  WJ.ds.adopter, WJ.ds.majority, WJ.ds.drifter, WJ.ws.adopter, WJ.ws.majority, WJ.ws.drifter)
#names(dif_CP.ds) <- c("CP.ds.adopter", "CP.ds.majority", )

#source("~/Documents/Github/network.project/chapter3/R/functions/function_cooc_table.R")
source("chapter3/R/functions/function_cooc_table.R")
cor.mat <- list()
for (i in 1:length(data.list)) {
  
  # select out the country and season varible
  temp <- data.list[[i]][!names(data.list[[i]]) %in% c("prod_env", "year" ,"season", "farmer_type")]
  temp <- as.data.frame(temp)
  # generate the corrlation matrix 
  cor.mat[[i]] <- cooc_table(temp)
}

# = country.season.net <- list()
yield.net <- list()
for (i in 1,2,3,5,6,7,8,9,10,11,12, 13, 14,15, 16,17, 18, 19,20, 21, 22){
  print(i) #4, just only 4 is not created the graph
  # keep the correlation coefficient at p.value < 0.05
  cut.table <- cor.mat[[i]] %>% filter(p.value < 0.05)
  
  # construct the netwotk object 
  yield.net[[i]] <- plot_network(cut.table)
  
}

# I need to remnove some network onjects

# name the list object
names(yield.net) <- c("CP_ds_adop", "CP_ds_major",
                      "CP_ws_adop", "CP_ws_major",
                      "OR_ds_major", "OR_ds_drift",
                      "OR_ws_major", "OR_ws_drift",
                      "RR_ds_major", "RR_ds_drift",
                      "RR_ws_adopt", "RR_ws_major",
                      "TM_ds_major", "TM_ds_drift",
                      "TM_ws_major", "TM_ws_drift",
                      "WJ_ds_adopt", "WJ_ds_major", "WJ_ds_drift",
                      "WJ_ws_adopt", "WJ_ws_major", "WJ_ws_drift")

for(i in 1: length(country.season.net)){
  pdf(file = paste("./chapter3/results/plots/network", names(country.season.net)[i] ,".pdf", sep =""), width = 15, height = 12)
  plot(country.season.net[[i]])
  dev.off()
}








cor.mat <- list()
difnet <- list()
 #for (i in 1:5) {
   cor.mat[[2*(i -1) +1]] <- country.season.cor.mat[[2*(i -1) +1]]
   cor.mat[[2*(i -1) +2]] <- country.season.cor.mat[[2*(i -1) +2]]
    
  cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$p.value > 0.05 ] <- 0
  cor.mat[[ 2*(i-1) + 1]]$rho[cor.mat[[ 2*(i-1)+1]]$rho < 0 ] <- 0
  g1 <- graph.edgelist(as.matrix(cor.mat[[2*(i-1) + 1]][1:2]), directed = FALSE)
  E(g1)$weight <- as.matrix(cor.mat[[2*(i-1) + 1]][,3])
  
  cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[2*(i-1) + 2]]$p.value > 0.05 ] <- 0
  cor.mat[[2*(i-1) + 2]]$rho[cor.mat[[ 2*(i-1)+2]]$rho < 0 ] <- 0
  g2 <- graph.edgelist(as.matrix(cor.mat[[2*(i-1) + 2]][1:2]), directed = FALSE)
  E(g2)$weight <- as.matrix(cor.mat[[2*(i-1) + 2]][,3])
  

  
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
  l <-layout.fruchterman.reingold(g1) 
  layout(t(1:3))
  plot(plot_network(g1), layout = l) #, main = paste(names(cor.mat[2*(i-1)+1])))
  plot(plot_difnetwork(gdif), layout = l) #, main = paste("Differential network of survey data in", area[i], sep = " "))
  plot(plot_network(g2), layout = l, main = paste(names(cor.mat[2*(i-1)+2])))
  
}

