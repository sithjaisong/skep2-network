# SKEPII network analysis of injury profile bason on different locations

# Summary 
# The workflow of this research composed of  four steps
# 1. Load survey data of SKEP1(raw data), and compact the data.
# 2. Generate the correlation matrix for building a network from data that were grouped by country and season
# 3. Analyze the topological statistic of the network, and interpret 
# 4. Detect the communities within the network
# 

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(extrafont)
library(ggExtra)
library(gtable)
library(igraph)
library(xtable)
library(reshape2)
library(cowplot)
library(pheatmap)
library(magrittr)
library(readr)

##### 1.Load survey data of SKEP1  (raw data), and compact the data. ######
# The data are in the shared folder named "surveySKEP1"
# You can download or folk from Github:[Crop_Survey_Database](https://github.com/sithjaisong/Crop_Survey_Database.git) from my fork.
load(file = "injuryprofiles.RData")
#load(file = "survey.RData")
#survey <- read_csv(file = "/Users/sithjaisong/Google Drive/4.SKEP2ProjectData/Farm_survey/SKEP2_survey.csv")

# I created the compared the variable by country by season
# reshape
injury.profiles$year <- NULL  # remove column year

#source("~/Documents/Github/skep2-network/chapter3/R/figure/F1.dataset.R")

#source("~/Documents/Github/skep2-network/chapter3/R/functions/function_to_pdf_png.R")

#all.dataset.boxplot <- boxplot_survey_data(injury.profiles)

#ggsave(filename  = "dataset_boxplot.pdf", all.dataset.boxplot, dpi = 720)

#### 2. Generate the correlationsion matrix  #####

source("~/Documents/Github/skep2-network/chapter3/R/functions/function_cooc_table.R")

# subset the data by country and season
CP.ds <- injury.profiles %>% filter(prod_env == "Central_Plain" & season == "dry_season")
CP.ws <- injury.profiles %>% filter(prod_env == "Central_Plain" & season == "wet_season")
OD.ds <- injury.profiles %>% filter(prod_env == "Odisha" & season == "dry_season")
OD.ws <- injury.profiles %>% filter(prod_env == "Odisha" & season == "wet_season")
RR.ds <- injury.profiles %>% filter(prod_env == "Red_river_delta" & season == "dry_season")
RR.ws <- injury.profiles %>% filter(prod_env == "Red_river_delta" & season == "wet_season")
TM.ds <- injury.profiles %>% filter(prod_env == "Tamil_Nadu" & season == "dry_season")
TM.ws <- injury.profiles %>% filter(prod_env == "Tamil_Nadu" & season == "wet_season")
WJ.ds <- injury.profiles %>% filter(prod_env == "West_Java" & season == "dry_season")
WJ.ws <- injury.profiles %>% filter(prod_env == "West_Java" & season == "wet_season")

# create the list 
country.season.dataset <- list(CP.ds, CP.ws, OD.ds, OD.ws, RR.ds, RR.ws, TM.ds, TM.ws, WJ.ds, WJ.ws)


# create the list object to store the data
country.season.cor.mat <- list()

for (i in 1:length(country.season.dataset)) {
  
  # select out the country and season varible
  temp <- country.season.dataset[[i]][!names(country.season.dataset[[i]]) %in% c("prod_env", "season")]
  
   # generate the corrlation matrix 
  
  country.season.cor.mat[[i]] <- cooc_table(temp)
}
 
##### 3. Generate the network object  #####
source("~/Documents/Github/skep2-network/chapter3/R/functions/function_plot_network.R")


country.season.net <- list()

for (i in 1:length(country.season.cor.mat)) {
  
  # keep the correlation coefficient at p.value < 0.05
  cut.table <- country.season.cor.mat[[i]] 
  #%>% filter(p.value < 0.05) %>% filter(rho > 0)

# construct the netwotk object 
country.season.net[[i]] <- 
  plot_network(cut.table)

}
# name the list object
names(country.season.net) <- c("CP_ds", "CP_ws", 
                               "OR_ds", "OR_ws",
                               "RR_ds", "RR_ws",
                               "TM_ds", "TM_ws",
                               "WJ_ds", "WJ_ws")

for(i in 1: length(country.season.net)){
  pdf(file = paste("./chapter3/results/plots/fullnetwork.new", names(country.season.net)[i] ,".pdf", sep =""), width = 9, height = 9)
  E(country.season.net[[i]])[weight > 0]$color <- adjustcolor("steelblue2" , alpha.f = .8)
  E(country.season.net[[i]])[weight < 0]$color <- adjustcolor("red2", alpha.f = .8)
  V(country.season.net[[i]])$size <- 15
  plot(country.season.net[[i]])
  dev.off()
}


E(net)[weight > 0]$color <- adjustcolor("steelblue2" , alpha.f = .8)
E(net)[weight < 0]$color <- adjustcolor("red2", alpha.f = .8)
V(net)$size <- 15
net$layout <- layout_with_fr
  plot(net)
# ========== The network properties display ============= # 
# plot dot plot

source("~/Documents/Github/skep2-network/chapter3/R/functions/function_plot.node.centrality.R") 
for(i in 1:length(country.season.net)){
  dotgraph <- plot.node.centrality(country.season.net[[i]]) 
  ggsave(dotgraph, file = paste("./chapter3/results/plots/nodepropnew.net", names(country.season.net)[i] ,".pdf", sep =""), width = 15, height = 8)
}
dotgraph

##### 4. network statistics #####
source("~/Documents/Github/skep2-network/chapter3/R/functions/function_node_net_stat.R")  # node_stat() and net_stat()
source("~/Documents/Github/skep2-network/chapter3/R/functions/function_random_graph.R") 

# compute the node statistic properties
node.stat.list <- sapply(country.season.net, node_stat, simplify = FALSE, 
                         USE.NAMES = TRUE)

# combine the node statistic from each network of country by season
node.df <- do.call(rbind, node.stat.list)

# add coolumn to indicate the source of node stat
node.df$country_season <- gsub("\\..*", "", row.names(node.df))

# delete row name
row.names(node.df) <- NULL

# reorder the colummn by moving column named country_season into the first
node.df <- node.df %>% dplyr::select(country_season, everything())

xtable(node.df)

# = the box plot present the node statistics with degree, betweenness, closness, and cluster.coef

node.df %>% dplyr::select(country_season, degree) %>% 
  ggplot(aes(x = country_season, y = degree, fill = country_season)) + geom_boxplot() + scale_fill_brewer(palette = "Paired")

node.df %>% dplyr::select(country_season, betweenness) %>% ggplot(aes(x = country_season, y = betweenness, fill = country_season)) + geom_boxplot() + scale_fill_brewer(palette = "Paired")


#http://blog.revolutionanalytics.com/2014/12/finding-clusters-of-cran-packages-using-igraph.html

##### 4. detect community ####
 
# call function cluster.network 
source("~/Documents/Github/skep2-network/chapter3/R/functions/function_cluster.network.R") 
source("~/Documents/Github/skep2-network/chapter3/R/functions/function_to_pdf_png.R") 

for(i in 1:length(country.season.net)){
  pdf(file = paste("./chapter3/results/plots/community.new", names(country.season.net)[i], ".pdf", sep =""), width = 9, height = 9) #, height = 800, width = 800)
  # setect the community with the function cluster.network
  clusts <- cluster.network(country.season.net[[i]]) 

  # plot network graph
plot(clusts, 
     mark.groups = NULL, 
     vertex.frame.color = NA), 
     main = paste("Communities in", names(country.season.net[i]), "network", sep =" " )
)
dev.off()

}

 # detect the communities
 community <- data.frame()

 for(i in 1:length(country.season.net)){
   
new.community  <- optimal.community(country.season.net[[i]], weights = abs(E(country.season.net[[i]])$weight)) %>% 
  membership() %>% 
  sort() %>% 
  as.data.frame() %>% 
  set_colnames("cluster")

new.community$node <- rownames(new.community)
new.community$group <- names(country.season.net[i])
rownames(new.community) <- NULL

community <- rbind(community, new.community)
}

 # = community profile
 
 community %>% filter(group == "CP_ds")
 community %>% filter(group == "CP_ws")
 community %>% filter(group == "OR_ws")
 community %>% filter(group == "OR_ws")
 community %>% filter(group == "RR_ws")
 community %>% filter(group == "RR_ws")
 community %>% filter(group == "TM_ws")
 community %>% filter(group == "TM_ws")
 community %>% filter(group == "WJ_ws")
 community %>% filter(group == "WJ_ws")
 
#load(file = "~/Google Drive/surveySKEP1/chapter3netdata.RData")

injury.profiles %>% group_by(prod_env, year, season) %>% summarise(count = n()) %>% data.frame()

## ======= network wise statistics of our empirical network

# apply net_stat function to all the object list
net.stat.list <- sapply(country.season.net, net_stat, simplify = FALSE, 
                        USE.NAMES = TRUE)
# combine the network stat from all list object
net.df <- do.call(rbind, net.stat.list)

# add column to indicate the source of data 
net.df$country_season <- gsub("\\..*", "", row.names(net.df))

# delete row name 
row.names(net.df) <- NULL

 # reorder the colummn by moving column named country_season into the first
net.df %>% dplyr::select(country_season, everything()) %>% write_csv(path = "net_prop.csv")
net.df %>% xtable()

save(country.season.net, country.season.cor.mat, file = "countryseasonnet.RData")
