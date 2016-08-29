#library(plyr)
# after you run  all

#result <- data.frame(injury = c("BB", "BLB", "BLS", "BS", "DH", "DP", "FS", "GS", "HB", "LB",  "LF", "LM", "LS", "NB", "NBS", "PM", "RB", "RGS", "RH" , "RS", "RT", "RTG", "RTH", "SHB", "SHR", "SR" ,"SS" , "WH" , "WM"),
#                     dry_deg = NA, dry_bet = NA, wet_deg = NA, wet_bet = NA,  dif_season_deg = NA, dif_season_bet = NA,  dif_yield_deg = NA, dif_yield_bet = NA)

#inj.name <- c("BB" , "BLB", "BLS" ,"BS" , "DH" , "DP"  ,"FS" , "GS" , "HB" , "LB",  "LF"  ,"LM" , "LS"  ,"NB" , "NBS", "PM" ,"RB" , "RGS" ,"RH"  ,"RS"  ,"RT" , "RTG", "RTH" ,"SHB" ,"SHR" ,"SR" , "SS" , "WH" , "WM")

#country.season.names <- c("CP_ds", "CP_ws", "OR_ds", "OR_ws", "RR_ds", "RR_ws", "TM_ds", "TM_ws", "WJ_ds", "WJ_ws")

#country.names <- c("CP","OD", "RR", "TM", "WJ") 

for (i in 1:5) {

  result <- data.frame(injury = c("BB", "BLB", "BLS", "BS", "DH", "DP", "FS", "GS", "HB", "LB",  "LF", "LM", "LS", "NB", "NBS", "PM", "RB", "RGS", "RH" , "RS", "RT", "RTG", "RTH", "SHB", "SHR", "SR" ,"SS" ,"WA", "WB", "WH" , "WM"),
                     dry_deg = NA, dry_bet = NA, wet_deg = NA, wet_bet = NA,  dif_season_deg = NA, dif_season_bet = NA,  dif_yield_deg = NA, dif_yield_bet = NA)
#result$injury <- inj.name

# = dry season network
dry_deg <- node.df %>% dplyr::select(country_season, var, degree) %>% filter(country_season == country.season.names[2*(i-1) + 1]) %>% arrange(desc(degree))
dry_deg <- head(dry_deg, nrow(dry_deg)/3) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$dry_deg <- ifelse(result$injury %in% dry_deg == TRUE, 1, 0)

dry_bet <- node.df %>% dplyr::select(country_season, var, betweenness) %>% filter(country_season == country.season.names[2*(i-1) + 1]) %>% arrange(desc(betweenness)) 
dry_bet <-  head(dry_bet, nrow(dry_bet)/3) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$dry_bet <- ifelse(result$injury %in% dry_bet == TRUE, 1, 0)

# = wet season network
wet_deg <- node.df %>% dplyr::select(country_season, var, degree) %>% filter(country_season == country.season.names[2*(i-1) + 2]) %>% arrange(desc(degree)) 
wet_deg <- head(wet_deg, nrow(wet_deg)/3) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$wet_deg <- ifelse(result$injury %in% wet_deg == TRUE, 1, 0)

wet_bet <- node.df %>% dplyr::select(country_season, var, betweenness) %>% filter(country_season == country.season.names[2*(i-1) + 2]) %>% arrange(desc(betweenness)) 
wet_bet <- head(wet_bet, nrow(wet_bet)/3) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$wet_bet <- ifelse(result$injury %in% wet_bet == TRUE, 1, 0)

# = dif season net



dif_season_deg <- re_difnet.node.stat.list.node.df %>% dplyr::select(country_season, var, degree) %>% filter(country_season == country.names[i]) %>% arrange(desc(degree)) 

sel_inj <- ifelse((nrow(dif_season_deg)/3 < 3), 3, nrow(dif_season_deg)/3)

dif_season_deg <- head(dif_season_deg, sel_inj) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$dif_season_deg <- ifelse(result$injury %in% dif_season_deg == TRUE, 1, 0)

dif_season_bet  <- re_difnet.node.stat.list.node.df %>% dplyr::select(country_season, var, betweenness) %>% filter(country_season == country.names[i]) %>% arrange(desc(betweenness)) 
dif_season_bet <- head(dif_season_bet, sel_inj) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$dif_season_bet <- ifelse(result$injury %in% dif_season_bet == TRUE, 1, 0)

# = dif year net

dif_yield_deg <- re_yielddifnet.node.stat.list.node.df %>% dplyr::select(country_season, var, degree) %>% filter(country_season == country.names[i]) %>% arrange(desc(degree)) 
sel_inj <- ifelse((nrow(dif_yield_deg)/3 < 3), 3, nrow(dif_yield_deg)/3)

dif_yield_deg <- head(dif_yield_deg, sel_inj) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$dif_yield_deg <- ifelse(result$injury %in% dif_yield_deg == TRUE, 1, 0)


dif_yield_bet  <- re_yielddifnet.node.stat.list.node.df %>% dplyr::select(country_season, var, betweenness) %>% filter(country_season == country.names[i]) %>% arrange(desc(betweenness)) 
dif_yield_bet <- head(dif_yield_bet, sel_inj) %>% as.data.frame() %>% dplyr::select(var) %>% unlist() %>% unname() %>% as.character()
result$dif_yield_bet <- ifelse(result$injury %in% dif_yield_bet == TRUE, 1, 0)


data <- result[, 1:9]
data.m <- melt(data, id.var = "injury")
data.m$value <- as.factor(data.m$value)
data.m$variable <- as.factor(data.m$variable)

levels(data.m$variable)[levels(data.m$variable) == "dry_deg"] <- "Node degree of dry season network"
levels(data.m$variable)[levels(data.m$variable) == "dry_bet"] <- "Betweenness of dry season network"
levels(data.m$variable)[levels(data.m$variable) == "wet_deg"] <- "Node degree of wet season network"
levels(data.m$variable)[levels(data.m$variable) == "wet_bet"] <- "Betweenness of wet season network"
levels(data.m$variable)[levels(data.m$variable) == "dif_season_deg"] <- "Node degree of differential season network"
levels(data.m$variable)[levels(data.m$variable) == "dif_season_bet"] <- "Betweenness of differential season network"
levels(data.m$variable)[levels(data.m$variable) == "dif_yield_deg"] <- "Node degree of differential yield level network"
levels(data.m$variable)[levels(data.m$variable) == "dif_yield_bet"] <- "Betweenness of differential yield level network"
  
  
ggplot(data.m, aes(injury, variable)) + geom_tile(aes(fill = value)) + scale_fill_manual(name = "Is it in \nthe top quater?", values = c( "1" = "black", "0" = "grey80"), labels = c("No", "Yes")) + 
  theme(axis.text.x = element_text(size = 6),  axis.text.y = element_text(size = 6), axis.title.x = element_text(size = 12),  axis.title.y = element_blank(), legend.title = element_text(size=8, face = "bold"), legend.text = element_text(size = 6))


ggsave(filename = paste("sum_mat_", country.names[i], ".pdf", sep =""), width = 25, height = 5, units = "cm")

}
#save(file = "coocnetwork.RData",country.season.net ,re_difnet, re_yielddifnet, node.df, re_difnet.node.stat.list.node.df, re_yielddifnet.node.stat.list.node.df )

