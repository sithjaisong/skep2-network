library(reshape)

data <- injury.profiles

# = transform to binary data 

data$RT[data$RT > 0] <- 1
data$RH[data$RH > 0] <- 1
data$SS[data$SS > 0] <- 1
data$WH[data$WH > 0] <- 1
data$WM[data$WM > 0] <- 1
data$PM[data$RB > 0] <- 1
data$RB[data$RB > 0] <- 1
data$DH[data$DH > 0] <- 1
data$DP[data$DP > 0] <- 1
data$PM[data$PM > 0] <- 1
data$FS[data$FS > 0] <- 1
data$NB[data$NB > 0] <- 1
data$SR[data$SR > 0] <- 1
data$RTH[data$RTH > 0] <- 1
data$LF[data$LF > 0] <- 1
data$LM[data$LM > 0] <- 1
data$LS[data$LS > 0] <- 1
data$BLB[data$BLB > 0] <- 1
data$BLS[data$BLS > 0] <- 1
data$BS[data$BS > 0] <- 1
data$LB[data$LB > 0] <- 1
data$RS[data$RS > 0] <- 1
data$BB[data$BB > 0] <- 1
data$BLB[data$BLB >0] <- 1
data$HB[data$HB > 0] <- 1
data$GS[data$GS > 0] <- 1
data$RGS[data$RTG > 0] <- 1
data$SHB[data$SHB > 0] <- 1
data$SHR[data$SHR > 0] <- 1
data$NBS[data$NBS > 0] <- 1
data$RGS[data$RGS > 0] <- 1
data$RTG[data$RTG > 0] <- 1
data$WA[data$WA > 0] <- 1
data$WB[data$WB > 0] <- 1

total <- data %>% group_by(prod_env, season) %>% summarise(count = n())
#data2 <- left_join(data, total)
data2 <- data %>% group_by(prod_env, season) %>% summarise(sum_RT = sum(RT),
                                                           sum_RH = sum(RH),
                                                           sum_SS = sum(SS),
                                                           sum_WH = sum(WH),
                                                           sum_PM = sum(PM),
                                                           sum_RB = sum(RB),
                                                           sum_DH = sum(DH),
                                                           sum_DP = sum(DP),
                                                           sum_FS = sum(FS),
                                                           sum_NB = sum(NB),
                                                           sum_SR = sum(SR),
                                                           sum_RTH = sum(RTH),
                                                           sum_LF = sum(LF),
                                                           sum_LM = sum(LM),
                                                           sum_LS = sum(LS),
                                                           sum_WM = sum(WM),
                                                           sum_BLB = sum(BLB),
                                                           sum_BLS = sum(BLS),
                                                           sum_BS = sum(BS),
                                                           sum_LB = sum(LB),
                                                           sum_NBS = sum(NBS),
                                                           sum_RS = sum(RS),
                                                           sum_BB = sum(BB),
                                                           sum_HB = sum(HB),
                                                           sum_GS = sum(GS),
                                                           sum_RGS = sum(RGS),
                                                           sum_RTG = sum(RTG),
                                                           sum_SHB = sum(SHB),
                                                           sum_SHR = sum(SHR),
                                                           sum_WA = sum(WA),
                                                           sum_WB = sum(WB))



m.data2 <- melt(as.data.frame(data2))
#, id.vars = c("prod_env", "season"))

m.j.data2 <- left_join(m.data2, total)

m.j.data2.prov <- m.j.data2 %>% group_by(prod_env, season, variable) %>% summarise(prev = value/count*100) %>% as.data.frame()

m.j.data2.prov$variable <- as.factor(as.character(m.j.data2.prov$variable))

levels(m.j.data2.prov$season)[levels(m.j.data2.prov$season) == "dry_season"] <- "DS"
levels(m.j.data2.prov$season)[levels(m.j.data2.prov$season) == "wet_season"] <- "WS"

varname <- sort(levels(m.j.data2.prov$variable))
new.name <- c("BB" , "BLB" ,"BLS" , "BS", "DH",  "DP" , "FS" , "GS" , "HB" , "LB"  ,"LF" , "LM"  ,"LS" , "NB" , "NBS" ,"PM", "RB" , "RGS" ,"RH" , "RS",  "RT"  ,"RTG", "RTH" ,"SHB", "SHR" ,"SR" , "SS" ,"WA", "WB", "WH" , "WM" )

levels(m.j.data2.prov$prod_env)[levels(m.j.data2.prov$prod_env) == "Central_Plain"] <- "Central Plain"
levels(m.j.data2.prov$prod_env)[levels(m.j.data2.prov$prod_env) == "Red_river_delta"] <- "Red River Delta"
levels(m.j.data2.prov$prod_env)[levels(m.j.data2.prov$prod_env) == "Tamil_Nadu"] <- "Tamil Nadu"
levels(m.j.data2.prov$prod_env)[levels(m.j.data2.prov$prod_env) == "West_Java"] <- "West Java"

barplot <- list()

for (i in 1:length(varname)) {
  
  barplot[[i]] <- m.j.data2.prov %>% filter(variable == varname[i]) %>% ggplot(aes(y = prev, x = season)) + geom_bar(stat = "identity")+ scale_fill_manual(values = c("white", "grey60")) + 
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(color = "grey85", fill = NA), 
          panel.grid.major = element_line(color = "grey85", linetype = "dashed"),
          strip.text = element_text(size = rel(0.70), face ="bold"),
          strip.background = element_rect(fill="white", colour ="black", size = 0.5),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.ticks = element_blank()) + 
    facet_grid(.~prod_env, space = "free") + guides(fill=FALSE) +
    ylab("% prevelence") +
    ylim(0, 100) +
    ggtitle(paste(new.name[i]))
  
}

all.barplot1 <- grid.arrange(barplot[[1]], barplot[[2]], barplot[[3]], barplot[[4]], barplot[[5]], barplot[[6]], barplot[[7]], barplot[[8]], barplot[[9]], barplot[[10]], barplot[[11]], barplot[[12]], ncol = 3, nrow = 4)

all.barplot2 <- grid.arrange(barplot[[13]], barplot[[14]], barplot[[15]], barplot[[16]], barplot[[17]], barplot[[18]], barplot[[19]], barplot[[20]], barplot[[21]], barplot[[22]], barplot[[23]], barplot[[24]], ncol = 3, nrow = 4)

all.barplot3 <- grid.arrange(barplot[[25]], barplot[[26]], barplot[[27]], barplot[[28]], barplot[[29]], barplot[[30]], barplot[[31]], ncol = 3, nrow = 4)

ggsave(filename = "barplotnew1.pdf", all.barplot1, width = 50, height = 30, units = "cm")
ggsave(filename = "barplotnew2.pdf", all.barplot2, width = 50, height = 30, units = "cm")
ggsave(filename = "barplotnew3.pdf", all.barplot3, width = 50, height = 30, units = "cm")
