#############################################################
#
# R script for data analysis as presented in Figures:
# - Fig : Boxplot of each of injury variables 
#
# files needed: 
# - data from \do.R
# 
#############################################################

boxplot_survey_data <- function(data){
  data <- injury.profiles
  m.all <- melt(data)
  
  #varname <- unique(as.character(as.factor(long.injury.profiles$variable)))
  varname1 <- c("RT", "SS", "WH", "PM", "RB", "DH", "DP", "FS", "NB", "SHB", "SHR", "SR")
  varname2 <- c("RH", "RTH", "LF", "LM", "LS", "WM", "BLB", "BLS", "BS", "LB", "NBS", "RS")
  varname3 <- c("BB", "HB", "GS", "RGS", "RTG") 
  varname1 <- sort(varname1)
  varname2 <- sort(varname2)
  varname3 <- sort(varname3)
  
  levels(m.all$season)[levels(m.all$season) == "dry_season"] <- "DS"
  levels(m.all$season)[levels(m.all$season) == "wet_season"] <- "WS"
  
  #m.all$season <- as.factor(as.character(m.all$season))
  
  ##### boxplots of tiller injuries ####
  
  tiller.boxplot <- list()
  
  for (i in 1:length(varname1)) {
    
    tiller.boxplot[[i]] <- m.all %>% filter(variable == varname1[i]) %>% ggplot(aes(y = value, x = season)) +
      geom_boxplot() + scale_fill_manual(values = c("white", "grey60")) + 
      theme(panel.background = element_rect(fill = "white"), 
            panel.border = element_rect(color = "grey85", fill = NA), 
            panel.grid.major = element_line(color = "grey85", linetype = "dashed"),
            strip.text = element_text(size = rel(0.70), face ="bold"),
            strip.background = element_rect(fill="white", colour ="black", size = 0.5),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.ticks = element_blank()) + 
      facet_grid(.~prod_env, space = "free") + guides(fill=FALSE) +
      ylab("% incidence") +
      ylim(0, 100) +
      ggtitle(paste("Boxplot of ", varname1[i], sep = " "))
    
  }
  ##### boxplots of folier injuries ####
  
  folier.boxplot <- list()
  
  for (i in 1:(length(varname2))) {
    
    folier.boxplot[[i]] <-  m.all %>% filter(variable == varname2[i]) %>% ggplot(aes(y = value, x = season)) +
      geom_boxplot() + scale_fill_manual(values = c("white", "grey60")) + 
      theme(panel.background = element_rect(fill = "white"), 
            panel.border = element_rect(color = "grey85", fill = NA), 
            panel.grid.major = element_line(color = "grey85", linetype = "dashed"),
            strip.text = element_text(size=rel(0.70), face = "bold"),
            strip.background = element_rect(fill="white", colour ="black", size = 0.5),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.ticks = element_blank()) + 
      facet_grid(.~prod_env, space = "free") + guides(fill=FALSE) +
      ylab("% dsu") +
      ggtitle(paste("Boxplot of ", varname2[i], sep = " "))
    
  }
  
  area.boxplot <- list()
  
  for (i in 1:length(varname1)) {
    
    area.boxplot[[i]] <- m.all %>% filter(variable == varname3[i]) %>% ggplot(aes(y = value, x = season)) +
      geom_boxplot() + scale_fill_manual(values = c("white", "grey60")) + 
      theme(panel.background = element_rect(fill = "white"), 
            panel.border = element_rect(color = "grey85", fill = NA), 
            panel.grid.major = element_line(color = "grey85", linetype = "dashed"),
            strip.text = element_text(size = rel(0.70), face ="bold"),
            strip.background = element_rect(fill="white", colour ="black", size = 0.5),
            axis.text.x = element_text(size = 10),
            axis.title.x = element_blank(),
            axis.ticks = element_blank()) + 
      facet_grid(.~prod_env, space = "free") + guides(fill=FALSE) +
      ylab("% area affected") +
      ylim(0, 100) +
      ggtitle(paste("Boxplot of ", varname1[i], sep = " "))
    
  }
 
  all.boxplot1 <- grid.arrange(tiller.boxplot[[1]], tiller.boxplot[[2]], tiller.boxplot[[3]], tiller.boxplot[[4]], tiller.boxplot[[5]], tiller.boxplot[[6]], tiller.boxplot[[7]], tiller.boxplot[[8]], tiller.boxplot[[9]], tiller.boxplot[[10]], tiller.boxplot[[11]], tiller.boxplot[[12]], ncol = 3, nrow = 4)
                             
  all.boxplot2 <- grid.arrange(folier.boxplot[[1]], folier.boxplot[[2]], folier.boxplot[[3]], folier.boxplot[[4]], folier.boxplot[[5]], folier.boxplot[[6]], folier.boxplot[[7]], folier.boxplot[[8]],
                               folier.boxplot[[9]], folier.boxplot[[10]], folier.boxplot[[11]], folier.boxplot[[12]], ncol = 3, nrow = 4)
  
  all.boxplot3 <- grid.arrange(area.boxplot[[1]], area.boxplot[[2]], area.boxplot[[3]], area.boxplot[[4]], area.boxplot[[5]], ncol = 3, nrow = 4)
  
  ggsave(filename = "boxplottiller.pdf", all.boxplot1, width = 50, height = 30, units = "cm")
  ggsave(filename = "boxplotfolier.pdf", all.boxplot2, width = 50, height = 30, units = "cm")
  ggsave(filename = "boxplotarea.pdf", all.boxplot3, width = 50, height = 30, units = "cm")
    
   #  all.dataset.boxplot <- marrangeGrob(c(folier.boxplot, tiller.boxplot), nrow = 3, ncol = 2)
  
 # all.dataset.boxplot <- do.call(grid.arrange, p)

  return(all.dataset.boxplot)
}

