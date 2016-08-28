load("injuryprofiles.RData")

prod_env.name <- c("West_Java", "Odisha", "Tamil_Nadu", "Central_Plain", "Red_river_delta" )
varnames <- sort(levels(m.all$variable))

varname1 <- c("RH", "RTH", "LF", "LM", "WM", "BLB", "BLS", "BS", "LB", "NBS", "RS","SHB", "SHR", "BB", "HB", "GS", "RGS", "RTG") # foliar injuries
varname2 <- c("RT", "SS", "WH", "PM", "RB", "DH", "DP", "FS", "NB", "SR")
varname1 <- sort(varname1)
varname2 <- sort(varname2)


#for (i in 1:length(prod_env.name)) {
  
for(i in 1:(length(varnames)-1)) {
  #png(paste("", "", ".png", wideth = 1000, height = 500)
  
  gdata <- m.all %>% filter(variable == varnames[i])
  ggplot(gdata, aes(y = value, x = variable, fill = season)) + geom_boxplot() + scale_fill_manual(values = c("white", "grey60")) + 
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(color = "grey85", fill = NA), 
          panel.grid.major = element_line(color = "grey85", linetype = "dashed"),
          strip.text = element_text(face="bold", size=rel(0.75)),
          strip.background = element_rect(fill="white", colour ="black", size = 0.5)) + facet_grid(.~prod_env) + guides(fill=FALSE)
#  + stat_summary(fun.y = mean, color="black", geom = "point", show_guide = FALSE)
  
  out[[i]] <- p
  
  out[[29]] <- ggplot(gdata, aes(y = value, x = variable, fill = season)) + geom_boxplot() + scale_fill_manual(values = c("white", "grey60"))
    theme(panel.background = element_rect(fill = "white"), 
          panel.border = element_rect(color = "grey85", fill = NA), 
          panel.grid.major = element_line(color = "grey85", linetype = "dashed")) + facet_grid(.~prod_env)

}
#}

boxplot.variable <- marrangeGrob(out, ncol = 3, nrow = 3 )

temp <- injury.profiles %>% filter(prod_env == prod_env.name[1])
describeBy(temp[-1], group = c("year", "season"), mat = TRUE, digits=2)

#data_profile <- describe(injury.profiles[,-c(1:3)], ranges = TRUE)