library(ggExtra)
library(grid)
library(ggplot2)
#cols <- c("negative" = "red","positive" = "blue")
#country.season <- c("CP_ds", "CP_ws", "OD_ds", "OD_ws","RR_ds", "RR_ws", "TM_ds", "TM_ws","WJ_ds", "WJ_ws")


rho.value.test <- list()
for (i in 1:length(country.season.cor.mat)) {
  
rho.value  <- country.season.cor.mat[[i]] %>% filter(p.value < 0.05) %>% filter(rho != 0) %>% dplyr::select(rho)

rho.value$correlation <- "na"
rho.value$correlation <- ifelse(rho.value$rho < 0, "negative", 
                                ifelse(rho.value$rho > 0, "positive",
                                       "no correlation"))

rho.value$rho <- abs(rho.value$rho)
rho.value$correlation <- as.factor(rho.value$correlation)
rho.value.test[[i]] <- rho.value

}


P1obj <- ggplot(rho.value, aes(x = rho, fill = correlation)) + 
  geom_histogram(position ="identity", alpha = 0.5, binwidth = 0.05, bins = 20) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_fill_manual(values = cols) +
  expand_limits(x = c(0,1), y = c(0, 30)) +
  xlab(expression(paste("Injury interaction stength (|", italic(R)^2, ")|)"))) +
  ylab("Frequency") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        legend.key = element_rect(size = 1),
      legend.position = c(0.8, 0.9),
        plot.margin = unit(c(0.2, 0.2, 0.5, 0.5), "lines"))

#P1obj$coordinates$limits
P1 <- P1obj + coord_cartesian(xlim = c(0,1))

# Horizontal marginal boxplot - to appear at the top of the chart
P2 <- ggplot(rho.value, aes(x = correlation, y = rho,  fill = correlation)) + 
  geom_boxplot(outlier.colour = NA, alpha = 0.5) + 
  guides(fill=FALSE) +
#  geom_jitter(position = position_jitter(width = 0.05)) + 
#  expand_limits(y = c(0, 1)) + 
  scale_y_continuous(limits=c(0,1), expand = c(0, 0)) +
  scale_fill_manual(values = cols) +
  coord_flip() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        plot.margin = unit(c(1, 0.2, -1.5, 0.5), "lines"))

# Assembly part
Gt1 <- ggplot_gtable(ggplot_build(P1)) # turn ggplot onject to the gtable object
Gt2 <- ggplot_gtable(ggplot_build(P2))

maxWidth <- unit.pmax(Gt1$widths[2:3], Gt2$widths[2:3])

Gt1$widths[2:3] <- as.list(maxWidth)
Gt2$widths[2:3] <- as.list(maxWidth)

# create gtable 2 rows with 1 column

Gt <- gtable(widths = unit(c(1), "null"), height = unit(c(1, 7), "null"))

#gtable_show_layout(G1)

Gt <- gtable_add_grob(Gt, Gt1, 2, 1) # place Gt1 (histogram) at the bottom 
Gt <- gtable_add_grob(Gt, Gt2, 1, 1) # place Gt2 (boxplot) at the top

grid.newpage() # clear space
grid.draw(Gt) # plot

ggsave(Gt,  file = paste("combinedplotnew", country.season[i], ".pdf", sep = ""))

}


wilcox.test(rho ~ correlation, data = rho.value.test[[10]], exact = FALSE)
kruskal.test(rho ~ correlation, data = rho.value.test[[4]])
t.test(rho ~ correlation, data = rho.value.test[[10]])
