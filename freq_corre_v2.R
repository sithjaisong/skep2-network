country.season <- c("CP_ds", "CP_ws", "OR_ds", "OR_ws", "RR_ds", "RR_ws", "TM_ds", "TM_ws",                               "WJ_ds", "WJ_ws")

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

for (i in 1:length(rho.value.test)) {

his <-   rho.value.test[[i]] %>% ggplot(aes(rho, color=correlation, fill=correlation)) + 
  geom_histogram(alpha=0.55) + 
  scale_fill_manual(values=c("#8B1A1A", "#104E8B")) +
  scale_color_manual(values=c("#8B1A1A", "#104E8B")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  xlab(expression(paste("Injury interaction strength (|", italic(R)^2, ")|)"))) +
  ylab("Frequency")
ggsave(his,  file = paste("his_rho", country.season[i], ".pdf", sep = ""))

}

