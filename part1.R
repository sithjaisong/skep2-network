library(dplyr)
library(ggplot2)
library(readr)
library(reshape)
library(reshape2)
library(gridExtra)
library(e1071) 
library(psych)
library(WGCNA)
library(pheatmap)
library(Hmisc)
library(xtable)
library(PerformanceAnalytics)

survey <- read_csv(file = "/Users/sithjaisong/Google Drive/4.SKEP2ProjectData/Farm_survey/SKEP2_survey.csv")

injury.profiles <- survey %>% dplyr::select(prod_env, year, season, RT, RH, SS, WH, PM, RB, DH, DP, FS, NB, SR, RTH, LF, LM, LS, WM, 
                                            BLB, BLS, BS, LB, NBS, RS, BB, HB, GS, RGS, RTG, SHB, SHR, WA, WB) %>% 
  transform(prod_env = as.factor(prod_env),
            year = as.factor(year),
            season = as.factor(season))


m.all <- melt(injury.profiles)

varnames <- sort(levels(m.all$variable))
 

out <- list()

for(i in 1:length(varnames)) {
  gdata <- m.all %>% filter(variable == varnames[i])
  p <- ggplot(gdata, aes(x = value)) + 
    geom_histogram(stat = "bin") + ylab("No. of farmers' fields") + ggtitle(paste(varnames[i], sep = " ")) + 
    theme_bw() + theme(axis.title.y = element_text(hjust = 0.5, size = 9)) + scale_x_continuous(expand = c(0, 0))
 out[[i]] <- p
}

#all.histo1 <- grid.arrange(out[[1]], out[[2]], out[[3]],out[[4]],  out[[5]],  out[[6]], out[[7]],out[[8]], out[[9]], out[[10]],  out[[11]],  out[[12]], out[[13]],  out[[14]], out[[15]], nrow = 3, ncol = 5)

#all.histo2 <- grid.arrange(out[[16]],  out[[17]],  out[[18]],  out[[19]], out[[20]], out[[21]], out[[22]], out[[23]], out[[24]], out[[25]], out[[26]], out[[27]], out[[28]], out[[29]],  nrow = 3, ncol = 5)


all.histo1 <- grid.arrange(out[[1]], out[[2]], out[[3]],out[[4]],  out[[5]],  out[[6]], out[[7]], out[[8]], out[[9]], out[[10]],  out[[11]],  out[[12]], out[[13]],  out[[14]], out[[15]], out[[16]], nrow = 4, ncol = 4)

all.histo2 <- grid.arrange(out[[17]],  out[[18]],  out[[19]], out[[20]], out[[21]], out[[22]], out[[23]], out[[24]], out[[25]], out[[26]], out[[27]], out[[28]], out[[29]], out[[30]],out[[31]], nrow = 4, ncol = 4)

ggsave(all.histo1, file = "allhistonew1.pdf", width = 45 , height = 30, units = "cm") # width = 1500, height = 1000
ggsave(all.histo2, file = "allhistonew2.pdf", width = 45 , height = 30, units = "cm") # width = 1500, height = 1000

data_profile <-   psych::describe(injury.profiles[,-c(1:3)], type = 2)
data_profile$vars <- row.names(data_profile)
data_profile <- as.data.frame(data_profile)
data_profile <- data_profile[rev(order(data_profile$vars, decreasing = TRUE)),]
row.names(data_profile) <- NULL
xtable(data_profile)

write_csv(path = "sum_injuries.csv", data_profile)


shapiro_result <- lapply(injury.profiles[,-c(1:3)], shapiro.test)
kurtosis_result <- lapply(injury.profiles[,-c(1:3)], kurtosis) # should be within the range ±2.

all <- injury.profiles[,-c(1:3)]

##### construct the correlation matrix ######
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

all.pearson <- cor(all, method = "pearson", use = "pairwise") # pearson correlation

res <- rcorr(as.matrix(all), type = "pearson")
cor.pearson <- flattenCorrMatrix(res$r, res$P)
cor.pearson <- as.data.frame(cor.pearson)
person.pval <- cor.pearson$p
sum(person.pval < 0.05)
sum(person.pval < 0.01)
sum(person.pval > 0.05)
cor.pearson %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% xtable() # 122 pair 
cor.pearson %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% write_csv(path = "corpearson.csv")

all.spearman <- cor(all, method = "spearman", use = "pairwise") # spearman correlation
res <- rcorr(as.matrix(all), type = "spearman")
cor.spearman <- flattenCorrMatrix(res$r, res$P)
cor.spearman <- as.data.frame(cor.spearman)
spearman.pval <- cor.spearman$p
sum(spearman.pval < 0.05)
sum(spearman.pval < 0.01)
sum(spearman.pval > 0.05)
cor.spearman %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% xtable() # 193 pairs
cor.spearman %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% write_csv(path = "corspearman.csv")

all.kendall <- cor(all, method = "kendall", use = "pairwise") # kendall correlation

res <- corr.test(all, method = "kendall", use = "pairwise") # kendall correlation
cor.kendall <- flattenCorrMatrix(res$r, res$p) 
cor.kendall <- as.data.frame(cor.kendall)
kendall.pval <- cor.kendall$p
sum(kendall.pval < 0.05)
sum(kendall.pval < 0.01)
sum(kendall.pval > 0.05)

cor.kendall %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% xtable() # 72 pairs
cor.kendall %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% write_csv(path = "corkendall.csv")

all.biweight <- bicor(all, use = "pairwise") # Biweight Midcorrelation from WGCNA package

res <- bicorAndPvalue(all, use = "pairwise")
cor.biweight <- flattenCorrMatrix(res$bicor, res$p)

cor.biweight <- as.data.frame(cor.biweight)
biweight.pval <- cor.biweight$p
sum(biweight.pval < 0.05)
sum(biweight.pval < 0.01)
sum(biweight.pval > 0.05)

cor.biweight %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% xtable()  # 128 pairs
cor.biweight %>% filter(p < 0.05) %>% arrange(desc(cor)) %>% write_csv(path = "corbiweight.csv")

chart.Correlation(all, method = "spearman")
chart.Correlation(all, method = "pearson")
chart.Correlation(all, method = "kendall")
#===========================================
##### Transform the correlation matrix #####
#============================================

# change from matrix to data frame, and extract the value of each correlation approach

### Peason correlation
df.pearson <- as.data.frame(all.pearson)
df.pearson.corval <- c(as.matrix(df.pearson))
names(df.pearson.corval) <- "Pearson"


### Spearman correlation

df.spear <- as.data.frame(all.spearman)
df.spear.corval <- c(as.matrix(df.spear))
names(df.spear.corval) <- "Spearman"


### Kendall correlation

df.kendall <- as.data.frame(res$r)
df.kendall.corval <- c(as.matrix(df.kendall))
names(df.kendall.corval) <- "Kendall"



### Biweight Midcorrelation
df.biweight <- as.data.frame(all.biweight)
df.biweight.cor.val <- c(as.matrix(df.biweight))
names(df.biweight.cor.val) <- "Biweight"


#====================================================
##### Combine correlation value of each method ######
#===================================================
# will add more correlation
bind.cor <- cbind(df.pearson.corval,
                  df.spear.corval,
                  df.kendall.corval,
                  df.biweight.cor.val)

bind.cor <- as.data.frame(bind.cor)
row.names(bind.cor) <- NULL

##### Cluster Analysis and correlation matrix #####

dis <- dist(bind.cor, diag=TRUE)
dis <- as.matrix(dis)
cor.of.cor <- cor(bind.cor)
pheatmap(cor.of.cor, cellwidth = 50, cellheight = 50, fontsize = 16)

save(file = "injuryprofiles.RData", injury.profiles)
