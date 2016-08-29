ggplot(data = all, aes(x = DP, y = BS)) + 
  geom_point()


#### Check correlation line on the xy plot #####

DP <- all$DP
BS <- all$BS
DF <- data.frame(DP, BS)

#DF <- DF %>% filter(BLB >0  & BLS >0)
cor.test(DF$BPH, DF$WBP, method = "pearson")

lin <- ggplot(DF, aes(x = DP, y = BS)) +geom_point() #+ coord_trans(xtrans = "log10", ytrans = "log10") # + scale_x_log10() + scale_y_log10()
lin
lin + stat_smooth()
lin  + stat_smooth(method = "lm", formula = y ~ x, size = 1)
lin + stat_smooth(method = "loess", formula = y ~ x, size = 1)
lin + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)
lin + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
lin + stat_smooth(method = "gam", formula = y ~ s(x), size = 1)

#lin + stat_smooth(method = 'nls', formula = 'y~a*x^b', start = list(a = 1,b=1)) + geom_text(x = 600, y = 1, label = power_eqn(DF), parse = TRUE)

power_eqn = function(df, start = list(a =300,b=1)){
  m = nls(DP ~ a*BS^b, start = start, data = df);
  eq <- substitute(italic(y) == a  ~italic(x)^b, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2)))
  as.character(as.expression(eq));                 
}


lin <- ggplot(DF, aes(BS, DP)) +geom_point()+
  geom_smooth(method = "lm")
glin <- lin + geom_smooth(method = "glm",
                          family = gaussian(link="log"))


#========================================================

NBS <- all$nbs
FSM <- all$fsm
DF2 <- data.frame(NBS, FSM)

DF2 <- DF2 %>% filter( NBS > 0 )

lin <- ggplot(DF2,aes(NBS, FSM)) +geom_point()+
  geom_smooth(method = "lm")
glin <- lin + geom_smooth(method = "glm")
glin


#===================================
DH <- all$dhx
FSM <- all$fsm
DF3 <- data.frame(DH, FSM)

DF2 <- DF %>% filter( BS > 0 & DP >0)
DF2 <- DF2 %>% filter( BS < 500)
DF2 <- DF2 %>% filter(DP < 40) 
ggplot(DF2, aes(BS, DP)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
glin <- lin + geom_smooth(method = "glm")
glin
