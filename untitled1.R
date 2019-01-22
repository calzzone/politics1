library(reshape2)

x = "Winner"
y = c("Democrat %", "Republican %")
df <- baza_de_date %>% 
  dplyr::select(c(x, y)) %>%
  tidyr::gather(variable, value, -x)

g1 <- ggplot(df)+ theme_pubclean()+
  geom_boxplot(aes(x=variable, y=value))+
  facet_grid(.~Winner)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name="% of votes received") + 
  theme(axis.title.x = element_blank())+
  labs(subtitle=paste(x, ":", sep=""))

#plot(g1)



g2 <- ggplot(df) + aes(x=Winner, y=value, fill=Winner) +
  theme_grey() +
  geom_violin(alpha=0.25) +
  geom_boxplot(alpha=0.75, width=0.25, varwidth = T) +
  geom_rug(aes(color=Winner),alpha=0.5, sides = "l")+
  stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3) + 
  facet_grid(.~variable)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name="% of votes received") + 
  #theme(axis.title.x = element_blank())+
  labs(subtitle="Party:")+
  theme(legend.position = "none")

#plot(g2)



g3 <- ggplot(df) + aes(fill=Winner, y=value, x=variable) +
  #theme_grey() + 
  #theme_pubclean()+
  theme_bw()+
  geom_split_violin(alpha=0.25) +
  geom_boxplot(alpha=0.75, width=0.25, varwidth = T) +
  geom_rug(aes(color=Winner), alpha=0.5,sides = "lr")+
  stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name="% of votes received") + 
  theme(legend.position = "top") +
  theme(axis.title.x = element_blank())

plot(g3)
