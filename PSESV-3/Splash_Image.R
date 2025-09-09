library(tidyverse)

data <- readRDS("Data/temperature_data.RDS") %>% 
  left_join(readRDS("Data/date_data.RDS")) %>% 
  left_join(readRDS("Data/spatial_data.RDS")) %>% 
  data.frame %>% 
  mutate(date=as.Date(date))

png("PSESV_Splash_Image.png",width=1200,height=800)
data %>% 
  filter(m.depth<=200 & year!=2019) %>% 
  group_by(NMFSAREA,year) %>% 
  summarise(meantemp=mean(sst.mean)) %>% 
  ungroup %>% 
  group_by(NMFSAREA) %>% 
  mutate(index=1:n(),
         tempanom=(meantemp-mean(meantemp))/sd(meantemp)) %>% 
  filter(NMFSAREA%in%c("659","630","517","543")) %>% 
  ggplot(aes(year,tempanom,fill=factor(NMFSAREA),width=0.65)) + 
  geom_bar(stat="identity",position="dodge") + 
  theme_bw() + 
  theme(legend.position=c(0.25,0.9),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank()) + 
  scale_fill_viridis_d(name="NMFS Area") + 
  scale_x_continuous(breaks = 2003:2017, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017")) + 
  ylab("Temperature Anomaly") + 
  guides(fill=guide_legend(ncol=4)) 
dev.off()