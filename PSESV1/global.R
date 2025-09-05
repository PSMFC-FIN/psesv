require(shiny)
require(shinydashboard)
require(lattice)
require(data.table)
require(maps)
require(ggplot2)
require(grid)
require(sp)
require(rgdal)
require(dplyr)

results<-readRDS('data/results.rds')




weighted.var.se <- function(x, w, na.rm=FALSE)
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}

plot_BIOM<-function(data=biomass,sn=SN,ptype="B"){
  
  data$PHIGH<-data$POP+1.96*sqrt(data$POP_VAR)
  data$PLOW<-data$POP-1.96*sqrt(data$POP_VAR)
  
  if(ptype=="B"){
    data$BHIGH<-data$BIOM+1.96*sqrt(data$BIO_VAR)
    data$BLOW<-data$BIOM-1.96*sqrt(data$BIO_VAR)
    d<-ggplot(data,aes(x=YEAR,y=BIOM))
    d<-d+geom_errorbar(aes(ymin=BLOW,ymax=BHIGH,width=0.1))
    d<-d+xlab("Year")+ylab("Total biomass (t)")
  }
  
  if(ptype=="P"){
    data$PHIGH<-data$POP+1.96*sqrt(data$POP_VAR)
    data$PLOW<-data$POP-1.96*sqrt(data$POP_VAR)
    d<-ggplot(data,aes(x=YEAR,y=POP))
    d<-d+geom_errorbar(aes(ymin=PLOW,ymax=PHIGH,width=0.1))
    d<-d+xlab("Year")+ylab("Population number")
  }
  d<-d+geom_line(size=1.5,color="gray65")
  d<-d+geom_point(size=3.5,color="blue")
  d<-d+ggtitle(paste(sn,unique(data$AREA)))
  d<- d + expand_limits(y=0)
  d <- d + theme1(base_size=20)
  print(d)
}



Get_TEMP<-function(data=results,plotT=F){
  
  require(data.table)
  require(ggplot2 )
  
  location      <- data.table(data$location)
  survey2        <- unique(data$length$SURVEY_DEFINITION_ID)
  location <- location[SURVEY_DEFINITION_ID==survey2]
  
  
  SURVEY1  <- c(rep(47,59),rep(52,49),rep(98,18))
  
  ## strata ids
  STRATA1 <- c(10,11,12,13,20,21,22,30,31,32,33,35,40,41,50,110,111,112,120,121,122,130,131,132,133,134,140,141,142,143,150,151,
               210,220,221,230,231,232,240,241,250,251,310,320,330,340,341,350,351,410,420,430,440,450,510,520,530,540,550,211,
               212,213,214,221,222,223,224,311,312,313,314,321,322,323,324,411,412,413,414,421,422,423,424,511,512,513,521,522,523,
               594,611,612,613,614,621,622,623,624,711,712,721,722,793,794,811,812,813,814,10,20,31,32,41,42,43,50,61,62,70,81,82,90,
               140,150,160,170)
  
  ## strata areas km^2
  AREA1 <- c(8192.574997,13682.65625,6876.381836,12399.20117,7941.368652,7302.438965,10792.57031,5766.484863,15403.96484,9887.424805,
             5260.411179,2199.774414,9947.788055,6714.710144,6546.749786,4247.325684,8152.350098,2278.351074,11104.63574,7735.040283,
             5010.910645,7912.287903,7337.189453,10981.39355,12077.79492,5025.512177,7346.035156,5276.860352,9032.429688,7727.947754,
             4196.598648,6888.172363,2788.070313,10018.25,1528.385864,6659.834412,1622.698395,3208.446533,3043.014069,2127.350769,
             1125.320068,3927.273926,2531.211182,1604.039185,2911.924805,1107.272964,1520.634399,2344.441895,772.6966553,2005.69043,
             1953.303955,1744.927856,1469.337891,1033.513306,1937.304199,3065.696777,3494.051025,1887.523682,1206.404175,3694.263541,
             4064.2204,940.2248052,1711.085075,1183.328831,1252.458884,783.4029729,1561.38569,960.0232771,1735.543661,766.4301818,
             1237.474859,2105.571189,1066.71488,438.3369788,1239.741186,1617.722884,1052.298382,426.0343037,789.2104159,1164.046529,
             751.7920926,477.3601063,714.3037925,1944.355707,1583.079015,155.9293403,1268.052932,2012.546625,1968.562267,2669.984931,
             1908.646009,2260.881404,716.3752055,438.2089785,1760.938457,1861.249777,2079.252159,2574.749486,1585.638665,669.5128599,
             2440.256707,1179.114117,563.8576854,1043.172833,0.49439225,346.4207737,485.4830076,1419.98111,78702.648,41328.676,94983.172,
             8935.522,62875.391,24242.438,21319.879,38989.602,88753.977,6462.794,73353.117,35392.938,20897.016,11542.001,88280.469,25800.449,
             41680.52,20115.686)
  
  
  area<-data.table(SURVEY_DEFINITION_ID=SURVEY1,STRATUM=STRATA1,AREA=AREA1)
  
  
  if(survey2 %in% c(46,47,48)){
    area47<-area[SURVEY_DEFINITION_ID==47]
    area<-area[SURVEY_DEFINITION_ID==52]
    area47$SL1=trunc(area47$STRATUM/10)-(trunc(area47$STRATUM/100)*10)
    area47<- area47[!is.na(STRATUM)&STRATUM<400]
    area47[area47$SL1>=4]$SURVEY_DEFINITION_ID <-48
    area47[area47$SL1==1]$SURVEY_DEFINITION_ID <-46
    area47$SL1<-NULL
    area<-data.table(rbind(area,area47))
  }
  
  
  area<-area[SURVEY_DEFINITION_ID %in% survey2]
  ## limit AI survey so does not include Bowers Ridge which was not consistently surveyed
  area <- area[STRATUM < 800]
  
  if(78 %in% survey2){area=data.table(SURVEY_DEFINITION_ID=78,STRATUM=unique(data$location$STRATUM),AREA=1)}
  
  
  
  loca<-location[!is.na(TEMP)]
  loca<-loca[!is.na(STEMP)]
  loca<-subset(loca,loca$STRATUM<800)
  
  loca<-merge(loca,area,by="STRATUM",all.x=T)
  
  loca<-loca[!is.na(AREA)]
  ar1<-loca[,list(AREA=max(AREA)),by='YEAR,STRATUM']
  ar2<-ar1[,list(AREA2=sum(AREA)),by='YEAR']
  ar3<-merge(ar2,ar1,by="YEAR")
  ar3$PROP_AREA<-ar3$AREA/ar3$AREA2
  ar3<-subset(ar3,select=-c(AREA2,AREA))
  
  ## calculating mean temperature based on Spencer(2009)
  
  loca<-merge(loca,ar3,by=c("YEAR","STRATUM"))
  t1<-loca[,list(NUM=length(TEMP)),by='YEAR,STRATUM,PROP_AREA']
  t1$W<-t1$PROP_AREA/t1$NUM
  t1<-subset(t1,select=-c(NUM,PROP_AREA))
  t2<-merge(loca,t1,by=c("YEAR","STRATUM"))
  t2$T1<-t2$W*t2$TEMP
  t2$D1<-t2$W*t2$DEPTH
  t3<-t2[,list(MTEMP=sum(T1),MDEPTH=sum(D1)),by='YEAR']
  
  xTEMP<-mean(t3$MTEMP)
  sdTEMP<-sd(t3$MTEMP)
  t3$REGI="MED"
  t3$REGI[t3$MTEMP<=(xTEMP-(0.66*sdTEMP))]="COLD"
  t3$REGI[t3$MTEMP>=(xTEMP+(0.66*sdTEMP))]<-"WARM"
  
  t3$xtemp<-t3$MTEMP-mean(t3$MTEMP)
  t3$SD<-sdTEMP
  t3$MT<-xTEMP
  
  t3$TEMP1<-"gray80"
  t3$TEMP1[t3$REGI=="MED"]<-"gray50"
  t3$TEMP1[t3$REGI=="COLD"]<-"gray20"
  
  t3$TEMP2<-"salmon"
  t3$TEMP2[t3$REGI=="MED"]<-"gray50"
  t3$TEMP2[t3$REGI=="COLD"]<-"light blue"
  
  
  if(plotT==T){
    
    plot.title=bquote("Area weighted mean bottom temp.="~.(round(xTEMP,1))~degree~"C")
    d <- ggplot(data=t3,aes(y=xtemp,x=YEAR,xend=YEAR,yend=0,color=REGI,shape=REGI))
    d <- d + geom_segment(col="gray10",size=0.1)
    d <- d + geom_point(size=3)
    d <- d + scale_colour_manual(name="Shelf temp.",values=c("blue","gray85","red"))+scale_shape_manual(name="Shelf temp.",values=c(15,17,16))
    d <- d + xlab("Year")
    d <- d + ylab(bquote("Temp. Anomaly ("~degree~"C)"))
    d <- d + ggtitle(plot.title) 
    d <- d + theme1(base_size=20)
    print(d)
    
    
    
  }
  t3
}


expand.length<-function(data)
  {
  data1<-data[,list(FREQUENCY=sum(FREQUENCY)),by="LENGTH"]
  x<-rep(data1$LENGTH[1],data1$FREQUENCY[1])
  for( i  in 2:nrow(data1))
  {
    x1<-rep(data1$LENGTH[i],data1$FREQUENCY[i])
    x<-c(x,x1)
  }
  
  y<-data.frame(LENGTH=x)
  y
}

theme1=function (base_size = 10, base_family = "serif") 
{
  half_line <- base_size/2
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"), 
        rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1), 
        text = element_text(family = base_family , face = "plain", colour = "black", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE), 
        axis.line = element_blank(), 
        axis.line.x = NULL, 
        axis.line.y = NULL, 
        axis.text = element_text(size = rel(0.5), colour = "grey30"), 
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1), 
        axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0), 
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1), 
        axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0), 
        axis.ticks.length = unit(half_line/2, "pt"), 
        axis.title = element_text(size=rel(0.75)),
        axis.title.x = element_text(margin = margin(t = half_line), vjust = 1), 
        axis.title.x.top = element_text(margin = margin(b = half_line),vjust = 0), 
        axis.title.y = element_text(angle = 90, margin = margin(r = half_line), vjust = 1), 
        axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0), 
        axis.ticks = element_blank(), 
        
        legend.background = element_blank(), 
        legend.key = element_blank(),
        legend.spacing = unit(0.2, "cm"), 
        legend.spacing.x = NULL, 
        legend.spacing.y = NULL, 
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"), 
        legend.key.size = unit(0.75, "lines"), 
        legend.key.height = unit(0.75, "lines"), 
        legend.key.width = NULL, 
        legend.text = element_text(size = rel(0.5)), 
        legend.text.align = NULL, 
        legend.title = element_text(hjust = 0.2,size=rel(0.75)), 
        legend.title.align = NULL, 
        legend.position = "right", 
        legend.direction = NULL, 
        legend.justification = "center", 
        legend.box = NULL, 
        legend.box.margin = margin(0, 0, 0, 0, "cm"), 
        legend.box.background = element_blank(), 
        legend.box.spacing = unit(0.2, "cm"), 
        
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = "grey95",size=0.1), 
        panel.grid.minor = element_line(colour = "grey95", size = 0.01), 
        panel.spacing = unit(0,"null"), 
        panel.spacing.x = NULL, 
        panel.spacing.y = NULL, 
        panel.ontop = FALSE, 
        
        strip.background = element_blank(), 
        strip.text = element_text(colour = "grey10", size = rel(0.75)), 
        strip.text.x = element_text(margin = margin(t = half_line, b = half_line)), 
        strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)), 
        strip.placement = "inside", 
        strip.placement.x = NULL, 
        strip.placement.y = NULL, 
        strip.switch.pad.grid = unit(0.03, "cm"), 
        strip.switch.pad.wrap = unit(0.08, "cm"), 
        
        plot.title = element_text(size = rel(0.9), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.75)), 
        plot.subtitle = element_text(size = rel(0.5), hjust = 0, vjust = 1, margin = margin(b = half_line * 0.5)), 
        plot.caption = element_text(size = 12, hjust = 0, vjust = 1, margin=unit(c(0,0,0,0),"inches")), 
        plot.margin = unit(c(0, 0, 0,0),"inches"),
        plot.background = element_blank(), 
        complete = TRUE)
}

SURVEY_AREA <- function(location=location, surveyx=survey){
    SURVEY1  <- c(rep(47,59),rep(52,49),rep(98,18))

## strata ids
    STRATA1 <- c(10,11,12,13,20,21,22,30,31,32,33,35,40,41,50,110,111,112,120,121,122,130,131,132,133,134,140,141,142,143,150,151,
    210,220,221,230,231,232,240,241,250,251,310,320,330,340,341,350,351,410,420,430,440,450,510,520,530,540,550,211,
    212,213,214,221,222,223,224,311,312,313,314,321,322,323,324,411,412,413,414,421,422,423,424,511,512,513,521,522,523,
    594,611,612,613,614,621,622,623,624,711,712,721,722,793,794,811,812,813,814,10,20,31,32,41,42,43,50,61,62,70,81,82,90,
    140,150,160,170)

## strata areas km^2
    AREA1 <- c(8192.574997,13682.65625,6876.381836,12399.20117,7941.368652,7302.438965,10792.57031,5766.484863,15403.96484,9887.424805,
    5260.411179,2199.774414,9947.788055,6714.710144,6546.749786,4247.325684,8152.350098,2278.351074,11104.63574,7735.040283,
    5010.910645,7912.287903,7337.189453,10981.39355,12077.79492,5025.512177,7346.035156,5276.860352,9032.429688,7727.947754,
    4196.598648,6888.172363,2788.070313,10018.25,1528.385864,6659.834412,1622.698395,3208.446533,3043.014069,2127.350769,
    1125.320068,3927.273926,2531.211182,1604.039185,2911.924805,1107.272964,1520.634399,2344.441895,772.6966553,2005.69043,
    1953.303955,1744.927856,1469.337891,1033.513306,1937.304199,3065.696777,3494.051025,1887.523682,1206.404175,3694.263541,
    4064.2204,940.2248052,1711.085075,1183.328831,1252.458884,783.4029729,1561.38569,960.0232771,1735.543661,766.4301818,
    1237.474859,2105.571189,1066.71488,438.3369788,1239.741186,1617.722884,1052.298382,426.0343037,789.2104159,1164.046529,
    751.7920926,477.3601063,714.3037925,1944.355707,1583.079015,155.9293403,1268.052932,2012.546625,1968.562267,2669.984931,
    1908.646009,2260.881404,716.3752055,438.2089785,1760.938457,1861.249777,2079.252159,2574.749486,1585.638665,669.5128599,
    2440.256707,1179.114117,563.8576854,1043.172833,0.49439225,346.4207737,485.4830076,1419.98111,78702.648,41328.676,94983.172,
    8935.522,62875.391,24242.438,21319.879,38989.602,88753.977,6462.794,73353.117,35392.938,20897.016,11542.001,88280.469,25800.449,
    41680.52,20115.686)

  
    
    MLON <- c(-166.6617708,-163.6476296,-161.4822643,-160.2728159,-156.7828336,-157.3683751,-154.9223253,-152.6896913,-151.8379673,-152.672754,
      -149.8345704,-153.4468568,-140.5223236,-145.8778702,-134.5579888,-161.2672718,-165.0450366,-159.5878392,-158.1528186,-155.9324919,-156.252355,
      -151.8777104,-149.7504065,-152.3229347,-149.0450957,-151.0683878,-145.9040637,-142.4291939,-139.9974508,-138.1406222,-135.8037049,-133.9084004,
      -165.4569163,-155.9348879,-156.3823498,-149.0602757,-151.6692325,-154.5676401,-141.0042489,-142.1295457,-135.9979079,-134.2214493,-164.1214793,
      -156.4576697,-150.3275524,-140.8184448,-142.8898086,-134.4814874,-135.1753093,-164.3434638,-155.5425212,-150.3943997,-142.3190627,-135.3420479,
      -164.8314867,-155.8779407,-150.0201444,-142.513717,-135.3234314,173.3881886,173.796178,173.6635595,173.3849459,176.1424686,176.1909145,176.0850644,
      175.933088,179.9901668,-179.8574816,179.9209679,179.7672396,179.5231324,-178.6220935,179.5321244,179.6071873,177.8881103,178.459149,177.6351318,
      178.1082596,-178.0553428,-178.692541,-178.2413441,-178.455635,-176.0308045,-176.0953212,-176.0771081,-172.3210574,-172.0086612,-171.6957753,
      -171.9632078,-176.289278,-176.2246774,-176.2857043,-175.7601399,-171.5418057,-170.8490782,-171.4550575,-170.5258444,-171.2892762,-169.696056,
      -167.6089796,-167.81809,-169.0842485,-171.3196929,-167.8068078,-167.2005608)


    MLAT <- c(53.6409009,54.26094781,55.2790761,54.93819047,56.99324408,55.5286317,56.46420692,57.70591141,57.36668098,59.5079296,59.58151775,57.84068084,
      59.51473373,59.97171241,56.34172313,54.59499901,53.68266936,55.44788092,55.69202414,56.72991197,55.47761648,57.48629165,58.55556059,58.71007181,
      59.42884375,57.36713146,59.83732587,59.67820496,59.06626687,58.36763463,57.00501678,55.27119429,53.53529033,56.55032889,55.3410874,58.81694652,
      56.945661,57.81376103,59.31411108,59.1251202,57.05094141,55.24904927,53.73786319,55.27264158,57.60324764,59.08873112,59.20206229,55.77366343,
      55.98356179,53.66707973,55.50472392,57.54089219,59.15826174,56.04832189,53.52551075,55.2785169,57.59974611,59.04025328,55.99554191,52.60427598,
      52.5858388,52.53411331,52.51947506,52.09546259,52.02363862,52.07515893,52.09398121,52.29301029,52.34790816,52.26881412,52.33082992,51.9353115,
      51.99732434,51.90603003,51.82118835,51.7132217,51.60114749,51.82237297,51.67854838,51.55474128,51.46179849,51.49570399,51.44924811,52.02328232,
      51.99507034,52.0558378,52.46664779,52.59261883,52.58709504,52.61347223,51.70991719,51.76009393,51.73003927,51.67680813,52.43534221,52.44107495,
      52.32782566,52.41011041,52.78583381,52.99544873,53.79808118,53.7298816,53.44490264,53.0001767,53.39750723,53.81550036)

    STRATA2<-c(10,11,12,13,20,21,22,30,31,32,33,35,40,41,50,110,111,112,120,121,122,130,131,132,133,134,140,141,142,143,150,151,210,220,221,230,231,232,
      240,241,250,251,310,320,330,340,341,350,351,410,420,430,440,450,510,520,530,540,550,211,212,213,214,221,222,223,224,311,312,313,314,321,
      322,323,324,411,412,413,414,421,422,423,424,511,512,513,521,522,523,594,611,612,613,614,621,622,623,624,711,712,721,722,
      793,794,811,812)


    MLOC<-data.table(SURVEY_DEFINITION_ID=c(rep(47,59),rep(52,47)),STRATUM=STRATA2,MLON=MLON,MLAT=MLAT)
    

    
    if(surveyx %in% c(46,47,48)){
      MLOC47<- MLOC[SURVEY_DEFINITION_ID==47]
      MLOC  <- MLOC[SURVEY_DEFINITION_ID==52]
      MLOC47$SL1=trunc(MLOC47$STRATUM/10)-(trunc(MLOC47$STRATUM/100)*10)
      MLOC47<- MLOC47[!is.na(STRATUM)&STRATUM<400]
      
      MLOC47[MLOC47$SL1>=4]$SURVEY_DEFINITION_ID <-48
      
      MLOC47[MLOC47$SL1==1]$SURVEY_DEFINITION_ID <-46
      
      MLOC47$SL1<-NULL
      MLOC<-data.table(rbind(MLOC,MLOC47))
    }
    
    

  #LAT1


  area<-data.table(SURVEY_DEFINITION_ID=SURVEY1,STRATUM=STRATA1,AREA=AREA1)
  
  
  if(surveyx %in% c(46,47,48)){
    area47<-area[SURVEY_DEFINITION_ID==47]
    area<-area[SURVEY_DEFINITION_ID==52]
    area47$SL1=trunc(area47$STRATUM/10)-(trunc(area47$STRATUM/100)*10)
    area47<- area47[!is.na(STRATUM)&STRATUM<400]
    area47[area47$SL1>=4]$SURVEY_DEFINITION_ID <-48
    area47[area47$SL1==1]$SURVEY_DEFINITION_ID <-46
    area47$SL1<-NULL
    area<-data.table(rbind(area,area47))
  }
  
  
  
  
  
  area<-area[SURVEY_DEFINITION_ID==surveyx]
  MLOC<-MLOC[SURVEY_DEFINITION_ID==surveyx]
  ## limit AI survey so does not include Bowers Ridge which was not consistently surveyed
    area<-subset(area,area$STRATUM<800)
    MLOC<-subset(MLOC,MLOC$STRATUM<800)
    
    if(surveyx==78){area=data.table(SURVEY_DEFINITION_ID=78,STRATUM=unique(location$STRATUM),AREA=1)}
   
   loca<-location[complete.cases(location[,c('TEMP','DEPTH')])]
   loca<-subset(loca,loca$STRATUM<800)

  loca<-merge(loca,area,by=c("SURVEY_DEFINITION_ID","STRATUM"),all.x=T)

  loca<-loca[!is.na(AREA)]
  

  ar1<-loca[,list(AREA=max(AREA)),by='YEAR,STRATUM,SURVEY_DEFINITION_ID']
  ar2<-ar1[,list(AREA2=sum(AREA)),by='YEAR,SURVEY_DEFINITION_ID']
  ar3<-merge(ar2,ar1,by=c("YEAR","SURVEY_DEFINITION_ID"))
  ar3$PROP_AREA<-ar3$AREA/ar3$AREA2
  ar3<-subset(ar3,select=-c(AREA2,AREA))

  loca<-merge(loca,ar3,by=c("YEAR","SURVEY_DEFINITION_ID","STRATUM"))

  MDEPTH<-loca[,list(SDEPTH=mean(DEPTH)),by='SURVEY_DEFINITION_ID,STRATUM']

  t1<-loca[,list(NUM=length(TEMP)),by='YEAR,SURVEY_DEFINITION_ID,STRATUM,PROP_AREA']
  t1$W<-t1$PROP_AREA/t1$NUM
  t1<-subset(t1,select=-c(NUM,PROP_AREA))
  t2<-merge(loca,t1,by=c("YEAR","SURVEY_DEFINITION_ID","STRATUM"))
  t2<-merge(t2,MDEPTH,by=c("SURVEY_DEFINITION_ID","STRATUM"))
  t2<-merge(t2,MLOC,by=c("SURVEY_DEFINITION_ID","STRATUM"))
  t2$SSDEPTH<-sum(t2$W*t2$SDEPTH)/sum(t2$W)
  
  return (t2)
}


expand.length2<-function(data)
{
  data1<-data[,list(FREQUENCY=sum(FREQUENCY)),by="LENGTH,SEX,YEAR"]
  x<-rep(data1$LENGTH[1]/10,data1$FREQUENCY[1])
  sx<-rep(data1$SEX[1],data1$FREQUENCY[1])
  yr<-rep(data1$YEAR[1],data1$FREQUENCY[1])
  
  for( i  in 2:nrow(data1))
  {
    x1<-rep(data1$LENGTH[i]/10,data1$FREQUENCY[i])
    sx1<-rep(data1$SEX[i],data1$FREQUENCY[i])
    yr1<-rep(data1$YEAR[i],data1$FREQUENCY[i])
    x<-c(x,x1)
    sx<-c(sx,sx1)
    yr<-c(yr,yr1)
  }
  
  y<-data.frame(YEAR=yr,SEX=sx,LENGTH=x)
  y
}

plot_DIST<-function(data=data1,sn=cn,area=AREA){

  data2<- data
  data2$LENGTH=data2$LENGTH/10
  data3<-data2[,list(FREQUENCY=sum(FREQUENCY)),by=c('SEX,YEAR,LENGTH')]
  ## making usexed 50/50 male female
  datax<-data3[SEX==3]
  datax1<-data3[SEX!=3]
  datax$FREQUENCY<-datax$FREQUENCY/2
  datax$SEX<-1
  dataxF<-datax
  dataxF$SEX<-2
  datax=rbind(datax1,datax,dataxF)
  data3<-datax[,list(FREQUENCY=sum(FREQUENCY)),by=c('SEX,YEAR,LENGTH')]
  
  gridx<-expand.grid(YEAR=sort(unique(data2$YEAR)),SEX=c(1,2),LENGTH=seq(1,max(data2$LENGTH)))
  
  data3<-merge(data3,gridx,all=T,by=c("YEAR","SEX","LENGTH"))
  data3$FREQUENCY[is.na(data3$FREQUENCY)]<-0
  
  
  data4 <- data2[,list(TOTAL=sum(FREQUENCY)),by=c('YEAR')]
  
  data2<-merge(data3,data4,by="YEAR")
  data2$DENSITY=data2$FREQUENCY/data2$TOTAL
  
  
  data2$SEX2<-"Female"
  data2$SEX2[data2$SEX==1]<-"Male"
  
  
  
  data3<-data.frame(x=data2$YEAR,y=data2$LENGTH,Sex=as.factor(data2$SEX2),dens=data2$DENSITY*10)
  data3<-data3[order(data3$x,data3$y),]
  data3$x2<-as.numeric(as.factor(data3$x))
  
  
  
  nm<-length(unique(data3$x))
  
  
  data3$dens <- ifelse(data3$Sex == 'Male', data3$dens * -1, data3$dens)
  data3$dens <-(data3$dens+data3$x2)
  
  
  
  
  ggplot(data3, aes(dens, y, color=Sex,fill = Sex, group = interaction(Sex, x2))) + 
    geom_polygon() +
    scale_x_continuous(breaks = seq(1,nm,by=1), labels = sort(unique(data3$x))) +
    xlab('Year')+
    ylab('Length (cm)') +
    theme1(base_size=20)+
    scale_colour_manual(name="Sex",values=c("salmon","light blue"))+
    ggtitle(paste(sn,"in",area))
    
}
