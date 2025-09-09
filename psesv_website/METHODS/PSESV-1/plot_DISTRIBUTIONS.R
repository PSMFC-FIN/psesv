  ## plot_DISTRIBUTIONS() by Steven Barbeaux 7/19/2017
  ## Function to plot distribution of catch by temperature and depth or location and CPUE from Alaska Fisheries
  ## Science Center (AFSC) bottom trawl surveys. 
  ## Uses data from the Get_DATA.R function but must be limited to a single survey/species
  ## survey(52= AI , 98=EBS Shelf, 78= EBS slope, 46 = Western GOA, 47 = Central GOA, 48= Eastern GOA, 
  ## species (RACE species code)
  ## plotT  (1,3,5,7,9 = Depth and temperature, 2,4,6,8 = Location)
  ## cat1 = which length category (1-5) 1 smallest to 5 largest length category 





     plot_DISTRIBUTIONS<-function(data=results,survey,species,plotT,cat1){  
     
       location      <- data$location[SURVEY_DEFINITION_ID==survey]
       length        <- data$length[SURVEY_DEFINITION_ID==survey & SPECIES_CODE==species]
       location_poll <- data$location_poll[SURVEY_DEFINITION_ID==survey & SPECIES_CODE==species]
       SN            <- data$SN[SPECIES_CODE==species]
       
       cn      <- paste(SN$COMMON_NAME)
       sn      <- paste(SN$SPECIES_NAME)
      
       validate(
         need(sum(length$FREQUENCY)>=500,"Not enough measurements for this species and survey"),
         need(length(unique(length$YEAR))>=2,"Not enough measurements for this species and survey")
       )
       ## rounding bottom depth to nearest 0.5 meters and temperature to nearest 0.1 degree to simplify calculations
       location$DEPTHR <- round(location$DEPTH)
       location$TEMPR  <- round(location$TEMP,1)
       
       length$TEMPR    <- round(length$TEMP,1)
       length$DEPTHR   <- round(length$DEPTH)
       
       length          <- data.table(merge(length,location_poll,by = c("YEAR","LON","LAT","SURVEY_DEFINITION_ID","STRATUM", "SPECIES_CODE","CPUE")))
       
       
       x<-quantile(length$LENGTH,probs=c(0.1,0.3,0.7,0.9,1.0))
       bins2<-c(0,as.numeric(x))
       
       
       ## create length bins  
       for ( i in 2:length(bins2)){
         length$BIN[length$LENGTH<bins2[i] & length$LENGTH >= bins2[(i-1)]] <- bins2[(i-1)]
       }
       
       label<-array(dim=5)
       label2<-array(dim=5)
       
       
       lab1<-c(trunc(bins2[1:5]/10),max(length$LENGTH/10))
       lab2<-lab1-1
       lab2[1]<-0
       
       for(i in 1:4){
         label[i]<-paste(as.character(lab1)[i],"-",as.character(lab2)[i+1]," cm",sep="")
         label2[i]<-i
         
       }
       
       label[5]<-paste(as.character(lab1)[5],"-",as.character(lab1)[6]," cm",sep="")
       label2[5]<-5
       
       label<-data.frame(BIN=bins2[1:5],LABEL=as.factor(label)) 
       length<-length[!is.na(length$TEMPR)]
       length<-merge(length,label,by="BIN")
       
       
       
       data1<-list(location=location,length=length)
       td1 <- Get_TEMP(data=data1, plotT=F)
       td1$LON=1;td1$LAT=1; td1$DEPTHR=1;td1$TEMPR=1;td1$bin2=1;td1$CPUELAB="0"
       
       
       if(survey %in% c(46,47,48)){
         location <- location[YEAR>1990]
         length <- length[YEAR>1990]
         td1<-td1[YEAR>1990]
       }
       
       
       
       yers    <- unique(length$YEAR)
       yers2<- sort(unique(location$YEAR))
       
       if(survey==98 & !1994 %in% length$YEAR){ location<-location[YEAR %in% yers]}
       if(survey %in% c(46,47,48) & !1993 %in% length$YEAR){ location<-location[YEAR %in% yers]}
       if(survey==52 & !1994 %in% length$YEAR){ location<-location[YEAR %in% yers]}
       
       length <- length[complete.cases(length[,c('CPUE','TEMPR')])]
       
       
       rgb.palette <- colorRampPalette(c("yellow","gold","goldenrod2","brown"),space = "rgb") 
       
       color1      <- c("gray70",rgb.palette(5))
       
       ## Temperature plot
       if(plotT%in%c(1,3,5,7,9))
       { 
         
         ## rounding depth to 20 m increments for visualization purposes.
         location$DEPTHR <- round(location$DEPTH,-1)
         length$DEPTHR   <- round(length$DEPTH,-1)
         location$TEMPR <- round(location$TEMP,1)
         length$TEMPR   <- round(length$TEMP,1)
         
         location$T <-1
         location1  <- location[,list(NUMBER=sum(T)),by= 'YEAR,DEPTHR,TEMPR']
         data1      <- length[,list(FREQ=sum(FREQUENCY)),by= 'YEAR,BIN,LABEL,DEPTHR,TEMPR']
         data2      <- length[,list(SUM=sum(FREQUENCY),CPUE=mean(CPUE)),by = 'YEAR,DEPTHR,TEMPR']
         data3      <- merge(data1,data2,all=T,by=c("YEAR","DEPTHR","TEMPR"))
         data3      <- data3[,list(PLOT=mean((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,TEMPR,DEPTHR']
       } 
       
       ## Location plot 
       if(plotT%in% c(2,4,6,8))
       { 
         location1  <- location[,list(NUMBER=length(YEAR)),by= 'YEAR,LON,LAT']
         data1      <- length[,list(FREQ=sum(FREQUENCY)),by= 'YEAR,BIN,LABEL,LON,LAT']
         data2      <- length[,list(SUM=sum(FREQUENCY),CPUE=mean(CPUE)),by = 'YEAR,LON,LAT']
         data3      <- merge(data1,data2,all=T,by=c("YEAR","LON","LAT"))
         data3      <- data3[,list(PLOT=mean((FREQ/SUM)*CPUE)),by = 'YEAR,BIN,LABEL,LON,LAT']
       }
       
       
       data3<-data3[order(data3$PLOT),]
       
       bins2x     <- seq(0,max(log(data3$PLOT)),length=7)
       bins2      <- c(0,exp(bins2x[2:7]))
       
       for( j in 3:7){
         data3$bin2[data3$PLOT>bins2[j-1] & data3$PLOT<=bins2[j]]<-max(1,round(bins2[(j-1)]))
       }
       
       data3$bin2[is.na(data3$bin2)]<-0
       
       dataT <- data3[order(data3$bin2),]
       label2<-array(dim=6)
       label4<-array(dim=6)
       
       binsx<-sort(bins2)
       binsx[binsx>0&binsx<0.5]<-1
       binsx<-round(binsx)
       lab1<-binsx
       lab2<-lab1-1
       lab2[1]<-1
       lab1<-formatC(lab1,format="d",big.mark=",")
       lab2<-formatC(lab2,format="d",big.mark=",")
       
       label2[1]<-"0"
       label2[2]<-paste("b) 0.1 - ",lab2[3],sep="")
       
       test=c("x","a","b","c","d","e","f")
       
       for(i in 4:(length(lab1)-1)){
         label2[i-1]<-paste(test[i],") ",lab1[i-1]," - ",lab2[i],sep="")
       }
       
       label2[6]<-paste("f) ",lab1[6]," - ",lab1[7],sep="")
       
       label3<-data.frame(bin2=binsx[1:6],CPUELAB=label2)
       label3$ID<-as.numeric(as.factor(label3$CPUELAB))
       label3$ID2<-c(1:6)
       label3$color<-color1
       
       
       
       label4[1]<-"0"
       label4[2]<-paste("0.1 - ",lab2[3],sep="")
       
       for(i in 4:(length(lab1)-1)){
         label4[i-1]<-paste(lab1[i-1]," - ",lab2[i],sep="")
       }
       
       label4[6]<-paste(lab1[6]," - ",lab1[7],sep="")
       
       label3<-data.frame(bin2=binsx[1:6],CPUELAB=label2,CPUELAB2=label4)
       label3$ID<-as.numeric(as.factor(label3$CPUELAB))
       label3$ID2<-c(1:6)
       label3$color<-color1
       
       dataT<-merge(dataT,label3,by="bin2")
       
       
     
       dataT$CPUELAB <- factor(dataT$CPUELAB,levels=label3$CPUELAB)
       
       
       BINS<-sort(unique(dataT$BIN))
       
       dataT <- dataT[dataT$BIN==BINS[cat1]]
       
       l1<-as.character(unique(dataT$LABEL))
       
       title1 <- bquote(.(cn)~" ("~italic(.(sn))~") "~.(l1))
       
       
       if(plotT%in%c(1,3,5,7,9)){ dataT <- merge(location1,dataT,all.x=T,by=c("YEAR","TEMPR","DEPTHR"),allow.cartesian=TRUE)}
       if(plotT%in%c(2,4,6,8)){ dataT <- merge(location1,dataT,all.x=T,by=c("YEAR","LON","LAT"),allow.cartesian=TRUE)}
       
       dataT$PLOT[is.na(dataT$PLOT==T)] <- 0
       dataT$bin2[is.na(dataT$bin2==T)] <- 0
       dataT$CPUELAB[is.na(dataT$CPUELAB==T)] <- "0"
       dataT$CPUELAB2[is.na(dataT$CPUELAB2==T)] <- "0"
       dataT$ID2[is.na(dataT$ID2==T)] <- 1
       dataT$ID[is.na(dataT$ID==T)] <- 1
       dataT$color[is.na(dataT$color==T)] <- "gray80"
       dataT<-dataT[order(dataT$ID2)]
       
       dataT$CPUELAB <- factor(dataT$CPUELAB,levels=label3$CPUELAB)
       
       dataT$YEAR<-factor(dataT$YEAR,levels=yers2)
       td1$YEAR<-factor(td1$YEAR,levels=yers2)
       
       if(plotT%in%c(1,3,5,7,9))
       {
         nc=4
         d <- ggplot(data=dataT,aes(x=TEMPR,y=-DEPTHR,color=factor(CPUELAB),shape=factor(CPUELAB),size=factor(CPUELAB)))
         d <- d + geom_rect(data=td1,aes(fill=REGI),xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.1,linetype=0)
         d <- d + geom_point(data=dataT)
         d <- d + scale_x_continuous(breaks=seq(-1,20,by=1))
         d <- d + scale_size_manual(name=expression(paste("Number/k",m^2,sep="")),values=c(0.2,rep(1,(length(unique(dataT$bin2))-1))),labels=unique(dataT$CPUELAB2))
         d <- d + xlab(expression("Temperature ("* degree * C *")"))
         d <- d + ylab("Depth (m)")
       }
       
       if(plotT%in%c(2,4,6,8))
       {
         
         
         B_sea <- map_data("world2","USA:alaska")
         p <- ggplot()
         
         if(survey[1]==98){  p <- p + coord_fixed(ylim=c(51,65),xlim=c(179,205))
         nc=5}
         if(survey[1]==46){  p <- p + coord_fixed(ylim=c(50,58),xlim=c(190,201))
         nc=3}
         if(survey[1]==47){  p <- p + coord_fixed(ylim=c(52,61.5),xlim=c(200,213))
         nc=3}
         if(survey[1]==48){  p <- p + coord_fixed(ylim=c(53,61.5),xlim=c(213,230))
         nc=3}
         if(survey[1]==52){  p <- p + coord_fixed(ylim=c(51,55),xlim=c(170,195))
         nc=2}
         if(survey[1]==78){  p <- p + coord_fixed(ylim=c(51,63),xlim=c(175,195))
         nc=2}
         
         d <- p + geom_polygon(data=B_sea,aes(x=long,y=lat,group=group))
         d <- d + geom_point(data=dataT,aes(x=LON,y=LAT,color=CPUELAB,shape=CPUELAB,size=CPUELAB))
         d <- d + geom_rect(data=td1,aes(fill=REGI),xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.1,linetype=0)
         
         if(survey[1]==98){  d <- d + scale_x_continuous(breaks=seq(180,205,by=5),labels=seq(180,155,by=-5)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
         if(survey[1]==46){  d <- d + scale_x_continuous(breaks=seq(190,216,by=2),labels=seq(170,144,by=-2)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
         if(survey[1]==47){  d <- d + scale_x_continuous(breaks=seq(190,216,by=2),labels=seq(170,144,by=-2)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
         if(survey[1]==48){  d <- d + scale_x_continuous(breaks=seq(190,230,by=2),labels=seq(170,130,by=-2)) + ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
         
         
         if(survey[1]==52){  d <- d + scale_x_continuous(breaks=seq(170,195,by=5),labels=c(170,175,seq(180,165,by=-5))) +ylab(expression(paste("Latitude ",degree," N",sep="")))+xlab(expression(paste("Longitude",sep=""))) }
         if(survey[1]==78){  d <- d + scale_x_continuous(breaks=seq(175,195,by=5),labels=c(175,180,175,170,165)) +ylab(expression(paste("Latitude ",degree,sep="")))+xlab(expression(paste("Longitude ",degree," W",sep=""))) }
         d <- d + scale_size_manual(name=expression(paste("Number/k",m^2,sep="")),values=c(0.1,rep(1,(length(unique(dataT$bin2))-1))),labels=unique(dataT$CPUELAB2))
         
       }
       
       d <- d + scale_fill_manual(name="Shelf temp.",values=c("light blue","white","salmon"))
       d <- d + scale_color_manual(name=expression(paste("Number/k",m^2,sep="")),values=color1[sort(unique(dataT$ID))],labels=unique(dataT$CPUELAB2))
       d <- d + scale_shape_manual(name=expression(paste("Number/k",m^2,sep="")),values=c(3,rep(16,(length(unique(dataT$bin2))-1))),labels=unique(dataT$CPUELAB2))
       d <- d + ggtitle(title1)
       
       d <- d + theme(panel.spacing=unit(0,"lines"),
                      panel.background = element_rect(fill = 'white', color = 'white'),
                      plot.title=element_text(vjust=1,hjust=0),
                      legend.key=element_rect(fill = 'white', color = 'white',linetype=0),
                      legend.key.width=unit(1,"cm"),
                      axis.text=element_text(size=6))
       d <- d + guides(colour = guide_legend(keyheight=2,override.aes = list(size=1)))
       d <- d + facet_wrap(~YEAR,ncol=nc,shrink=FALSE, drop=F) #ncol=nc
       d <- d + theme1(base_size=20)
       
       print(d)
     })