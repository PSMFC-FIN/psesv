## Created 1/19/2018 by Steve Barbeaux 
## This function pulls RACE survey haul and length data for later analysis for all species
## Username and Password are AKFIN database username and password
## survey 52=AI,98=EBS Shelf, 78=EBS Slope, and 47 = GOA
## yr (years for data extraction)
## minN(minimum number of lengths measured over all years for a species to be included)



Get_DATA<- function(username="",password="",species=21740,survey = 78 ,yr=c(1982:2016),minN=2000) {

  require(RODBC)
  require(data.table)

  years<-paste(yr,collapse=",")

  if(R.Version()$arch=="i386"){
    AKFIN=odbcConnect("AKFIN",username,password)
    }
  else {
    AKFIN=odbcConnect("AKFIN",username,password,believeNRows=FALSE)
    }


    if(survey %in% c(52,47)){
    haul <- "AFSC.RACE_HAULAIGOA"
    catch <- "AFSC.RACE_CPUEAIGOA"
    length1 <- "AFSC.RACE_LENGTHAIGOA"
    if(survey == 52) area<-"IN ('AI')"
    if(survey == 47) area<-"IN ('GOA')"

  }

  if(survey %in% c(98)){
    haul <- "AFSC.RACE_HAUL_EBSSHELF"
    catch <- "AFSC.RACE_CPUE_EBSSHELF"
    length1 <- "AFSC.RACE_LENGTH_EBSSHELF"
    area   <- "IN ('EBS_SHELF')"
  }

  if(survey %in% c(78)){
    haul <- "AFSC.RACE_HAUL_EBSSLOPE"
    catch <- "AFSC.RACE_CPUE_EBSSLOPE"
    length1 <- "AFSC.RACE_LENGTH_EBSSLOPE"
    area <- " IS NULL"
  }

     if("ALL" %in% species){
      end1  <- ""
      end2 <- ""
      }
      else {
      end1 <- paste0("AND ",noquote(length1),".SPECIES_CODE IN (",noquote(species),")")
      end2 <- paste0("AND ",noquote(catch),".SPECIES_CODE IN (",noquote(species),")")
      end3 <- paste0("WHERE AFSC.RACE_RACESPECIESCODES.SPECIES_CODE IN (",noquote(species),")")
    }

    spec<-paste0("SELECT AFSC.RACE_RACESPECIESCODES.COMMON_NAME, \n",
    "AFSC.RACE_RACESPECIESCODES.SPECIES_NAME, \n",
    "AFSC.RACE_RACESPECIESCODES.SPECIES_CODE \n",
    "FROM AFSC.RACE_RACESPECIESCODES", end3)

    test<-paste0("SELECT TO_CHAR(",noquote(haul),".START_TIME, 'yyyy') AS YEAR, \n ",
      " ",noquote(haul),".REGION, \n ",
      " ",noquote(haul),".STRATUM,\n ",
      " ",noquote(haul),".GEAR_TEMPERATURE AS TEMP, \n ",
      " ",noquote(haul),".SURFACE_TEMPERATURE AS STEMP, \n ",
      " ",noquote(haul),".BOTTOM_DEPTH AS DEPTH, \n ",
      " ",noquote(haul),".END_LATITUDE AS LAT, \n ",
      " ",noquote(haul),".END_LONGITUDE AS LON \n ",
      "FROM ",noquote(haul)," \n ",
      "WHERE ",noquote(haul),".REGION ", noquote(area)," \n ",
      "AND TO_CHAR(",noquote(haul),".START_TIME, 'yyyy') IN (",noquote(years),")" ,sep="")  

    test2<-paste0("SELECT TO_CHAR(",noquote(haul),".START_TIME, 'yyyy') AS YEAR, \n",
      " ",noquote(haul),".REGION,\n",
      " ",noquote(haul),".STRATUM,\n",
      " ",noquote(haul),".END_LATITUDE AS LAT, \n",
      " ",noquote(haul),".END_LONGITUDE AS LON, \n",
      " ",noquote(catch),".NUMCPUE AS CPUE, \n",
      " ",noquote(catch),".SPECIES_CODE \n",
      "FROM ",noquote(catch)," \n ",
      "INNER JOIN ",noquote(haul)," \n",
      "ON ",noquote(catch),".HAULJOIN = ",noquote(haul),".HAULJOIN \n",
      "WHERE ",noquote(haul),".REGION ", noquote(area)," \n ",
      "AND TO_CHAR(",noquote(haul),".START_TIME, 'yyyy') IN (",noquote(years),") \n ",
      end2)
    
    test3<-paste0("SELECT TO_CHAR(",noquote(haul),".START_TIME, 'yyyy') AS YEAR, \n",
      " ",noquote(haul),".REGION,\n",
      " ",noquote(haul),".STRATUM,\n",
      " ",noquote(length1),".LENGTH, \n",
      " ",noquote(length1),".FREQUENCY, \n",
      " ",noquote(length1),".SEX, \n",
      " ",noquote(haul),".GEAR_TEMPERATURE AS TEMP, \n",
      " ",noquote(haul),".SURFACE_TEMPERATURE AS STEMP, \n",
      " ",noquote(haul),".BOTTOM_DEPTH AS DEPTH, \n",
      " ",noquote(haul),".END_LATITUDE AS LAT, \n",
      " ",noquote(haul),".END_LONGITUDE AS LON, \n",
      " ",noquote(length1),".SPECIES_CODE \n",
      "FROM ",noquote(haul)," \n",
      "INNER JOIN ",noquote(length)," \n",
      "ON ",noquote(haul),".HAULJOIN                     = ",noquote(length1),".HAULJOIN \n ",
      "WHERE ",noquote(haul),".REGION ", noquote(area)," \n ",
      "AND TO_CHAR(",noquote(haul),".START_TIME, 'yyyy') IN (",noquote(years),") \n ",
      end1)

    
  
  SN            <- data.table(sqlQuery(AKFIN,spec))
  location      <- data.table(sqlQuery(AKFIN,test))
  location_poll <- data.table(sqlQuery(AKFIN,test2))
  length        <- data.table(sqlQuery(AKFIN,test3))
  
  odbcClose(AKFIN)
  
  location$SURVEY_DEFINITION_ID      <- survey
  location_poll$SURVEY_DEFINITION_ID <- survey
  length$SURVEY_DEFINITION_ID        <- survey
  
  
  if(survey==78) {
    location$REGION <- as.character(location$REGION)
    location_poll$REGION <- as.character(location_poll$REGION)
    length$REGION <- as.character(length$REGION)
    SN$REGION <- as.character(SN$REGION)

    location$REGION      <- "EBS_SLOPE"
    location_poll$REGION <- "EBS_SLOPE"
    length$REGION        <- "EBS_SLOPE"
    SN$REGION            <- "EBS_SLOPE"
  }
  
  SN <-SN[!is.na(SN$COMMON_NAME)]
  
  l2<-length[,list(NUMBER=sum(FREQUENCY)),by=c("SPECIES_CODE")]
  spc1<-l2[NUMBER>=minN]
  spc<-unique(spc1$SPECIES_CODE)
  length<-length[SPECIES_CODE %in% spc]
  location_poll<-location_poll[SPECIES_CODE %in% spc]
  SN<- SN[SPECIES_CODE %in% spc]
  SN<-merge(SN,spc1,all.x=T,by="SPECIES_CODE")
  SN<-SN[order(COMMON_NAME),]

  data<-list(location=location,location_poll=location_poll,length=length,SN=SN)
  return(data)
}

## for pulling all survey and species data

Get_ALL_DATA<- function(username1="",password1="",yr1=c(1982:2017))
{
  require(reshape2)
  dataslope<-Get_DATA(username=username1,password=password1,species="ALL",survey = 78 ,yr=yr1)
  datagoa<-Get_DATA(username=username1,password=password1,species="ALL",survey = 47,yr=yr1)
  datashelf<-Get_DATA(username=username1,password=password1,species="ALL",survey = 98 ,yr=yr1)
  dataai<-Get_DATA(username=username1,password=password1,species="ALL",survey = 52 ,yr=yr1)

  length<-data.table(rbind(dataslope$length,datagoa$length,dataai$length,datashelf$length))
  location<-data.table(rbind(dataslope$location,datagoa$location,dataai$location,datashelf$location))
  location_poll<-data.table(rbind(dataslope$location_poll,datagoa$location_poll,dataai$location_poll,datashelf$location_poll))
  SN <- data.table(rbind(dataslope$SN,datagoa$SN,dataai$SN,datashelf$SN))
  SN <- reshape2::dcast(SN,formula=COMMON_NAME+SPECIES_CODE+SPECIES_NAME~REGION,value.var="NUMBER")
  SN[is.na(SN)] <- 0
  SN<-data.table(SN)
  data<-list(location=location,location_poll=location_poll,length=length,SN=SN)
  return(data)
}

 

## formatting the data as used in the PSESV-1 shiny app

EDIT_DATA4SHINY <- function(data=data1)
  {
    SN            <- data1$SN
    length        <- data1$length
    location      <- data1$location
    location_poll <- data1$location_poll

  surveys<-unique(length$SURVEY_DEFINITION_ID)
  length1<-vector("list",length=length(surveys))
  location1<-vector("list",length=length(surveys))
  location_poll1<-vector("list",length=length(surveys))
  g <- 0
## if survey is EBS, exlcude far north regions.
  if(98 %in% surveys){
    g<-g+1
    length1[[g]] <- length[SURVEY_DEFINITION_ID==98 & !is.na(STRATUM) & STRATUM<63]
    location1[[g]]      <- location[SURVEY_DEFINITION_ID==98 & !is.na(STRATUM) & STRATUM<63]
    location_poll1[[g]] <- location_poll[SURVEY_DEFINITION_ID==98 & !is.na(STRATUM) & STRATUM<63]
  }

  if(47 %in% surveys){
    g<-g+1
    length1[[g]]<-length[SURVEY_DEFINITION_ID==47]
    location1[[g]]<-location[SURVEY_DEFINITION_ID==47]
    location_poll1[[g]]<-location_poll[SURVEY_DEFINITION_ID==47]
    ## limiting data to western GOA and to depths < 500m to be consistent across time
    location1[[g]]$SL1=trunc(location1[[g]]$STRATUM/10)-(trunc(location1[[g]]$STRATUM/100)*10)
    length1[[g]]$SL1=trunc(length1[[g]]$STRATUM/10)-(trunc(length1[[g]]$STRATUM/100)*10)
    location_poll1[[g]]$SL1=trunc(location_poll1[[g]]$STRATUM/10)-(trunc(location_poll1[[g]]$STRATUM/100)*10)

    length1[[g]]        <- length1[[g]][!is.na(STRATUM)&STRATUM<400 & YEAR > 1990]
    location1[[g]]      <- location1[[g]][!is.na(STRATUM)&STRATUM<400 & YEAR > 1990]
    location_poll1[[g]] <- location_poll1[[g]][!is.na(STRATUM)&STRATUM<400 & YEAR > 1990]

    length1[[g]][length1[[g]]$SL1>=4]$SURVEY_DEFINITION_ID <-48
    location1[[g]][location1[[g]]$SL1>=4]$SURVEY_DEFINITION_ID <-48
    location_poll1[[g]][location_poll1[[g]]$SL1>=4]$SURVEY_DEFINITION_ID <-48

    length1[[g]][length1[[g]]$SL1==1]$SURVEY_DEFINITION_ID <-46
    location1[[g]][location1[[g]]$SL1==1]$SURVEY_DEFINITION_ID <-46
    location_poll1[[g]][location_poll1[[g]]$SL1==1]$SURVEY_DEFINITION_ID <-46

    length1[[g]]$SL1<-NULL
    location1[[g]]$SL1<-NULL
    location_poll1[[g]]$SL1<-NULL
  
    }

## for SLope survey exclue 2000 from all plots
  if(78  %in% surveys){
    g<-g+1
    length1[[g]]        <- length[SURVEY_DEFINITION_ID==78 & YEAR!=2000]
    location1[[g]]      <- location[SURVEY_DEFINITION_ID==78 & YEAR!=2000]
    location_poll1[[g]] <- location_poll[SURVEY_DEFINITION_ID==78 & YEAR!=2000]
    }

  if(52 %in% surveys){
    g <- g+1
    length1[[g]]        <- length[SURVEY_DEFINITION_ID==52 & !is.na(STRATUM) & STRATUM<800 & YEAR > 1991]
    location1[[g]]      <- location[SURVEY_DEFINITION_ID==52 & !is.na(STRATUM) & STRATUM<800 & YEAR > 1991]
    location_poll1[[g]] <- location_poll[SURVEY_DEFINITION_ID==52 & !is.na(STRATUM) & STRATUM<800 & YEAR > 1991]
  }

  length<-data.table(do.call(rbind,length1))
  location<-data.table(do.call(rbind,location1))
  location_poll<-data.table(do.call(rbind,location_poll1))
##Exclude null locations and transform to all positive longitudes
  location      <-location[!is.na(LON)]
  location_poll <-location_poll[!is.na(LON)]
  length        <-length[!is.na(LON)]
     
  location$LON[location$LON<0]           <-  360 + location$LON[location$LON<0]
  location_poll$LON[location_poll$LON<0] <-  360 + location_poll$LON[location_poll$LON<0]
  length$LON[length$LON<0]               <-  360 + length$LON[length$LON<0]
      
## rounding bottom depth to nearest 0.5 meters and temperature to nearest 0.1 degree to simplify calculations
  location$DEPTHR <- round(location$DEPTH)
  location$TEMPR  <- round(location$TEMP,1)
      
  length$TEMPR    <- round(length$TEMP,1)
  length$DEPTHR   <- round(length$DEPTH)
  
  length          <- merge(length,location_poll,by = c("YEAR","LON","LAT","STRATUM","SURVEY_DEFINITION_ID", "SPECIES_CODE"))

  length          <-subset(length,!is.na(length$CPUE))

  data<-list(length=length,location=location, location_poll=location_poll, SN=SN)
  return(data)
}

### to get data exactly as used in the PSESV-1 example

data1<-Get_ALL_DATA(username1="",password1="",yr=c(1982:2016))
results <- EDIT_DATA4SHINY (data=data1)

saveRDS(results,"results.rds")