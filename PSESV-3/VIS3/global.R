library(dplyr)
library(ggplot2)
library(shiny)
library(viridis)
##library(rgdal)
library(sf)
library(sp)
library(terra)
library(leaflet)
library(htmltools)
library(shinycssloaders)
library(shinyWidgets)
library(psesv)


vis <- "vis_table"
# source("Global.R")

if(vis=="vis_table"){
  
  mydata <- readRDS("Data/temperature_data.RDS") %>% 
    left_join(readRDS("Data/date_data.RDS")) %>% 
    left_join(readRDS("Data/spatial_data.RDS")) %>% 
    # dplyr::select(-statlab)) %>% 
    data.frame %>% 
    mutate(date=as.Date(date))
  
  mydata.extra <- mydata %>% 
    dplyr::select(-c(sst.mean,sst.sd,date,month,week,julian,date)) %>% 
    distinct()
  
  #  Read in weekly summarized data file for stat areas
  weekly_stat_data <- readRDS("Data/weekly_stat.RDS") %>% 
    inner_join(mydata.extra)
  
  #  Read in monthly summarized data file for stat areas
  monthly_stat_data <- readRDS("Data/monthly_stat.RDS") %>% 
    inner_join(mydata.extra)
  
  nmfsdat <- readRDS("Data/nmfsarea_coords.RDS")
  
} else if (vis=="vis_nmfs"){
  
  # The seasons of winter 2003 (Oct 2002 - Mar 2003) and winter 2019 (Oct 2018 - Mar 2019) are
  # incomplete so I create a "seasonflag" field that allows us to flag these months and then avoid
  # including them in the calculation of anomalies so as to avoid bias. This will have to be tweaked
  # when data are updated.
  # Similarly, the last line removes the partial month of March 2019 to avoid bias.
  data2 <- readRDS("Data/temperature_data.RDS") %>%
    left_join(readRDS("Data/date_data.RDS") %>% 
                dplyr::select(-c(julian,week)) %>% 
                mutate(newyr=ifelse(month<4,year-1,year),
                       month2=ifelse(month<4,month+12,month),
                       season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                       monthname=month.name[month],
                       seasonflag=ifelse((newyr==2018 & month2>=10) | newyr==2002,"incomplete","complete"))) %>% #avoid incomplete seasons for anomalies.
    left_join(readRDS("Data/spatial_data.RDS") %>% 
                dplyr::select(c(STAT_AREA,NMFSAREA,m.depth))) %>% 
    dplyr::select(-STAT_AREA) %>% 
    filter(date<"2019-03-01") # March only includes two days of data at present; they are omitted to avoid skewing anomalies.
  
  
} else {
  
  mydata <- readRDS("Data/temperature_data.RDS") %>% 
    dplyr::select(-sst.sd) %>% 
    left_join(readRDS("Data/date_data.RDS") %>% 
                transmute(cumwk=(year-2003)*52+week,
                          cummo=(year-2003)*12+month,
                          date,
                          year)) %>% 
    mutate(date=as.POSIXct(date))
}

mystats <- sort(unique(mydata$STAT_AREA))
mynmfsall <- sort(unique(mydata$NMFSAREA))

