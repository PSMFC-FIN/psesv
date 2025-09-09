# vis <- "vis_nmfs"
# source("Global.R")


#  In order to scale plots of months based on the number of months being plotted, need to extract 
#  the ggplotting number of rows that will be used to rescale
gg_facet_nrow <- function(p){
  num_panels <- length(unique(ggplot_build(p)$data[[1]]$PANEL)) # get number of panels
  num_cols <- ggplot_build(p)$layout$facet$params$ncol # get number of columns set by user
  num_rows <- wrap_dims(num_panels, ncol=num_cols)[1] # determine number of rows
}


#  Load shapefile with NMFS areas. 
#  Shapefile originated from a NOAA EFH geodatabase (https://www.fisheries.noaa.gov/webdam/download/77686817)
#  Shapefile of NMFS areas was:
#  (1) exported from geodatabase as saved as "nmfs_areas"
#simplenmfs <- readOGR(dsn="../Data",layer="nmfs_areas")
#  (2) transformed to lat lon (0 to 360) via: spTransform(simplenmfs, CRS("+proj=longlat +datum=WGS84 +no_defs +lon_wrap=180"))
#simplenmfs <- spTransform(simplenmfs, CRS("+proj=longlat +datum=WGS84 +no_defs +lon_wrap=180"))
#  (3) simplified to reduce file size
#library(rmapshaper)
#simple <- rmapshaper::ms_simplify(simplenmfs)
#  (4) saved as the final shapefile that we use for the map in this app
#writeOGR(simple,dsn="Data","simplenmfs",driver="ESRI Shapefile")

#  Read in NMFS reporting area shapefile
simplenmfs <- sf::st_read(dsn="Data",layer="simplenmfs")

#------------------------------------------------------------------------------------
#  Define global parameters for each of the three tabs
#------------------------------------------------------------------------------------

#  Create the map 
mynmfsmap <- leaflet(simplenmfs) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = "blue",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              layerId = ~REP_AREA,
              label=~REP_AREA,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>% 
  addScaleBar()

#----------------------------------------------------
#  Multi-panel layout
#----------------------------------------------------

server_vis <- shinyServer(function(input, output){

  # Define monthly data summary
  rxDataMonthly <- reactive({

    data2 %>% 
      filter(m.depth>=input$range_month[1] & m.depth<=input$range_month[2] & NMFSAREA%in%c(input$dnmfs_month) & monthname%in%input$dmonth) %>% 
      group_by(NMFSAREA,month,year) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA,month) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE),
             monthname=month.name[month])
  })

  #  Create monthly plot 
  myplot <- reactive({
    req(input$dnmfs_month)
    req(input$dmonth)
    
    dataset <- rxDataMonthly()  # this is the subsetted data
    
    #  Subsetted months get plotted alphabetically unless factor levels are redefined
    dataset$monthname <- factor(dataset$monthname,levels=unique(dataset$monthname[order(dataset$month)]))
  
    myplot <- ggplot(dataset, aes(x = year, y = tempanom, fill=factor(NMFSAREA))) +
      geom_bar(stat="identity",position="dodge") + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      xlab("Year") + 
      ylab("Temperature Anomaly (SD)") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14),
            plot.title = element_text(size=16),
            strip.text = element_text(size=14)) + 
      scale_x_continuous(breaks = 2003:2019, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017","","2019")) + 
      facet_wrap(. ~ monthname,ncol=1)
    return(myplot)
  })
  
  #  Determine number of months in order to scale the size of the facets in the plot
  he <- reactive(gg_facet_nrow(myplot()))
  
  #  Create plot and scale figure height based on number of months selected using he() function above.
  output$monthly_nmfs_plot <- renderPlot({myplot()}, height = function(){he()*250})
  
  rxDataSeasonal <- reactive({
    #req(input$dnmfs)
    dat <- data2 %>% 
      filter(m.depth>=input$range[1] & m.depth<=input$range[2] & newyr>2002 & NMFSAREA%in%c(input$dnmfs) & seasonflag=="complete") %>% 
      group_by(NMFSAREA,season,newyr) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA,season) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE))
    dat
  })
  
  output$seasonal_nmfs_plot <- renderPlot({
    req(input$dnmfs)
    datasetSeasonal <- rxDataSeasonal()  # this is the subsetted data
    p <- ggplot(datasetSeasonal, aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
      geom_bar(stat="identity",position="dodge") + 
      facet_wrap(~season) + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      xlab("Year") + 
      ylab("Temperature Anomaly (SD)") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14),
            strip.text = element_text(size=14)) + 
      scale_x_continuous(breaks = 2003:2018, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017",""))
    print(p)
  })
  
  
  output$map <- renderLeaflet(
    mynmfsmap
  )
  
})

