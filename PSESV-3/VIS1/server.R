# vis <- "vis_map_stat"
# source("Global.R")

#-------------------------------------------------------------------------------
#  The original shapefile for visualization was downloaded from:
#  soa-adfg.opendata.arcgis.com/datasets/groundfish-statistical-areas-2001
#  The file is named "Groundfish_Statistical_Areas_2001.shp" and can be found in the data folder.
#stat6 <- readOGR(dsn="../Data",layer="Groundfish_Statistical_Areas_2001")

#  The original file included both positive and negative longitudes across the international date line. 
#  Convert positive longitudes to negative in order to make a single contiguous map of Alaska waters.
#mypoly <- length(stat6@polygons)
#for(i in 1:mypoly){
#  mypoly2 <- length(stat6@polygons[[i]]@Polygons)
#  for(j in 1:mypoly2){
#    stat6@polygons[[i]]@Polygons[[j]]@coords[,1][stat6@polygons[[i]]@Polygons[[j]]@coords[,1]>0] <- stat6@polygons[[i]]@Polygons[[j]]@coords[,1]-360
#  }
#}

#  The original shapefile is more detailed than is necessary for the visualization. Simplify polygons using rmapshaper::ms_simplify
#library(rmapshaper)
#simple <- rmapshaper::ms_simplify(stat6)

#  This simplified file is used for the visualization and to make the visualization load faster is saved as "adf_stat_areas_simple.shp" and can be 
#  found in the data folder
#writeOGR(simple,dsn="Data","adfg_stat_areas_simple",driver="ESRI Shapefile")

#  For the purposes of the visualization, use a simplified version of the shapefile where some of the coastline detail is smoothed. This speeds up
#  display of the data. 

simple <- sf::st_read(dsn="Data",layer="adfg_stat_areas_simple")

#------------------------------------------------------------------------------------
#  Define global parameters for each of the three tabs
#------------------------------------------------------------------------------------

#  Define the map parameters
mymap <- leaflet(simple) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = "blue",
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              layerId = ~STAT_AREA,
              label=~STAT_AREA,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto")) %>% 
  addScaleBar()

#  Define temperature plot parameters
myplotpars <- list(xlab("Date"),
                   ylab(expression("Temperature " ( degree*C))),
                   scale_x_datetime(date_breaks="1 year",date_labels="%Y"), 
                   scale_color_viridis(discrete=TRUE,
                      guide=guide_legend("Statistical Area",ncol=4)))

#  Define temperature plot theme
mytheme <- theme_bw() + 
    theme(legend.position="top",
        legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        axis.text = element_text(size=10),
        axis.title = element_text(size=14))
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
#  Define server side functions
#------------------------------------------------------------------------------------

server_vis <- shinyServer(function(input, output) {
  
  #  Populate click list as vector to allow for multiple selections
  click_list <- reactiveValues(ids=vector())
  # produce the basic leaflet map
  output$map <- renderLeaflet(
    mymap
  )
  
  observeEvent(input$map_shape_click, { # update the location selectInput on map clicks
    click <- input$map_shape_click
    #  Concatenate successive clicks to allow for plotting of multiple stat areas.
    click_list$ids <- c(click_list$ids, click$id)
    #print(click_list$ids)
  })
  
  # Populate the reset button.
  observeEvent(input$reset,{
    click_list$ids <- NULL
  })
  
  #  Define three different plots (daily, weekly, monthly)
  daily_plot <- reactive({
    ggplot(
      mydata %>% 
        filter(STAT_AREA%in%click_list$ids & 
                 year>=input$yearrange[1] & 
                 year<=input$yearrange[2]), 
      aes(x = (date), y = sst.mean,color=factor(STAT_AREA))) +
      geom_line(size=1.0) + 
      myplotpars + 
      mytheme
  })
  
  weekly_plot <- reactive({
    ggplot(
      mydata %>% 
        filter(STAT_AREA%in%click_list$ids & 
                 year>=input$yearrange[1] & 
                 year<=input$yearrange[2]) %>% 
        group_by(cumwk,STAT_AREA) %>% 
        summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                  date=min(date),
                  year=year[1]), 
      aes(x = (date), y = sst.mean,color=factor(STAT_AREA))) +
      geom_line(size=1.0) + 
      myplotpars + 
      mytheme
  })
  
  monthly_plot <- reactive({
    ggplot(
      mydata %>% 
        filter(STAT_AREA%in%click_list$ids & 
                 year>=input$yearrange[1] & 
                 year<=input$yearrange[2]) %>% 
        group_by(cummo,STAT_AREA) %>% 
        summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                  date=min(date),
                  year=year[1]), 
      aes(x = (date), y = sst.mean,color=factor(STAT_AREA))) +
      geom_line(size=1.0) + 
      myplotpars + 
      mytheme
  })
  
  #  Use the drop down to determine whether to view the daily, weekly, or monthly plot
  graphInput <- reactive({
    switch(input$graph,
           "Daily"=daily_plot(),
           "Weekly average"=weekly_plot(),
           "Monthly average"=monthly_plot())
  })
  
  #  Create the final plot
  output$mainplot <- renderPlot({
    req(click_list$ids)
    
    #  If the reset button was clicked, abort plotting and start fresh.
    if(is.null(click_list$ids)) return()
    
    graphInput()
    
  })
  
})


