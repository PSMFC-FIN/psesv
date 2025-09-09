#browser()
gg_facet_nrow <- function(p){
  num_panels <- length(unique(ggplot_build(p)$data[[1]]$PANEL)) # get number of panels
  num_cols <- ggplot_build(p)$layout$facet$params$ncol # get number of columns set by user
  num_rows <- wrap_dims(num_panels, ncol=num_cols)[1] # determine number of rows
}


shinyServer(function(input, output, session) {
  
  ########################## AK tab plots and tables ###############################################
  ak_datasetInput <- reactive({
  if(input$akvar=="vesscounts"){
    ak.Dat <- subset(eff$vesscounts$nvescat_ak, year %in% seq(input$ak.year[1],input$ak.year[2]) &
                       target %in% input$ak.target & zone %in% input$ak.zone & sector %in% input$ak.sector)
  }
    if(input$akvar=="fishingweeks"){
      # browser()
      ak.Dat <- subset(eff$fishingweeks, year %in% seq(input$ak.year[1],input$ak.year[2]) & zone %in% "All Alaska" &
                         target %in% input$ak.target & gear %in% input$ak.gear & 
                         sector %in% input$ak.sector & length %in% input$ak.length)
    }
    if(input$akvar=="crewweeks"){
      ak.Dat <- subset(eff$crewweeks, year %in% seq(input$ak.year[1],input$ak.year[2]) & zone %in% "All Alaska" &
                         month %in% input$ak.month & sector %in% input$ak.sector)
    }
    ak.Dat
  }) ## close reactive for creating ak dataset
  mk_plt_ak <- reactive({
    if(input$akvar=="vesscounts"){
      ak.pltFig <- ggplot(ak_datasetInput(), aes(x=year, y=vessels, fill=target)) + geom_bar(position="dodge",stat="identity") +
        facet_grid(zone~sector, scales="fixed", labeller = labeller(zone = label_wrap_gen(20))) + 
        theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Vessel Counts") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        #ggtitle(paste0("Number of vessel fishing by species, sector, and zone.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$akvar=="fishingweeks"){
      ak.pltFig <- ggplot(ak_datasetInput(), aes(x=year, y=weeks, fill=length)) + geom_bar(position="dodge",stat="identity") +
        facet_grid(target~sector, scales="fixed", labeller = labeller(sector = label_wrap_gen(10))) + 
        theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Fishing Weeks") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Weeks fishing in Alaska of ",tolower(input$ak.gear), " vessels by species, sector, length.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$akvar=="crewweeks"){
      ak.pltFig <- ggplot(ak_datasetInput(), aes(x=year, y=crew.weeks, fill = month)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(.~sector, scales="fixed", labeller = labeller(sector = label_wrap_gen(10))) + 
        theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Crew Weeks") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Crew weeks in Alaska by sector and month.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
      # browser()
    }
    ak.pltFig
  }) ## close plot function
  # he <- reactive(gg_facet_nrow(mk_plt_ak()))
  # output$ak_plt <- renderPlot({mk_plt_ak()}, height = function(){he()*250}) ## close renderPlot
  he_ak <- reactive({
    if(input$akvar=="vesscounts"){
      he_tmp <- length(input$ak.zone)*250
    }
    if(input$akvar=="fishingweeks"){
      he_tmp <- length(input$ak.target)*250
    }
    if(input$akvar=="crewweeks"){
      he_tmp <- 250
    }
    he_tmp
  })
  output$ak_plt <- renderPlot({mk_plt_ak()}, height = he_ak) ## close renderPlot
  output$ak_title <- renderText({ 
    if(input$akvar=="vesscounts"){
      akttl <- paste0("Number of vessel fishing by species, sector, and zone.")
    }
    if(input$akvar=="fishingweeks"){
      akttl <- paste0("Weeks fishing in Alaska of ",tolower(input$ak.gear), " vessels by species, sector, length.")
    }
    if(input$akvar=="crewweeks"){
      akttl <- paste0("Crew weeks in Alaska by sector and month.")
    }
    akttl
    # paste("First wholesale ", wsvar, " in the ", input$wsl_zone, "region by species and product ($millions)")
  })
  output$ak_txt <- renderText({
    ak.txtOut <-  ""
    # if(any(grepl("Gulf of Alaska|All Alaska", input$ak.zone)) & input$ak.sector!="All"){
    #   ak.txtOut <- "'All' is the only available sector for the GOA and AK regions."}
    # if(validEntry$useAllProd){
    #   if(ak.txtOut!="") {paste0(ak.txtOut, " \n ")}
    #   ak.txtOut <- paste(ak.txtOut, "The product",input$ak.product,"is not available for the selected species, zone, and sector.")}
    #   ak.txtOut
  }) ## close renderText
  output$ak_tab <- renderTable({
    if(input$akvar=="vesscounts"){
    ak.tblDat <- dcast(ak_datasetInput(), target+zone+sector ~ year, value.var="vessels")}
    if(input$akvar=="fishingweeks"){
    ak.tblDat <- dcast(ak_datasetInput(), target+zone+sector+gear+length ~ year, value.var="weeks")}
    if(input$akvar=="crewweeks"){
    ak.tblDat <- dcast(ak_datasetInput(), zone+sector+year ~ month, value.var="crew.weeks")}
    xtable(ak.tblDat, type="html")
  }) ## close renderTable
  #### data for download button
  output$akeff_downloadData <- downloadHandler(
    filename = "akeffortdata.csv",
    content = function(file){write.csv(ak_datasetInput(), file, row.names = FALSE)},
    contentType = "csv"
  ) ## close downloadHandler
  ########################## BSAI tab plots and tables ###############################################
  bsai_datasetInput <- reactive({
    if(input$bsaivar=="vesscounts"){
      bsai.Dat <- subset(eff$vesscounts$nvesmonth, zone %in% "Bering Sea and Aleutian Islands" & 
                              year %in% seq(input$bsai.year[1],input$bsai.year[2]) & 
                              month %in% input$bsai.month & 
                              gear %in% input$bsai.gear1 & sector %in% input$bsai.sector)
    }
    if(input$bsaivar=="fishingweeks"){
      bsai.Dat <- subset(eff$fishingweeks, year %in% seq(input$bsai.year[1],input$bsai.year[2]) & 
                              zone %in% "Bering Sea and Aleutian Islands" &
                              target %in% input$bsai.target & gear %in% input$bsai.gear2 & 
                              sector %in% input$bsai.sector & length %in% input$bsai.length)
    }
    if(input$bsaivar=="crewweeks"){
      bsai.Dat <- subset(eff$crewweeks, year %in% seq(input$bsai.year[1],input$bsai.year[2]) & 
                              zone %in% "Bering Sea and Aleutian Islands" &
                              month %in% input$bsai.month &  sector %in% input$bsai.sector)
    }
    bsai.Dat
  })
  mk_plt_bsai <- reactive({
    if(input$bsaivar=="vesscounts"){
      bsai.pltFig <- ggplot(bsai_datasetInput(), aes(x=year, y=vessels, fill = month)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(gear~sector, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Vessel Counts") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Number of vessel fishing in the BSAI by month, sector, and gear type.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$bsaivar=="fishingweeks"){
      bsai.pltFig <- ggplot(bsai_datasetInput(), aes(x=year, y=weeks, fill=length)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(target~sector, scales="fixed", labeller = labeller(sector = label_wrap_gen(10))) + 
        theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Fishing Weeks") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Weeks fishing in the BSAI of ",tolower(input$bsai.gear2), " vessels by species, sector, and length.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$bsaivar=="crewweeks"){
      # browser()
      bsai.pltFig <- ggplot(bsai_datasetInput(), aes(x=year, y=crew.weeks, fill = month)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(.~sector, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Crew Weeks") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Crew weeks in  the BSAI by sector and month.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    bsai.pltFig
  }) ## close plot function
  he_bsai <- reactive({
    if(input$bsaivar=="vesscounts"){
      he_tmp <- length(input$bsai.gear1)*250
    }
    if(input$bsaivar=="fishingweeks"){
      he_tmp <- length(input$bsai.target)*250
    }
    if(input$bsaivar=="crewweeks"){
      he_tmp <- 250
    }
    he_tmp
  })
  output$bsai_plt <- renderPlot({mk_plt_bsai()}, height = he_bsai) ## close renderPlot
  output$bsai_title <- renderText({ 
    if(input$bsaivar=="vesscounts"){
      bsaittl <- paste0("Number of vessel fishing in the BSAI by month, sector, and gear type.")
    }
    if(input$bsaivar=="fishingweeks"){
      bsaittl <- paste0("Weeks fishing in the BSAI of ",tolower(input$bsai.gear2), " vessels by species, sector, and length.")
    }
    if(input$bsaivar=="crewweeks"){
      bsaittl <- paste0("Crew weeks in  the BSAI by sector and month.")
    }
    bsaittl
  })
  output$bsai_txt <- renderText({
    bsai.txtOut <-  ""
    bsai.txtOut
  })## close renderText
  output$bsai_tab <- renderTable({
    if(input$bsaivar=="vesscounts"){
      bsai.tblDat <- dcast(bsai_datasetInput(), zone+sector+gear+year ~ month, value.var="vessels")}
    if(input$bsaivar=="fishingweeks"){
      bsai.tblDat <- dcast(bsai_datasetInput(), target+zone+sector+gear+length ~ year, 
                           value.var="weeks")}
    if(input$bsaivar=="crewweeks"){
      bsai.tblDat <- dcast(bsai_datasetInput(), zone+sector+year ~ month, value.var="crew.weeks")}
    xtable(bsai.tblDat, type="html")
  })## close renderTable
  #### data for download button
  output$bsaieff_downloadData <- downloadHandler(
    filename = "bsaieffortdata.csv",
    content = function(file){write.csv(bsai_datasetInput(), file, row.names = FALSE)},
    contentType = "csv"
  ) ## close downloadHandler
  ########################## GOA tab plots and tables ###############################################
  goa_datasetInput <- reactive({
    if(input$goavar=="vesscounts"){
      goa.Dat <- subset(eff$vesscounts$nvesmonth, zone %in% "Gulf of Alaska" & 
                             year %in% seq(input$goa.year[1],input$goa.year[2]) & 
                             month %in% input$goa.month & 
                             gear %in% input$goa.gear1 & sector %in% input$goa.sector)
    }
    if(input$goavar=="fishingweeks"){
      goa.Dat <- subset(eff$fishingweeks, year %in% seq(input$goa.year[1],input$goa.year[2]) & 
                             zone %in% "Gulf of Alaska" &
                             target %in% input$goa.target & gear %in% input$goa.gear2 & 
                             sector %in% input$goa.sector & length %in% input$goa.length)
    }
    if(input$goavar=="crewweeks"){
      goa.Dat <- subset(eff$crewweeks, year %in% seq(input$goa.year[1],input$goa.year[2]) & 
                             zone %in% "Gulf of Alaska" &
                             month %in% input$goa.month &  sector %in% input$goa.sector)
    }
    goa.Dat
  })
  mk_plt_goa <- reactive({
    if(input$goavar=="vesscounts"){
      goa.pltFig <- ggplot(goa_datasetInput(), aes(x=year, y=vessels, fill = month)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(gear~sector, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Vessel Counts") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Number of vessel fishing in the GOA by month, sector, and gear type.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$goavar=="fishingweeks"){
      goa.pltFig <- ggplot(goa_datasetInput(), aes(x=year, y=weeks, fill=length)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(target~sector, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Fishing Weeks") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Weeks fishing in the GOA of ",tolower(input$goa.gear2), 
        #                " vessels by species, sector, and length.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$goavar=="crewweeks"){
      goa.pltFig <- ggplot(goa_datasetInput(), aes(x=year, y=crew.weeks, fill = month)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(.~sector, scales="fixed", labeller = labeller(sector = label_wrap_gen(10))) + 
        theme(panel.spacing = unit(0.1, "lines")) +
        ylab("Crew Weeks") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        # ggtitle(paste0("Crew weeks in the GOA by sector and month.")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    print(goa.pltFig)
  }) ## close renderPlot
  he_goa <- reactive({
    if(input$goavar=="vesscounts"){
      he_tmp <- length(input$goa.gear1)*250
    }
    if(input$goavar=="fishingweeks"){
      he_tmp <- length(input$goa.target)*250
    }
    if(input$goavar=="crewweeks"){
      he_tmp <- 250
    }
    he_tmp
  })
  output$goa_plt <- renderPlot({mk_plt_goa()}, height = he_goa) ## close renderPlot
  output$goa_title <- renderText({ 
    if(input$goavar=="vesscounts"){
      goattl <- paste0("Number of vessel fishing in the GOA by month, sector, and gear type.")
    }
    if(input$goavar=="fishingweeks"){
      goattl <- paste0("Weeks fishing in the GOA of ",tolower(input$goa.gear2), " vessels by species, sector, and length.")
    }
    if(input$goavar=="crewweeks"){
      goattl <- paste0("Crew weeks in the GOA by sector and month.")
    }
    goattl
  })
  
  output$goa_txt <- renderText({
    goa.txtOut <-  ""
    goa.txtOut
  })## close renderText
  output$goa_tab <- renderTable({
    # browser()
    if(input$goavar=="vesscounts"){
      goa.tblDat <- dcast(goa_datasetInput(), zone+sector+gear+year ~ month, value.var="vessels")}
    if(input$goavar=="fishingweeks"){
      goa.tblDat <- dcast(goa_datasetInput(), target+zone+sector+gear+length ~ year, value.var="weeks")}
    if(input$goavar=="crewweeks"){
      goa.tblDat <- dcast(goa_datasetInput(), zone+sector+year ~ month, value.var="crew.weeks")}
    xtable(goa.tblDat, type="html")
  })## close renderTable
  #### data for download button
  output$goaeff_downloadData <- downloadHandler(
    filename = "goaeffortdata.csv",
    content = function(file){write.csv(goa_datasetInput(), file, row.names = FALSE)},
    contentType = "csv"
  ) ## close downloadHandler
})