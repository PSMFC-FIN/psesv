shinyServer(function(input, output, session) {

  
    output$wstitle <- renderText({ 
      if(input$wsl_var=="ws.val"){
        wsvar <- "value"
      }
      if(input$wsl_var=="product.weight.mt"){
        wsvar <- "production volume"
      }
      if(input$wsl_var=="ws.price.lb"){
        wsvar <- "price"
      }
      
      paste("First wholesale ", wsvar, " in the ", input$wsl_zone, "region by species and product ($millions)")
    })
    
    output$evtitle<- renderText({ 
      if(input$exv_var=="exves.val.m"){
        evvar <- "value"
      }
      if(input$exv_var=="retcatch.mt.k"){
        evvar <- "retained catch"
      }
      if(input$exv_var=="exves.price.lb"){
        evvar <- "price"
      }
      
      
      paste("Ex-vessel ", evvar, " in the ", input$exv_zone, "region by species and gear type ($millions)")
    })
  
  
  validEntry <- reactiveValues(useAllProd = FALSE, useAllGear = FALSE)
  ################### Frist-wholesale tab  ##############################
  #### Create wholesale data
    wsl_datasetInput <- reactive({
    if(input$wsl_zone=="AK"){   wsl.product <- input$ak.wsl.product;   wsl.species <- input$ak.wsl.species}
    if(input$wsl_zone=="BSAI"){ wsl.product <- input$bsai.wsl.product; wsl.species <- input$bsai.wsl.species}
    if(input$wsl_zone=="GOA"){  wsl.product <- input$goa.wsl.product;  wsl.species <- input$goa.wsl.species}
    wsl.Dat <- subset(econ$wsl[c(wsl.vars,input$wsl_var)], year %in% seq(input$wsl.year[1],input$wsl.year[2]) & 
                           species %in% wsl.species & zone %in% input$wsl_zone & sector %in% input$wsl.sector)
    if(!any(wsl.product%in% unique(wsl.Dat$product))){
      wsl.Dat <- subset(wsl.Dat, product == "All Products")
      validEntry$useAllProd <- TRUE
    }else{
      wsl.Dat <- subset(wsl.Dat, product%in%wsl.product)
    }
    wsl.Dat
  }) ## close reactive for creating dataset
  #### Render wholesale plot 
  output$wsl_plt <- renderPlot({
    # browser()
    if(input$wsl_var=="ws.val"){
      wsl.pltFig <- ggplot(wsl_datasetInput(), aes(x=year, y=ws.val, fill = product)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(species~sector, scales="fixed", labeller = labeller(species = label_wrap_gen(10))) + 
        theme(panel.spacing = unit(0.1, "lines")) + 
        ylab("Value ($ millions)") + xlab("Year") + scale_y_continuous(label=dollar_format()) +
        ##ggtitle(paste0("First-wholesale value in the ", input$wsl_zone, 
                       ##" region by species and product ($ millions).")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$wsl_var=="product.weight.mt"){
      wsl.pltFig <- ggplot(wsl_datasetInput(), aes(x=year, y=product.weight.mt, fill = product)) + 
        geom_bar(position="dodge",stat="identity") +
        facet_grid(species~sector, scales="fixed", labeller = labeller(species = label_wrap_gen(10))) + 
        theme(panel.spacing = unit(0.1, "lines")) + 
        ylab("Production (mt)") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        ##ggtitle(paste0("First-wholesale production volume in the ", input$wsl_zone, 
                       ##" region by species and product (thousand mt).")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$wsl_var=="ws.price.lb"){
      wsl.pltFig <- ggplot(wsl_datasetInput(), aes(x=year, y=ws.price.lb, colour = product)) + 
        geom_line(position="dodge",stat="identity") + #width = 1, 
        facet_grid(species~sector, scales="fixed", labeller = labeller(species = label_wrap_gen(10))) + 
        theme(panel.spacing = unit(0.1, "lines")) + 
        ylab("Price/lb") + xlab("Year") + scale_y_continuous(label=dollar_format()) +
        ##ggtitle(paste("First-wholesale price in the ", input$wsl_zone, 
                       ##" region by species and product ($/lb).")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    print(wsl.pltFig)
  }, height = reactive(length(unique(wsl_datasetInput()$species))*200)) ## close renderPlot
  #### create text below figure if needed
  output$wsl_txt <- renderText({
    wsl.txtOut <-  ""
    if(any(input$wsl_var%in%c("ws.price.lb","ws.val"))) {
      wsl.txtOut <- paste(wsl.txtOut, "Dollar values are in nominal terms (not adjusted for inflation).")
    }
    # if(any(grepl("GOA|AK", input$wsl_zone)) & input$wsl.sector!="All"){
    #   wsl.txtOut <- "'All' is the only available sector for the GOA and AK regions."}
    if(validEntry$useAllProd){
      if(wsl.txtOut!="") {paste0(wsl.txtOut, " \n ")}
      wsl.txtOut <- paste(wsl.txtOut, "The product(s)", paste(wsl.product,collapse=", "),
                          "is/are not available for the selected species, zone, and sector.")}
      wsl.txtOut
  }) ## close renderText
  ####  Render table
  output$wsl_tab <- renderTable({
    xtable(dcast(wsl_datasetInput(), species+zone+product+sector ~ year, value.var=input$wsl_var), type="html")
  })## close renderTable
  # browser()
  #### data for download button
  output$wsl_downloadData <- downloadHandler(
    filename = "wsldata.csv",
    content = function(file){write.csv(wsl_datasetInput(), file, row.names = FALSE)},
    contentType = "csv"
  ) ## close downloadHandler

################### Ex-vessel tab  ##############################
  #### Create ex-vessel data
  exv_datasetInput <- reactive({
    if(input$exv_zone=="AK"){   exv.specgrp <- input$ak.exv.specgrp}
    if(input$exv_zone=="BSAI"){ exv.specgrp <- input$bsai.exv.specgrp}
    if(input$exv_zone=="GOA"){  exv.specgrp <- input$goa.exv.specgrp}
    exv.Dat <- subset(econ$exv[c(exv.vars,input$exv_var)], year %in% seq(input$exv.year[1],input$exv.year[2]) & 
                           specgrp %in% exv.specgrp & zone %in% input$exv_zone & area %in% input$exv.area & 
                           sector %in% input$exv.sector)
    # browser()
    # if(any(grepl("GOA|AK", input$exv_zone)) & input$exv.sector!="All Sectors"){
    #   exv.Dat <- subset(exv.Dat, sector == "All Sectors")
    # }else{
    #   exv.Dat <- subset(exv.Dat, sector %in% input$exv.sector)
    # }
    if(!any(input$exv.gear%in% unique(exv.Dat$gear))){
      exv.Dat <- subset(exv.Dat, gear == "All Gear")
      validEntry$useAllGear <- TRUE
    }else{
      exv.Dat <- subset(exv.Dat, gear %in% input$exv.gear)
    }
    exv.Dat
  })
  #### render ex-vessel plot  
  output$exv_plt <- renderPlot({
    if(input$exv_var=="exves.val.m"){
      exv.pltFig <- ggplot(exv_datasetInput(), aes(x=year, y=exves.val.m, fill = gear)) + 
        geom_bar(position="dodge",stat="identity")
      if(input$exv_zone=="AK"){  exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~., scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      if(input$exv_zone=="BSAI"){exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~sector, scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      if(input$exv_zone=="GOA"){ exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~area, scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      exv.pltFig <- exv.pltFig + theme(panel.spacing = unit(0.1, "lines")) + 
        ylab("Value ($ millions)")  + xlab("Year") + scale_y_continuous(label=dollar_format()) +
       # ggtitle(paste0("Ex-vessel value in the ", input$exv_zone, 
        #               " region by species and gear type ($ millions).")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$exv_var=="retcatch.mt.k"){
      exv.pltFig <- ggplot(exv_datasetInput(), aes(x=year, y=retcatch.mt.k, fill = gear)) + 
        geom_bar(position="dodge",stat="identity") 
      if(input$exv_zone=="AK"){  exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~., scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      if(input$exv_zone=="BSAI"){exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~sector, scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      if(input$exv_zone=="GOA"){ exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~area, scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      exv.pltFig <- exv.pltFig + theme(panel.spacing = unit(0.1, "lines")) + 
        ylab("Landings (mt)") + xlab("Year") + scale_y_continuous(label=comma_format()) +
        ##ggtitle(paste0("Ex-vessel retained catch in the ", input$exv_zone, 
          ##             " region by species and gear type (thousand mt).")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    if(input$exv_var=="exves.price.lb"){
      exv.pltFig <- ggplot(exv_datasetInput(), aes(x=year, y=exves.price.lb, colour = gear)) + 
        geom_line(position="dodge",stat="identity")
      if(input$exv_zone=="AK"){  exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~., scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      if(input$exv_zone=="BSAI"){exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~sector, scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      if(input$exv_zone=="GOA"){ exv.pltFig <- exv.pltFig + 
        facet_grid(specgrp~area, scales="fixed", labeller = labeller(specgrp = label_wrap_gen(10)))}
      exv.pltFig <- exv.pltFig + theme(panel.spacing = unit(0.1, "lines")) + 
        ylab("Price/lb") + xlab("Year") + scale_y_continuous(label=dollar_format()) +
      #  ggtitle(paste0("Ex-vessel price in the ", input$exv_zone, 
       #                " region by species and gear type ($/lb).")) +
        theme_economist() + scale_colour_economist() +
        theme(plot.title = element_text(size = 20),
              strip.text = element_text(size=18),
              axis.text.x = element_text(size=14, color="black"),
              axis.text.y = element_text(size=14, color="black"),
              axis.title.x = element_text(size=18),
              axis.title.y = element_text(size=18))
    }
    print(exv.pltFig)
  }, height = reactive(length(unique(exv_datasetInput()$specgrp))*200)) ## close renderPlot
  #### create text below figure if needed
  output$exv_txt <- renderText({
    exv.txtOut <-  ""
    if(any(input$exv_var%in%c("price","exves.val.m"))) {
      exv.txtOut <- paste(exv.txtOut, "Dollar values are in nominal terms (not adjusted for inflation).")
    }
    # if(any(grepl("GOA|AK", input$exv_zone)) & input$exv.sector!="All Sectors"){
    #   exv.txtOut <- "'All Sectors' is the only available sector in the GOA and AK."}
    if(validEntry$useAllGear){
      if(exv.txtOut!="") {paste0(exv.txtOut, " \n ")}
      exv.txtOut <- paste(exv.txtOut, "The gear types ", paste(input$exv.gear, collapse = ", "),
                          "is/are not available for the selected species, zone, and sector.")}
    exv.txtOut
  })## close renderText
  #### render ex-vessel table
  output$exv_tab <- renderTable({
    xtable(dcast(exv_datasetInput(), specgrp+zone+gear+sector+area ~ year, 
                 value.var=input$exv_var), type="html")
  })## close renderTable
  #### Create ex-vessel data for download
  output$exv_downloadData <- downloadHandler(
    filename = "exvdata.csv",
    # filename = function() {
    #   paste(input$dataset, ".csv", sep = "")
    # },
    content = function(file) {write.csv(exv_datasetInput(), file, row.names = FALSE)},
    contentType = "csv"
    ) ## close downloadHandler
})