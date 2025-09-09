shinyServer(function(input, output, session) {
  output$wsl_plt <- renderPlot({
    if(any(grepl("GOA|AK", input$wsl.zone)) & input$wsl.sector!="All"){
      # browser()
      pltDat <- subset(econ$wsl[c(wsl.vars,input$wsl.var)], year %in% seq(input$wsl.year[1],input$wsl.year[2]) & 
                         species %in% input$wsl.species & product %in% input$wsl.product & 
                         zone %in% input$wsl.zone & sector == "All")
    }else{
      pltDat <- subset(econ$wsl[c(wsl.vars,input$wsl.var)], year %in% seq(input$wsl.year[1],input$wsl.year[2]) & 
                         species %in% input$wsl.species & product %in% input$wsl.product & 
                         zone %in% input$wsl.zone & sector %in% input$wsl.sector)
    }
    if(input$wsl.var=="wholesale.value"){
      tblplt <- ggplot(pltDat, aes(x=year, y=wholesale.value)) + geom_bar(position="dodge",stat="identity") +
        facet_grid(species~zone, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) + ylab("Value ($ millions)")
    }
    if(input$wsl.var=="product.weight.mt"){
      tblplt <- ggplot(pltDat, aes(x=year, y=product.weight.mt)) + geom_bar(position="dodge",stat="identity") +
        facet_grid(species~zone, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) + ylab("Production (mt)")
    }
    if(input$wsl.var=="price.lb"){
      tblplt <- ggplot(pltDat, aes(x=year, y=price.lb)) + geom_line(position="dodge",stat="identity") + #width = 1, 
        facet_grid(species~zone, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) + ylab("Price/lb")
    }
    print(tblplt)
  }) ## close renderPlot
  output$wsl_txt <- renderText({
    txtOut <-  ""
    if(any(grepl("GOA|AK", input$wsl.zone)) & input$wsl.sector!="All"){
      txtOut <- "'All' is the only available sector for the GOA and AK regions."}
    txtOut
  }) ## close renderText
  output$wsl_tab <- renderTable({
    if(any(grepl("GOA|AK", input$wsl.zone)) & input$wsl.sector!="All"){
      tblDat <- subset(econ$wsl[c(wsl.vars,input$wsl.var)], year %in% seq(input$wsl.year[1],input$wsl.year[2]) & species %in% input$wsl.species & 
                         product %in% input$wsl.product & zone %in% input$wsl.zone & 
                         sector =="All")
    }else{
      tblDat <- subset(econ$wsl[c(wsl.vars,input$wsl.var)], year %in% seq(input$wsl.year[1],input$wsl.year[2]) & species %in% input$wsl.species & 
                         product %in% input$wsl.product & zone %in% input$wsl.zone & 
                         sector %in% input$wsl.sector)
    }
    xtable(dcast(tblDat, species+zone+product+sector ~ year, value.var=input$wsl.var), type="html")
  }) ## close renderTable
  output$exv_plt <- renderPlot({
    if(any(grepl("GOA", input$exv.zone)) & input$exv.sector!="All Sectors"){
      # browser()
      # updateSelectInput(session, inputId = "sector", selected = "All Sectors")
      pltDat <- subset(econ$exv[c(exv.vars,input$exv.var)], year %in% seq(input$exv.year[1],input$exv.year[2]) & 
                         specgrp %in% input$exv.specgrp & gear %in% input$exv.gear & 
                         zone %in% input$exv.zone & sector == "All Sectors")
    }else{
      #browser()
      pltDat <- subset(econ$exv[c(exv.vars,input$exv.var)], year %in% seq(input$exv.year[1],input$exv.year[2]) & 
                         specgrp %in% input$exv.specgrp & gear %in% input$exv.gear & 
                         zone %in% input$exv.zone & sector %in% input$exv.sector)
    }
    if(input$exv.var=="exves.val.m"){
      tblplt <- ggplot(pltDat, aes(x=year, y=exves.val.m)) + geom_bar(position="dodge",stat="identity") +
        facet_grid(specgrp~zone, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) + ylab("Value ($ millions)")
    }
    if(input$exv.var=="tons.k"){
      tblplt <- ggplot(pltDat, aes(x=year, y=tons.k)) + geom_bar(position="dodge",stat="identity") +
        facet_grid(specgrp~zone, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) + ylab("Landings (mt)")
    }
    if(input$exv.var=="price"){
      tblplt <- ggplot(pltDat, aes(x=year, y=price)) + geom_line(position="dodge",stat="identity") +
        facet_grid(specgrp~zone, scales="fixed") + theme(panel.spacing = unit(0.1, "lines")) + ylab("Price/lb")
    }
    print(tblplt)
  }) ## close renderPlot
  output$exv_txt <- renderText({
    txtOut <-  ""
    if(any(grepl("GOA", input$exv.zone)) & input$exv.sector!="All Sectors"){
      txtOut <- "'All Sectors' is the only available sector in the GOA."}
    txtOut
  })## close renderText
  output$exv_tab <- renderTable({
    if(any(grepl("GOA", input$exv.zone)) & input$exv.sector!="All Sectors"){
      tblDat <- subset(econ$exv[c(exv.vars,input$exv.var)], year %in% seq(input$exv.year[1],input$exv.year[2]) & specgrp %in% input$exv.specgrp & 
                         gear %in% input$exv.gear & zone %in% input$exv.zone & 
                         sector =="All Sectors")
    }else{
      tblDat <- subset(econ$exv[c(exv.vars,input$exv.var)], year %in% seq(input$exv.year[1],input$exv.year[2]) & specgrp %in% input$exv.specgrp & 
                         gear %in% input$exv.gear & zone %in% input$exv.zone & 
                         sector %in% input$exv.sector)
    }
    xtable(dcast(tblDat, specgrp+zone+gear+sector ~ year, value.var=input$exv.var), type="html")
  })## close renderTable
})