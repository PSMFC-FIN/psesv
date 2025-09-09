app <-
  
  
  fluidPage(
  
  tabBox(selected = "wsl", id = "id",width="100%",
    tabPanel(title = "Wholesale", value = "wsl",
             sidebarLayout(
               sidebarPanel(width=3,
                            selectInput(inputId = "wsl_var", label = h4("Variable"),
                                        choices = list(value  = "ws.val", price = "ws.price.lb",
                                                       volume = "product.weight.mt"),
                                        selected = "value",
                                        multiple = FALSE),
                            sliderInput(inputId = "wsl.year", label = "Year",
                                        min = 2003, max=2019, step =1,
                                        value = c(2012,2019), dragRange=TRUE, sep=""),
                            selectInput(inputId = "wsl_zone", label = h4("Zone"),
                                        choices = wsl.zone.list,
                                        selected = "AK",
                                        multiple = FALSE),
                            conditionalPanel(condition = "input.wsl_zone=='AK'",
                                             selectInput(inputId = "ak.wsl.species", label = h4("Species"),
                                                         choices = ak.wsl.spec.list,
                                                         selected = "All Species",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.wsl_zone=='BSAI'",
                                             selectInput(inputId = "bsai.wsl.species", label = h4("Species"),
                                                         choices = bsai.wsl.spec.list,
                                                         selected = "All Species",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.wsl_zone=='GOA'",
                                             selectInput(inputId = "goa.wsl.species", label = h4("Species"),
                                                         choices = goa.wsl.spec.list,
                                                         selected = "All Species",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.wsl_zone=='BSAI'",
                                             selectInput(inputId = "wsl.sector", label = h4("Sector"),
                                                         choices = wsl.sector.list,
                                                         selected = "All Sectors",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.wsl_zone=='AK'",
                                             selectInput(inputId = "ak.wsl.product", label = h4("Product"),
                                                         choices = ak.wsl.product.list,
                                                         selected = "All Products",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.wsl_zone=='BSAI'",
                                             selectInput(inputId = "bsai.wsl.product", label = h4("Product"),
                                                         choices = bsai.wsl.product.list,
                                                         selected = "All Products",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.wsl_zone=='GOA'",
                                             selectInput(inputId = "goa.wsl.product", label = h4("Product"),
                                                         choices = goa.wsl.product.list,
                                                         selected = "All Products",
                                                         multiple = TRUE)),
                            HTML('<p>For details see the  <a style="color:#4288BA;" href="https://www.fisheries.noaa.gov/alaska/commercial-fishing/groundfish-economic-status-reports-gulf-alaska-and-bering-sea-and-aleutian-islands">Economic Status of the Groundfish Fisheries off Alaska</a></p>'),
                            downloadButton(outputId="wsl_downloadData", "Download data", style = "color: #000000; background-color:#dbdbdb;")
               ),
               mainPanel(
                 tags$h3(tags$b(textOutput("wstitle"))),
                 plotOutput(outputId="wsl_plt", height="auto"),
                         textOutput(outputId="wsl_txt"),
                 div(style="overflow-x:scroll;", tableOutput(outputId="wsl_tab")))
             )## close sidebarLayout
    ), ## close tabPanel
    tabPanel(title = "Ex-vessel", value = "exv",
             sidebarLayout(
               #headerPanel("Ex-vessel data of the groundfish catch off Alaska, 2003-2019"),
               sidebarPanel(width=3,
                            selectInput(inputId = "exv_var", label = h4("Variable"),
                                        choices = list(value  = "exves.val.m", price = "exves.price.lb",
                                                       catch = "retcatch.mt.k"),
                                        selected = "value",
                                        multiple = FALSE),
                            sliderInput( inputId = "exv.year", label = "Year",
                                         min = 2003, max=2019, step =1,
                                         value = c(2012,2019), dragRange=TRUE, sep=""),
                            selectInput(inputId = "exv_zone", label = h4("Zone"),
                                        choices = exv.zone.list,
                                        selected = "AK",
                                        multiple = FALSE),

                            conditionalPanel(condition = "input.exv_zone=='AK'",
                                             selectInput(inputId = "ak.exv.specgrp", label = h4("Species"),
                                                         choices = ak.exv.spec.list,
                                                         selected = "All Species",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.exv_zone=='BSAI'",
                                             selectInput(inputId = "bsai.exv.specgrp", label = h4("Species"),
                                                         choices = bsai.exv.spec.list,
                                                         selected = "All Species",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.exv_zone=='GOA'",
                                             selectInput(inputId = "goa.exv.specgrp", label = h4("Species"),
                                                         choices = goa.exv.spec.list,
                                                         selected = "All Species",
                                                         multiple = TRUE)),
                            
                            conditionalPanel(condition = "input.exv_zone=='GOA'",
                                             selectInput(inputId = "exv.area", label = h4("Area"),
                                                         choices = exv.area.list,
                                                         selected = "All Areas",
                                                         multiple = TRUE)),
                            conditionalPanel(condition = "input.exv_zone=='BSAI'",
                                             selectInput(inputId = "exv.sector", label = h4("Sector"),
                                                         choices = exv.sector.list,
                                                         selected = "All Sectors",
                                                         multiple = TRUE)),
                            selectInput(inputId = "exv.gear", label = h4("Gear"),
                                        choices = exv.gear.list,
                                        selected = "All Gear",
                                        multiple = TRUE),
                            HTML('<p>For details see the  <a style="color:#4288BA;" href="https://www.fisheries.noaa.gov/alaska/commercial-fishing/groundfish-economic-status-reports-gulf-alaska-and-bering-sea-and-aleutian-islands">Economic Status of the Groundfish Fisheries off Alaska</a></p>'),
                            downloadButton(outputId="exv_downloadData", "Download data", style = "color: #000000; background-color:#dbdbdb;")
                      
               ),
               mainPanel(
                 tags$h3(tags$b(textOutput("evtitle"))),
                 plotOutput(outputId="exv_plt", height="auto"),
                         textOutput(outputId="exv_txt"),
                         div(style="overflow-x:scroll;", tableOutput(outputId="exv_tab")))
               
               
               ##div(style="overflow-x:scroll;",shiny::dataTableOutput("table2") %>% withSpinner(color="#0dc5c1") )
             ) ## close sidebarLayout
    ) # close tabPanel
  ) )


