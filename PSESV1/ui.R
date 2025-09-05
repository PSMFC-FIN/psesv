##set article number and viz number to pull in metadata
articleno <-1
vizno <- 1

##read in metadata

metadata <- fread('https://raw.githubusercontent.com/PSMFC-FIN/psesv/main/metadata.csv')
vizmetadata <- filter (metadata, art_number==articleno & viz_id==viz_id)

#add relative path to create full URL
articleurl <- paste(vizmetadata$art_url, sep="")
methodsfile <-paste (vizmetadata$art_methodsfile,sep="")


results<-readRDS('data/results.rds')
choicesSpecies <- setNames(as.numeric(results$SN$SPECIES_CODE), results$SN$COMMON_NAME)




shinyUI(


  dashboardPage(skin="blue",
       title="PSESV-1: Visualizations of groundfish distributions from the AFSC bottom trawl surveys",
    
    dashboardHeader(
        	 titleWidth="95%",
	         title =div(includeHTML("https://psesv.psmfc.org/dashboardheader.html"))
	 
    ),

          

    dashboardSidebar(width=300,
      fluidRow(
        column(width=12,
               
               ## populate article information
               
               tags$div(class="articleinfo",
                        tags$h2(tags$a(href=articleurl,vizmetadata$viz_title)),
                        tags$p(vizmetadata$authors, tags$br(),
                               "Article Number: ", vizmetadata$art_number, tags$br(),
                               "Version of Record Online: ", vizmetadata$art_versiondate, tags$br(),
                               "DOI: ", vizmetadata$art_doi
                               ),
                        tags$p(
                          tags$a(href= methodsfile,
                                 icon("cogs", "fa-lg"),
                                 " Methods"
                                   )
                              )
                        )))),



          
    dashboardBody(
      tags$head( includeCSS(path="https://psesv.psmfc.org/style/appstyle.css")),

        tabBox(width="100%",
          tabPanel("Distributions",
            sidebarLayout(
             sidebarPanel(width=3,

                  radioButtons(inputId ="survey", 
                               label = h4("Survey"), 
                               choices = list("Western GOA" = 46,
                                              "Central GOA" = 47,
                                              "Eastern GOA" = 48,
                                              "EBS" = 98, 
                                              "AI" = 52,
                                              "Slope" = 78),
                                selected = 47),
        
                  # radioButtons(inputId ="AREA_W",width="100%", 
                  #              label = h4("Stratum area weighted?(GOA only)"), 
                  #              choices = list("YES" = TRUE, 
                  #                             "NO" = FALSE), 
                  #              selected = FALSE),
        
        
                  selectInput(inputId ="plotT",width="100%",
                              label = h4("Plot"), 
                              choices = list(
                                             'Temp. and depth' = 5,
                                            
                                             'Location' = 6
                                              ),
                              selected = 5),
        
        
                  selectInput(inputId ="species",width="100%",
                              label = h4("Species"), 
                              choices = choicesSpecies,
                              selected = 21720),

                  sliderInput("cat1","Length category", 1, 5, 1, step = 1,ticks=T, 
                              animate=animationOptions(interval=8000, loop = FALSE,
                                                       playButton = "Play", pauseButton = "Stop"))
                  ),


            mainPanel(
                      h3(class="tabtitle","Distribution by size from AFSC bottom trawl surveys"),
                      plotOutput(outputId="plot4",width="80%",height=800)
                      )
            )  ## sidebarbLayout end
          ),   ##  tabPanel end
               
          tabPanel("Centroids",
            sidebarLayout(
              sidebarPanel(width=3,

                  radioButtons(inputId ="survey.cent", 
                               label = h4("Survey"), 
                               choices = list("Western GOA" = 46,
                                              "Central GOA" = 47,
                                              "Eastern GOA" = 48,
                                              "EBS" = 98, 
                                              "AI" = 52,
                                              "Slope" = 78),
                           selected=47),
        
                  radioButtons(inputId ="AREA_W.cent",width="100%", 
                               label = h4("Stratum area weighted?(GOA only)"), 
                               choices = list("YES" = TRUE, 
                                              "NO" = FALSE), 
                               selected = FALSE),
        
        
                  selectInput(inputId ="plotT.cent",width="100%",
                              label = h4("Plot"), 
                              choices = list(
                                             'Temp. and depth' = 5,
                                             'Temp. and depth all years' = 3,
                                             'Temp. and depth by shelf temp.' = 1,
                                             'Temp. and depth by sex' = 7,
                                             'Location' = 6,
                                             'Location all years' = 4,
                                             'Location by shelf temp.' = 2,
                                             'Location by sex' = 8,
                                             'Depth and eastings' = 9
                                              ), selected=5
                             ),
        
        
                  selectInput(inputId ="species.cent",width="100%",
                              label = h4("Species"), 
                              choices = choicesSpecies, selected=21720
                              )),

              mainPanel(
                        h3(class="tabtitle", "Centers of distribution by size from AFSC bottom trawl surveys"),
                        plotOutput(outputId="plot1",width="100%")
                        )
              ) ## sidebarLayout end
          ),    ## tabPanel end


               
        tabPanel("Temps",
          sidebarLayout(
            sidebarPanel(width=3,
                radioButtons(inputId ="survey.temp", 
                             label = h4("Survey"), 
                             choices = list("Western GOA" = 46,
                                            "Central GOA" = 47,
                                            "Eastern GOA" = 48,
                                            "EBS" = 98, 
                                            "AI" = 52,
                                            "Slope" = 78),
                              selected=47)),

            mainPanel(
                      h3(class="tabtitle", "Mean bottom temperature anomaly by region"),
                      plotOutput(outputId="plot2",width="100%"))
            ) ## sidebarLayout end
          ),  ## tabPanel end 
                
        tabPanel("Lengths",
          sidebarLayout(
            sidebarPanel(width=3,
                radioButtons(inputId ="survey.length", 
                             label = h4("Survey"), 
                             choices = list("Western GOA" = 46,
                                            "Central GOA" = 47,
                                            "Eastern GOA" = 48,
                                            "EBS" = 98, 
                                            "AI" = 52,
                                            "Slope" = 78),
                             selected=47),
        
        
                selectInput(inputId ="species.length",width="100%",
                            label = h4("Species"), 
                            choices = choicesSpecies,selected=21720
                            )),
            mainPanel(
                      h3(class="tabtitle", "Length distributions"),
                      plotOutput(outputId="plot3",width="100%")
                      )
            ) ## sidebarLayout end 
          ),  ##tabPanel end

        tabPanel("Biomass",
          sidebarLayout(
            sidebarPanel(width=3,
                radioButtons(inputId ="survey.BIO", 
                             label = h4("Survey"), 
                             choices = list("Western GOA" = 46,
                                            "Central GOA" = 47,
                                            "Eastern GOA" = 48,
                                            "EBS" = 98, 
                                            "AI" = 52,
                                            "Slope" = 78),
                             selected=47),
        
                selectInput(inputId ="species.BIO",width="100%",
                            label = h4("Species"), 
                            choices = choicesSpecies,
                            selected=21720)),

            mainPanel(
                      h3(class="tabtitle", "Biomass"),
                      plotOutput(outputId="plot5",width="100%")
                      )
            )  ## sidebarLayout end
          ),   ## tabPanel end

        tabPanel("Population",
          sidebarLayout(
            sidebarPanel(width=3,
                radioButtons(inputId ="survey.POP", 
                             label = h4("Survey"), 
                             choices = list("Western GOA" = 46,
                                            "Central GOA" = 47,
                                            "Eastern GOA" = 48,
                                            "EBS" = 98, 
                                            "AI" = 52,
                                            "Slope" = 78),
                              selected=47),
        
                selectInput(inputId ="species.POP",width="100%",
                            label = h4("Species"), 
                            choices = choicesSpecies,
                            selected = 21720)),

            mainPanel(
                      h3(class="tabtitle", "Population"),
                      plotOutput(outputId="plot6",width="100%")
                      )
            ) ## sidebarLayout end
          )   ## tabPanel end

        ) ## tabBox end
      )   ## dashboardBody end
    )     ## dashboardPage end
  )       ## shinyUI end

          


