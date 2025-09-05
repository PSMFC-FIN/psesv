
choicesSpecies <- setNames(as.numeric(results$SN$SPECIES_CODE), results$SN$COMMON_NAME)

shinyUI(
  dashboardPage(
       title="PSESV-1: Visualizations of groundfish distributions from the AFSC bottom trawl surveys",
    
    dashboardHeader(
        	 titleWidth="95%",
	 title=div(a(style="color: #ffffff", href='/PSESV_INDEX-2017.html',img(height=70,width=70,src='http://www.psmfc.org/wp-content/themes/pacific-state-marine-fisheries-commission/images/logo.png'),"Pacific States E-journal of Scientific Visualizations (PSESV)")
            )
    ),

          

    dashboardSidebar(width=300,
      fluidRow(
        column(width=12,
                          
               HTML('<BR><left><top><B>Visualizations of groundfish distributions from the Alaska Fisheries Science Center bottom trawl surveys </b><BR>
                                                      <font size="1">Author:  Steven J. Barbeaux <BR>
                                                      Article Number:  1 <BR>
                                                      Version of Record Online:  17 JULY 2017 <BR>
                                                      DOI: Not yet available<BR><BR></font></top></left>'),
          
               HTML('<a href="/METHODS/PSESV-1/Methods-PSESV-1.pdf"><i class="fa fa-cogs fa-fw" aria-hidden="true"></i>&nbsp;Methods </a><BR>'), 
          
         radioButtons("survey", 
                     label = h4("Survey"), 
                     choices = list("Western GOA" = 46,
                                    "Central GOA" = 47,
                                    "Eastern GOA" = 48,
                                    "EBS" = 98, 
                                    "AI" = 52,
                                    "Slope" = 78),
                     selected = 47),
        
        radioButtons("AREA_W",width="100%", 
                     label = h4("Stratum area weighted?(GOA only)"), 
                     choices = list("YES" = TRUE, 
                                    "NO" = FALSE), 
                     selected = FALSE),
        
        
        selectInput("plotT",width="100%",
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
                      
                    ),
                    selected = 5),
        
        
        selectInput("species",width="100%",
                    label = h4("Species"), 
                    choices = choicesSpecies,
                    selected = 21720)
        
        ##plotOutput(outputId="plot2", width="98%",height=200)
        
       
    
  ))),
          
    dashboardBody(
      tags$head( includeCSS(path="www/style.css")),
     
     
      fluidRow(
        


        tabBox(side="left",selected="Distributions",width="80%",height=1100,
               tabPanel("Distributions",height=1100,

                  
                  box(width="80%",HTML('<BR><center><B><font size="4">Distribution by size from AFSC bottom trawl surveys</font></B></center>'),sliderInput("cat1","Length category", 1, 5, 1, step = 1,ticks=T, 
                              animate=animationOptions(interval=8000, loop = FALSE,
                                                       playButton = "Play", pauseButton = "Stop"))),
                  
                  plotOutput(outputId="plot4",width="80%",height=800)
                  
                  
             
                  
                  ),
               
               tabPanel("Centroids",
                        HTML('<BR><center><B><font size="5">Centers of distribution by size from AFSC bottom trawl surveys</font></B></center>'),
                        plotOutput(outputId="plot1",width="100%")),
               
               tabPanel("Temps",
                        HTML('<BR><center><B><font size="5">Mean bottom temperature anomaly by region. </font></B></center>'),
                        plotOutput(outputId="plot2",width="100%")),
                
               tabPanel("Lengths",
                        HTML('<BR><center><B><font size="5">Length distribution</font></B></center>'),
                        plotOutput(outputId="plot3",width="100%")))

    ))
))
          


