##change notes:
##replaced tabSetPanel with tabBox
##added div to content of each tabPanel with wrapperdiv class - resolves issue with tall plots
##modified styling of plotOutput for seasonal_nmfs_plot and mothly_nmfs_plot for overflow height and width
##styled paragraph tags, added hr() 

app <- fluidPage(

  tags$head(tags$style(".wrapperdiv{padding-left:25px; padding-right:15px; height:100%}")),
  
##replaced tabSetPanel with tabBox  
  tabBox(width="85%",
    tabPanel("NMFS Seasonal Anomaly",
             div(class="wrapperdiv",
             
             titlePanel("Seasonal sea surface temperature anomaly"),

             p(style="font-size:16px; margin-top:15px;","Select NMFS reporting area(s) to view winter and summer temperature anomalies. Each winter season includes October - December, plus January to March of the following year (e.g., Winter 2003 includes Oct - Dec 2003 and Jan - Mar 2004). Winter 2002 and 2018 have been omitted because they were incomplete and could thus bias anomaly calculations."),
             p(style="font-size:16px;","Depth filters remove statistical areas whose average depth is outside of the selected depth range. The default depth is set at 0 - 200m, a general approximation for the continental shelf. If the selected depth leads to a plotting error try a different depth range."),
            p(style="font-size:16px;","See the map tab for spatial reference of NMFS areas."),
            hr(),
             fluidRow(
               column(4,
                      pickerInput("dnmfs","Select NMFS reporting area", choices=mynmfs, options = list(`actions-box` = TRUE),multiple = T)),
               column(6,
                      sliderInput("range", "Depth (m):",min = -5000, max = 0,value = c(-200,0))
               )),
               plotOutput('seasonal_nmfs_plot', width="95%") %>% withSpinner(color="#0dc5c1"))
    ),
    tabPanel("NMFS Monthly Anomaly",
             div(class="wrapperdiv",
             titlePanel("Monthly sea surface temperature anomaly"),
             p(style="font-size:16px; margin-top:15px;","Select NMFS reporting area(s) and month to view temperature anomalies (standard deviations) for those areas and that month."),

             p(style="font-size:16px;","Depth filters remove statistical areas whose average depth is outside of the selected depth range. The default depth is set at 0 - 200m, a general approximation for the continental shelf. If the selected depth leads to a plotting error try a different depth range."),

             p(style="font-size:16px;","See the map tab for spatial reference of NMFS areas."),
             hr(),
             fluidRow(
               column(4,
                      pickerInput("dnmfs_month","Select NMFS reporting area", choices=mynmfs, options = list(`actions-box` = TRUE),multiple = T)),
               column(4,
                      pickerInput("dmonth","Select month", choices=month.name, options = list(`actions-box` = TRUE),multiple = T)),
               column(4,
                      sliderInput("range_month", "Depth (m):",min = -5000, max = 0,value = c(-200,0))
               )),
             
                  plotOutput('monthly_nmfs_plot', height="100%", width="95%") %>% withSpinner(color="#0dc5c1"))

),
    tabPanel("Map of NMFS areas",
             div(class="wrapperdiv",
             br(),
             p(style="font-size:16px;",
               "Map may take a few moments to load. Hover over areas to identify NMFS areas for reference."),
             leafletOutput("map"))
    )
  )
)

