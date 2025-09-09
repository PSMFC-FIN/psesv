##change notes:
##added wrapper div to resolve background color issue
##added h2() for title
##added hr()
##styled p() tag for larger font



app <- fluidPage(

##    title="Sea surface temperatures by state statistical area",
tags$div(style="background-color:white; padding-left:25px; padding-top:15px; padding-right:10px;",
         h2("Sea surface temperatures by ADF&G statistical area"),
         tags$hr(),
    p(style="font-size:16px;",
      "Map may take a few moments to load. Click on a state statistical area polygon to view the sea surface temperature time series for that area."),
    leafletOutput("map") %>% withSpinner(color="#0dc5c1"),
    fluidRow(column(3,
                    selectInput("graph", "Display data as daily, weekly, or monthly:", 
                                           choices = c("Daily", "Weekly average","Monthly average"))),
             column(4,offset=1,
                    br(),
                    br(),
                    actionButton("reset","Click to reset\nplot below")),
             column(4,sliderInput("yearrange", 
                                           "Year:",
                                           min = 2003, 
                                           max = max(mydata$year),
                                           value = c(2003,max(mydata$year)),
                                           sep=""))),
             plotOutput('mainplot')
    )
)
    


