##general change notes:
##added CSS styling
##replaced tabSetPanel with tabBox, set to width 85%
##added div to contain content of each tabPanel with wrapperdiv class 
##added h2() to show titles for 1st and 2nd tabs
##added hr() to tabs 1 and 2
##changed fluidrow for ShinyTableOutput to div(style='overflow-x: scroll', shiny:ddataTableOutput()) to resolve scrolling issue for narrow screen
##replace metadata tab with table, styled with metadata class



app <- fluidPage(
  
  tags$head(tags$style(".wrapperdiv {
                          padding-left:25px; 
                          padding-right:15px; 
                          height:100%} 

                       .description {
                          font-size:16px;}

                       .tableoutput{
                          overflow-x:scroll; 
                          width:98%}
                       
                       .metadata {
                          border-collapse: collapse;
                          width: 100%;
                          font-size:14px;}
                       
                       .metadata td {
                          border: 1px solid #ddd;
                          padding: 8px;}
                       
                       .metadata td:first-child{font-weight:bold;}
                       .metadata tr:nth-child(even){background-color: #f2f2f2;}
                       .metadata tr:hover {background-color: #ddd;}

                      .metadata th {
                          padding-top: 12px;
                          padding-bottom: 12px;
                          padding-left:12px;
                          text-align: left;
                          background-color: #3C8DBC;
                          color: white;
                          font-size:16px;
                       }
                       "
                       
                       
                       
                       )),
  tabBox(width="85%",
    tabPanel("Sea surface temperatures by ADF&G statistical areas",
             
             div(class="wrapperdiv",

             
             h2("Sea surface temperatures by ADF&G statistical areas"),
             p(class="description",  "Select an ADF&G statistical area to view daily data or data averaged by week or month."),
             p(class="description", "See the 'More Information' tab for descriptions of each variable."),
             p(class="description", 
               span(style="font-weight:bold", "WARNING!"),
               "The full dataset has more than 10 million rows so selections with all areas may take a few minutes to load."),
             hr(),
             fluidRow(
               column(4,
                      selectInput("mystattable", "Display data as daily, weekly, or monthly:", 
                                  choices = c("Daily", "Weekly average","Monthly average"))),
               column(4,
                      pickerInput("statarealist","Select ADF&G statistical area", choices=mystats, options = list(`actions-box` = TRUE),multiple = T)),
               column(4,p("Download buttons will deliver the version of the data selected. 'Download RDS' (recommended for R users) will yield smaller files than 'Download csv`."),
                      downloadButton("download_stat_area_csv", "Download csv"),
                      downloadButton("download_stat_area_RDS", "Download RDS"),
                      br())),
            # fluidRow(
             #  column(12,
              #        br(),
               #       shiny::dataTableOutput("table1") %>% withSpinner(color="#0dc5c1")))
            
            div(class="tableoutput", shiny::dataTableOutput("table1") %>% withSpinner(color="#0dc5c1"))
    )),
  tabPanel("Sea surface temperatures by NMFS reporting areas",
           
           div(class="wrapperdiv",
           h2("Sea surface temperatures by NMFS reporting areas"),
          p(class="description", "Select a NMFS reporting area to view daily data or data averaged by week or month."),
           p(class="description", "See the 'More Information' tab for descriptions of each variable."),
          p(class="description", 
            span(style="font-weight:bold", "WARNING!"),
            "The full dataset has more than 10 million rows so selections with all areas may take a few minutes to load."),
           hr(),
           fluidRow(
             column(4,
                    selectInput("mynmfstable", "Display data as daily, weekly, or monthly:", 
                                choices = c("Daily", "Weekly average","Monthly average"))),
             column(4,
                    pickerInput("nmfsarealist","Select NMFS reporting area", choices=mynmfsall, options = list(`actions-box` = TRUE),multiple = T)),
             column(4,
                    p("Download buttons will deliver the version of the data selected. 'Download RDS' (recommended for R users) will yield smaller files than 'Download csv`."),
                    downloadButton("download_nmfs_area_csv", "Download csv"),
                    downloadButton("download_nmfs_area_RDS", "Download RDS"))),
         #  fluidRow(
          #   column(12,shiny::dataTableOutput("table2") %>% withSpinner(color="#0dc5c1")))
         div(style="overflow-x:scroll;",shiny::dataTableOutput("table2") %>% withSpinner(color="#0dc5c1") )
  )),
  tabPanel("More Information",   # Information about data collection.
           div(class="wrapperdiv",
           p(class="description", style="font-weight:bold;", "See article methods for a complete description of the data and extraction process"),
           HTML("<table class='metadata'>
            <tr>
            <th>Variable</th>
            <th>Description</th>
            </tr>
             <tr>
             <td style='width:15%;'>sst.mean</td>
             <td>Average sea surface temperature across the queried spatial stratum (e.g., STAT_AREA or NMFSAREA) during the queried time period (e.g., day, week, month).</td>
             </tr>
             <tr>
             <td>date</td>
             <td>calendar date between 01 Jan 2003 and 10 May 2018</td>
             </tr>
             <tr>
             <td>STAT_AREA</td>
             <td>Alaska Dept of Fish and Game stat6 groundfish management area. For maps of the NMFS reporting areas visit www.adfg.alaska.gov/index.cfm?adfg</td>
             </tr>
             <tr>
             <td>NMFSAREA</td>
             <td>NMFS reporting areas as described in the manuscript text. For maps of the NMFS reporting areas visit https://alaskafisheries.noaa.gov/maps.</td>
             </tr>
             <tr>
             <td>FMP_AREA_C</td>
             <td>Fishery management plan area (e.g., Bering Sea Aleutian Islands, Gulf of Alaska)</td>
             </tr>
             <tr>
             <td>STATEFED</td>
             <td>Flag identifying whether a stat area occurs in state or federal waters</td>
             </tr>
             <tr>
             <td>sst.sd</td>
             <td>For daily STAT_AREA query, standard deviation of daily sst values per STAT_AREA. For coarser spatial or temporal queries, this is the standard deviation of sst values from the daily STAT_AREA, but does not propagate the original standard deviations.</td>
             </tr>
             <tr>
             <td>year</td>
             <td>year corresponding to the days, weeks, or months that were aggregated.td>
             </tr>
             <tr>
             <td>month</td>
             <td>month corresponding to the days or weeks that were aggregated.</td>
             </tr>
             <tr>
             <td>week</td>
             <td>week, beginning on Sunday, corresponding to the days that were aggregated.</td>
             </tr>
             <tr>
             <td>julian</td>
             <td>julian day for a given date</td>
             </tr>
             <tr>
             <td>min and max lons and lats</td>
             <td>bounding box coordinates for a given STAT_AREA</td>
             </tr>
             <tr>
             <td>m.depth</td>
             <td>mean depth for a given STAT_AREA in meters. This field is dropped for coarser spatial areas.</td>
             </tr>
             <tr>
             <td>sd.depth</td>
             <td>standard deviation of depth for a given STAT_AREA. Useful for identifiying regions occurring on the continental slope</td>
             </tr>
             <tr>
             <td>sd.depth</td>
             <td>standard deviation of depth for a given STAT_AREA. Useful for identifiying regions occurring on the continental slope</td>
             </tr>
             </table>")
           )
  )
))
