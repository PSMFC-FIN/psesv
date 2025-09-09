psesvroot <- "http://psesv.psmfc.org/"


###dashboard header
psesvheader <- dashboardHeader(titleWidth = "95%",
title = (div(includeHTML(paste(psesvroot,"dashboardheader.html", sep="")))))


####dashboard sidebar
psesvsidebar <- function(articleno, vizno) {
  

  sidebar<-
    dashboardSidebar(width = 300,
                     fluidRow(column(
                       width = 12,
                       psesvprintmetadata(articleno, vizno) )))

  return(sidebar)

}


####dashboard body
psesvbody <- function(appvar) {
  body <- dashboardBody(
  tags$head(includeCSS(path = paste(psesvroot,"style/appstyle.css",sep=""))),
  tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-113916739-1'></script>
                 <script>window.dataLayer = window.dataLayer || [];
                 function gtag(){dataLayer.push(arguments);}
                 gtag('js', new Date());
                 gtag('config', 'UA-113916739-1');
                 </script>")),
  fluidRow(
    appvar
  ))

  return(body)
}


###read in metadata from for given article number and viz number
psesvmetadata <-function(articleno,vizno){

  ##read in metadata -note filepath may need to be modified if there are subfolders
  metadata <- read.csv(paste(psesvroot,"metadata.csv",sep=""))

  vizmetadata <-
   dplyr::filter (metadata, art_number == articleno & viz_id == vizno)

  return(vizmetadata)

}


###print metadata for sidebar
psesvprintmetadata <- function(articleno, vizno) {

  vizmetadata <- psesvmetadata(articleno, vizno)

### need to decide whether to use relative or absolute links here, relative links
### may not work if apps can be in subfolders

  articleurl <- paste(psesvroot, vizmetadata$art_url, sep = "")
  methodsfile <-
    paste (psesvroot, "METHODS/", vizmetadata$art_methodsfile, sep = "")

  articleinfo <-  tags$div(
    class = "articleinfo",
    
    tags$p(
      
      style="padding-top:15px;
      font-size:16px;",
      tags$a(href = articleurl,
             icon("arrow-left", "fa-lg"),
             HTML("&nbsp"),
             "Return to article"),
      
      HTML("&nbsp"),
      tags$br(),
      tags$a(href = psesvroot,
             icon("arrow-left", "fa-lg"),
             HTML("&nbsp"),
             "Return to PSESV")
    ),
    tags$h2(vizmetadata$viz_title),
    tags$p(
      vizmetadata$authors,
      tags$br(),
      "Article Number: ",
      vizmetadata$art_number,
      tags$br(),
      "Version of Record Online: ",
      vizmetadata$art_versiondate,
      tags$br(),
      "DOI: ",
      vizmetadata$art_doi
    ),
    tags$p(tags$a(href = methodsfile,
                  icon("cogs", "fa-lg"),
                  HTML("&nbsp"),
                  "Methods")),

    HTML (psesvrelatedviz(articleno,vizno))
   
    
    )
  

  return(articleinfo)


}



#### dashboardpage
psesvdashboard <- function(articleno, vizno, app) {
  dashboard <- dashboardPage(skin="blue",
                             title=paste("PSESV-",psesvmetadata(articleno, vizno)$art_number, ": ", psesvmetadata(articleno, vizno)$viz_title),
                             psesvheader,
                             psesvsidebar(articleno, vizno),
                             psesvbody(app))
  return(dashboard)


}


###generates HTML for related visualizations if they exist (other visualizations with same article number)
psesvrelatedviz <- function(articleno, vizno) {
  vizmetadata <-
    dplyr::filter (read.csv(paste(psesvroot,"metadata.csv",sep="")), art_number == articleno & viz_id != vizno)
  
  htmlcode <- "<p>Related visualizations</br>"
  
  for (i in 1:nrow(vizmetadata)){
    
    vizurl <- paste(psesvroot, vizmetadata$viz_url, sep = "")
    viztitle <- vizmetadata$viz_title
    vizlinkhtml <- paste('<a href="',vizurl,'"> <i class="fa fa-line-chart fa-lg" aria-hidden="true"></i> &nbsp ',
                         viztitle,'</a><br>',
                         sep="")
    
    htmlcode <- c(htmlcode,vizlinkhtml)

  }

  
  htmlcode<- c(htmlcode, "</p>") 
  
  novizhtml <- ""
  
  if (nrow(vizmetadata)>=1) {
    return(htmlcode)}
    else {return (novizhtml)}
    
}  
  


  
  
  
  
  
  
  
  








