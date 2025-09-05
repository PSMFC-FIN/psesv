#' Shiny dashboard body for PSESV
#'
#' Includes tags and css for body o app and uses fluidRow() in dashboard body.
#'
#' @param appvar An object returned from fluidPage()
#' @export

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
