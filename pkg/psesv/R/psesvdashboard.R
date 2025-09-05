#' Dashboardpage for PSESV
#'
#' Pulls together PSESV metadata, header, sidebar and body.
#'
#' @param articleno Article number pulled from metadata file
#' @param vizno Visualization number pulled from metadata file
#' @param app An object returned from fluidPage()
#' @export

#### dashboardpage
psesvdashboard <- function(articleno, vizno, app) {
  dashboard <- dashboardPage(skin="blue",
                             title=paste("PSESV-",psesvmetadata(articleno, vizno)$art_number, ": ", psesvmetadata(articleno, vizno)$viz_title),
                             psesvheader,
                             psesvsidebar(articleno, vizno),
                             psesvbody(app))
  return(dashboard)


}
