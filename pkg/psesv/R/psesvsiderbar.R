#' Shiny dashboard sidebar for PSESV
#'
#' Sets up sidebar for PSESV with article number and viz number.
#'
#' @param articleno Article number pulled from metadata file
#' @param vizno Visualization number pulled from metadata file
#' @export

####dashboard sidebar
psesvsidebar <- function(articleno, vizno) {

  sidebar<-
    dashboardSidebar(width = 300,
                     fluidRow(column(
                       width = 12,
                       psesvprintmetadata(articleno, vizno) )))

  return(sidebar)

}
