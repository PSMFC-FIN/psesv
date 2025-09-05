#' PSESV root directory
#'
#' PSESV root directory
#'
#' @export
psesvroot <- "https://psesv.psmfc.org/"


#' Shiny dashboard header for PSESV
#'
#' Creates object dashboard header html from psesv server.
#'
#' @export
psesvheader <- dashboardHeader(titleWidth = "95%",
                               title = (div(includeHTML(paste(psesvroot,"dashboardheader.html", sep="")))))
