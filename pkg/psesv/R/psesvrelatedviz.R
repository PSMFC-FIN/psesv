#' Print html for related PSESV visualizations
#'
#' generates HTML for related visualizations if they exist (other visualizations with same article number)
#'
#' @param articleno Article number pulled from metadata file
#' @param vizno Visualization number pulled from metadata file
#' @export

###generates HTML for related visualizations if they exist (other visualizations with same article number)
psesvrelatedviz <- function (articleno, vizno)
{
  vizmetadata <- dplyr::filter(read.csv(paste(psesvroot, "metadata.csv",
                                              sep = "")), art_number == articleno & viz_id != vizno)
  htmlcode <- "<p>Related visualizations</br>"
  for (i in 1:nrow(vizmetadata)) {
    vizurl <- paste(psesvroot, vizmetadata$viz_url[i], sep = "")
    viztitle <- vizmetadata$viz_title[i]
    vizlinkhtml <- paste("<a href=\"", vizurl, "\"> <i class=\"fa fa-line-chart fa-lg\" aria-hidden=\"true\"></i> &nbsp ",
                         viztitle, "</a><br>", sep = "")
    htmlcode <- c(htmlcode, vizlinkhtml)
  }
  htmlcode <- c(htmlcode, "</p>")
  novizhtml <- ""
  if (nrow(vizmetadata) >= 1) {
    return(htmlcode)
  }
  else {
    return(novizhtml)
  }
}
