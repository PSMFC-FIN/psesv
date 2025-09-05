#' print metadata for sidebar
#'
#'   print metadata for sidebar
#' @param articleno Article number pulled from metadata file
#' @param vizno Visualization number pulled from metadata file
#' @export

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
                  target="_blank",
                  icon("cogs", "fa-lg"),
                  HTML("&nbsp"),
                  "Methods")),

    HTML (psesvrelatedviz(articleno,vizno))


    )


  return(articleinfo)


}
