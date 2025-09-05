#' Metadata for PSESV
#'
#' Read in metadata from for given article number and viz number
#'
#' @param articleno Article number pulled from metadata file
#' @param vizno Visualization number pulled from metadata file
#' @export

###read in metadata from for given article number and viz number
psesvmetadata <-function(articleno,vizno){

  ##read in metadata -note filepath may need to be modified if there are subfolders
  metadata <- data.table::fread('https://raw.githubusercontent.com/PSMFC-FIN/psesv/main/metadata.csv')

  vizmetadata <-
    dplyr::filter (metadata, art_number == articleno & viz_id == vizno)

  return(vizmetadata)

}

