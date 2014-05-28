#' Gather IUCN Status for EOL Pages
#' 
#' This function gathers IUCN threat statuses from EOL pages if they are reported.
#'   
#' @export
#' 
#' @param MyEOLs A vector of filenames or a list of XMLs for downloaded EOL pages
#'   
#' @return Returns a data frame with taxon, eol ID, and IUCN status
#' @seealso \code{\link{GetRichnessScores}} \code{\link{GetCommonNames}} \code{\link{GetReferences}}
#' \code{\link{DataObjectOverview}}
#' @examples \dontrun{
#' data(MyEOLs)
#' GetIUCNStat(MyEOLs)
#' GetIUCNStat(MyEOLs[3])
#' }

GetIUCNStat <- function(MyEOLs) {
  MyEOLs <- RemoveNAFiles(MyEOLs)
  IUCN <- matrix(ncol=3, nrow=length(MyEOLs))
  colnames(IUCN) <- c("Taxon", "eolID", "IUCNstat")
  for(i in sequence(length(MyEOLs))){
    res <- PageProcessing(MyEOLs[i])
    IUCNstat <- NA
    if(length(grep("IUCNConservationStatus", res)) > 0)
      IUCNstat <- res[[which(res == grep("IUCNConservationStatus", res, value=TRUE))]]$description
 scientificName  <- res$taxonConcept[[which(names(res$taxonConcept) == grep("ScientificName", names(res$taxonConcept), ignore.case=TRUE, value=TRUE))]] #because some are cap and some are not
  IUCN[i,] <- c(scientificName, res$taxonConcept$taxonConceptID, IUCNstat)
  }
  return(data.frame(IUCN, stringsAsFactors=FALSE))
}
