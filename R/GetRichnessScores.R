#' Gather EOL Richness Score Information
#' 
#' EOL hosts richness scores for each species ranging from 0-100.  These are calculated on how much
#' information data is available for that species, based on the amount of text on a page, how many 
#' multimedia files are present, how many different topics are covered, how many sources 
#' contribute, and whether the information has been reviewed.
#' 
#' @export
#' @param MyEOLs A vector of filenames for downloaded EOL pages OR a list of XML data stored as an 
#' R object
#' 
#' @return Returns a data frame with taxon, eol ID and numerical richness score
#' 
#' @seealso \code{\link{GetCommonNames}} \code{\link{GetIUCNStat}} \code{\link{GetReferences}}
#' \code{\link{DataObjectOverview}}
#' 
#' @examples \dontrun{
#' data(MyEOLs)
#' GetRichnessScores(MyEOLs[1:2])
#' }

GetRichnessScores <- function(MyEOLs) {
  MyEOLs <- RemoveNAFiles(MyEOLs)
  richnessDF <- matrix(nrow=length(MyEOLs), ncol=3)
  for(i in sequence(length(MyEOLs))) {
    richnessData <- rep(NA, 3)
    res <- PageProcessing(MyEOLs[i])$taxonConcept
    scientificName  <- res[[which(names(res) == grep("ScientificName", names(res), ignore.case=TRUE, value=T))]] #because some are cap and some are not
    richnessData <- c(scientificName, res$taxonConceptID, res$additionalInformation$richness_score)
    richnessDF[i,] <- richnessData
  }
  richnessDF <- as.data.frame(richnessDF, stringsAsFactors=FALSE)
  colnames(richnessDF) <- c("Taxon", "eolID", "Richness_Score")
  return(richnessDF)
}
