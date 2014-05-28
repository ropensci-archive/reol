#' Gather EOL References
#' 
#' This function gathers the references on the EOL pages (not the references on the provider pages)
#' 
#' @export
#' 
#' @param MyEOLs A vector of filenames or a list of XMLs for downloaded EOL pages
#' @param output Detail will return a data frame with eolID and reference; counts will return a 
#' dataframe with  eol taxon name, eol ID, and number of references.
#' 
#' @return Returns a data frame with taxon, eol ID, common name, and language.
#' 
#' @seealso \code{\link{GetRichnessScores}} \code{\link{GetCommonNames}} \code{\link{GetIUCNStat}}
#' \code{\link{DataObjectOverview}}
#' 
#' @examples \dontrun{
#' data(MyEOLs)
#' GetReferences(MyEOLs, output="detail")
#' GetReferences(MyEOLs[1], "c")
#' }

GetReferences <- function(MyEOLs, output=c("detail", "counts")) {
  MyEOLs <- RemoveNAFiles(MyEOLs)
  output <- match.arg(output)
  ReferenceList <- matrix(nrow=0, ncol=3)
  colnames(ReferenceList) <- c("Taxon", "eolID", "Reference")
  RefCounts <- matrix(nrow=length(MyEOLs), ncol=3)
  colnames(RefCounts) <- c("Taxon", "eolID", "Number Of References")
  for(i in sequence(length(MyEOLs))) {
    res <- PageProcessing(MyEOLs[i])$taxonConcept
    whichReferences <- which(names(res) == "reference")
    scientificName  <- res[[which(names(res) == grep("ScientificName", names(res), ignore.case=TRUE, value=T))]] #because some are cap and some are not
    RefCounts[i,] <- c(scientificName, res$taxonConceptID, length(whichReferences))
    for(j in sequence(length(whichReferences))) {
      ReferenceList <- rbind(ReferenceList, c(scientificName, res$taxonConceptID, as.character (res[whichReferences[j]])))
      #}
    }
  }
  RefCounts <- data.frame(RefCounts, stringsAsFactors=F)
  ReferenceList <- data.frame(ReferenceList, stringsAsFactors=F)
  if(output == "detail")
    return(ReferenceList)
  if(output == "counts")
    return(RefCounts)
}
