#' Gathers A List Of Taxonomic Parents and Offspring
#'
#' This function goes through the hierarchy pages to collect taxonomic offspring and parantage.
#' 
#' @export
#' 
#' @param MyHiers A vector or single filename for downloaded hierarchy pages
#' @param MyHier A single filename for downloaded hierarchy pages
#'
#' @return \code{TaxonChildren} will report the primary offspring of a taxon if the hierarchy page
#' reports this information.  \code{TaxonParents} will return the list of taxonomic parentage
#'
#' @seealso \code{\link{MakeTreeData}}
#'
#' @examples \dontrun{
#' # Simple example using Reol data:
#' data(MyHiers)
#' TaxonChildren(MyHiers)
#' TaxonParents(MyHiers[1])
#'   
#' # Species of Anolis off NCBI
#' data(MyHiers)
#' TaxonChildren(MyHiers)
#'
#' # Example to get all Anolis species from NCBI
#' eolAnolis <- DownloadSearchedTaxa("Anolis", to.file=FALSE)
#' hierAnolis <- DownloadHierarchy(eolAnolis, to.file=FALSE, database="NCBI Taxonomy")
#' TaxonChildren(hierAnolis)
#'
#' # Species of Anolis off The Reptile Database
#' eolAnolis <- DownloadSearchedTaxa("Anolis", to.file=FALSE)
#' repdbAnolis <- DownloadHierarchy(eolAnolis, to.file=FALSE, database="The Reptile Database")
#' TaxonChildren(repdbAnolis)
#' }

TaxonChildren <- function (MyHiers){
  MyHiers <- RemoveNAFiles(MyHiers)
  TC <- matrix(ncol=5, nrow=0)
  colnames(TC) <- c("Taxon", "TaxonChild", "TaxonRank", "eolID", "hierID")
  for(tax in sequence(length(MyHiers))){
    oneFile <- OneFileHierarchy(MyHiers[tax])
    Taxon <- oneFile[which(oneFile[,6] == GetHierID(MyHiers[tax])), 1]
    whichRows <- which(oneFile[,5] == GetHierID(MyHiers[tax]))
    if(length(whichRows) > 0){
      oneFile <- matrix(oneFile[whichRows,], nrow=length(whichRows))  #find which are to parent
      #if(any(is.na(oneFile[,2])))
      #  oneFile <- oneFile[-which(is.na(oneFile[,2])),] #remove any NAs
      if(dim(oneFile)[1] > 0){
        for(i in sequence(dim(oneFile)[1])) {
          TC <- rbind(TC, c(Taxon, oneFile[i,c(1,2,4,6)]))
        }
      }
    }
  }
  TC <- data.frame(TC, stringsAsFactors=FALSE)
  return(TC)
}

#' @rdname TaxonChildren
#' @export
TaxonParents <- function(MyHier) {
  hierID <- GetHierID(MyHier)
  parents <- subsetDataForHierTrees(OneFileHierarchy(MyHier), hierID)
  parents <- data.frame(paste(parents[,2], parents[,1]))
  names(parents) <- ""
  return(parents)
}
