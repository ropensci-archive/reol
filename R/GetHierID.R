#' Gets Hierarchy page ID
#' 
#' This function uses the name of the file to return numerical ID.
#' 
#' @export
#' 
#' @param MyHier A single filename for downloaded hierarchy page
#' @return Returns a hierarchical concept ID.
#' @examples \dontrun{
#' # Works with a single page
#' GetHierID("hier51323249.xml")
#'
#' # Or works with a list or vector of names
#' data(MyHiers)
#' GetHierID(MyHiers)
#' }

GetHierID <- function(MyHier){
  if(class(MyHier) == "list")
    conceptID <- gsub("^\\D+|\\D+$", "", names(MyHier))
  if(class(MyHier) == "character")
    conceptID <- gsub("^\\D+|\\D+$", "", MyHier)
  if(any(conceptID == "") || any(is.na(conceptID)))
    conceptID[which(conceptID == "")] <- NA
  return(conceptID)
}
