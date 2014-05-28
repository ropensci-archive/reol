#' Get Binomial Nomenclature 
#' 
#' This function strips a taxon name to the first two words.  Many times, and EOL species name will
#' come in with additional data (such as the describer, subspecies, variety, etc).  This will prune
#' it down to just two names.
#' 
#' @export
#' @param name Any taxonomic unit
#' @return Returns a name
#' 
#' @examples \dontrun{
#' FirstTwo("Galanthus cilicicus Baker")
#' FirstTwo("Galanthus peshmenii A.P.Davis & C.D.Brickell")
#' FirstTwo("Galanthus")
#' }

FirstTwo <- function(name) {
  if(is.null(name))
    name <- NA
  if(!is.na(name)) {
  	#trim2 <- function (x) gsub("^\\s+|\\s+$", "", x)
    name <- gsub("^\\s+|\\s+$", "", name)
    if(length(strsplit(name, " ")[[1]]) > 2)
      name <- paste(strsplit(name," ")[[1]][1:2],sep=" ",collapse=" ")
  }
  return(name)
}
