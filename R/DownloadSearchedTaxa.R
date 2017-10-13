#' Download Page Content From EOL
#'
#' These functions will take a string of EOL IDs or taxonomic names and search EOL database for the
#' pages. If it finds a match, it will download the EOL page.
#'
#' @import RCurl XML ape
#' @export
#'
#' @param to.file Whether to download data to a file
#' @param MyKey An optional user identification key to identify yourself to EOL
#' @param verbose An optional print statement during download
#' @param ListOfTaxa List of EOL taxa to search and download EOL pages
#' @param exact Should taxon name match exactly in EOL or fuzzy match
#' @param taxon Taxon
#' @param ... Curl options passed on to \code{\link[RCurl]{getURL}}
#'
#' @details \code{DownloadEOLpages} will download EOL pages based on the EOL unique identifyer
#' number (EOL ID). Each taxon is associated with a unique identifier.  These numbers are used to
#' match EOL pages with hierarchy pages and keep track of taxonomic changes. If you are unsure of
#' these numbers you can use \code{DownloadSearchedTaxa}, which will search for either an exact
#' taxonomic match (exact=TRUE) or use fuzzy name matching to catch spelling errors (exact=FALSE).
#' This will automatically recover the matching EOL ID and download or save the XML data
#' accordingly.
#'
#' To generate an api key (MyKey), register with EOL and find it under your profile.
#'
#' @return Either an XML file(s) downloaded to working directory or as an R object saved in the
#' workspace.
#'
#' @seealso \code{\link{DownloadHierarchy}}
#'
#' @examples \dontrun{
#' # Download taxa files to working directory in R
#' DownloadEOLpages(c(1,2,3), to.file=TRUE, MyKey)
#' DownloadSearchedTaxa(c("Anolis_carolinensis", "Anolis garmani"), to.file=TRUE, exact=TRUE)
#'
#' # Save data as an R object rather than download files
#' MyTaxa <- c("Camelus dromedarius")
#' MyEOLs <- DownloadSearchedTaxa(MyTaxa, to.file=FALSE)
#' #save(MyEOLs, file="MyEOLs.rda")
#' }

DownloadSearchedTaxa <- function(ListOfTaxa, to.file=TRUE, MyKey=NULL, exact=TRUE, verbose=TRUE, ...) {
  matches <- MatchTaxatoEOLID(ListOfTaxa, exact=exact, ...)
  pagesToDownload <- unique(matches[,3])
  fileNames <- data.frame(paste("eol", matches[,3], ".xml", sep=""), stringsAsFactors=FALSE)
  colnames(fileNames) <- "fileNames"
  if(any(!is.na(pagesToDownload))) {
    DownloadEOLpages(as.numeric(pagesToDownload), to.file, MyKey, verbose=verbose, ...)
  }
}

#' @rdname DownloadSearchedTaxa
#' @export
APItaxon <- function(taxon) {
  taxon <- gsub("_", "+", taxon)
  taxon <- gsub(" ", "+", taxon)  #doesn't work
  return(taxon)
}

#' @rdname DownloadSearchedTaxa
#' @export
MatchTaxatoEOLID <- function(ListOfTaxa, exact=TRUE, ...){
  #Match a search taxon to an EOLID for downloading or storing
  #API can support fuzzy matching up to 30 matches if exact=F, but then pages have to be specified.  Might be a good thing to add later.
  eolPageNumbers <- rep(NA, length(ListOfTaxa))
  speciesNameForRef <- rep(NA, length(ListOfTaxa))
  for (i in sequence(length(ListOfTaxa))) {
    taxon <- APItaxon(ListOfTaxa[i])
	web <- paste('http://eol.org/api/search/1.0.xml?q=', taxon, '&exact=', exact, '&page=1', sep="")
    a <- getURL(web)
    searchRes <- NULL
    searchRes <- xmlToList(xmlRoot(xmlParse(a, getDTD=FALSE), ...), simplify=FALSE)
    if (searchRes$totalResults == 1) {  # matched a single entry
      eolPageNumbers[i] <- searchRes$entry$id
      speciesNameForRef[i] <- searchRes$entry$title
    } else {
      # sometimes results contain multiple entries
      # check if these all correspond to the same EOLID
      ids <- sapply(searchRes[names(searchRes) == "entry"], function(x) x$id)
      if (length(rle(ids)$values) == 1) { # IDs match
        eolPageNumbers[i] <- searchRes$entry$id
        speciesNameForRef[i] <- searchRes$entry$title
      }
    }
  }
  return(data.frame(ListOfTaxa, speciesNameForRef, eolPageNumbers, stringsAsFactors=F))
}
