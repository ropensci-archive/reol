#' Download Hierarchy Content From Contributors
#'
#' This function will use the EOL pages to find the provider information (ie which providers
#' contribute information to the pages).  Hierarchies get downloaded as hier pages rather than
#' eol pages. Providers contribute information on taxonomic rankings, hierarchical tree structure,
#' synonyms, etc.  Some providers only contribute certain kinds of information; for example not all
#' will provide synonyms or taxonomic hierarchy information.
#'
#' @export
#'
#' @param MyEOLs A vector of filenames for downloaded EOL pages
#' @param to.file Whether to download data to a file
#' @param database Provider hierarchy information to use
#' @param verbose An optional print statement during download
#'
#' @return \code{ProviderCount} will give a named vector back of all of the providers that
#' contribute to the set of MyEOLs taxa, and the number of taxa they contribute.  The maximum
#' number of species a provider can contribute will be the number of taxa in MyEOLs.  Usually,
#' ITIS, GBIF, and NCBI are top contributors (but this varies depending on scale).
#' \code{BestProvider} will choose the top provider from this list (even if there are ties).
#' \code{DownloadHierarchy} will download the XML information either to a file (to.file=TRUE) or
#' as a single R object as a list (to.file=FALSE).
#'
#' @seealso \code{\link{DownloadEOLpages}} \code{\link{DownloadSearchedTaxa}}
#'
#' @examples \dontrun{
#' data(MyEOLs)
#' ProviderCount(MyEOLs[6], verbose=TRUE)
#' BestProvider(MyEOLs[6])
#' DownloadHierarchy(MyEOLs[6], FALSE, database="NCBI Taxonomy")
#'
#' # Download data from whichever provider has the most coverage
#' MyHiers <- DownloadHierarchy(MyEOLs, to.file=TRUE, BestProvider(MyEOLs))
#'
#' # Or download from a specific provider
#' MyHiers <- DownloadHierarchy(MyEOLs, FALSE, database="NCBI Taxonomy")
#' }

DownloadHierarchy <- function(MyEOLs, to.file=TRUE, database=NULL, verbose=TRUE, ...) {
#MyEOLs can be a file or an R object
#to.file is whether you want to save the information as a file (T) or an R object (F)
  #Downloads provider database
  if(is.null(database))
    database <- BestProvider(MyEOLs)
  results <- GatherProviderDataFrame(MyEOLs, extended.output=TRUE)
  column <- which(colnames(results) == paste(database, ".taxonID", sep=""))
  pages <- results[,column]
  hierpages <- vector("list", length=length(pages))
  for (i in sequence(length(pages))) {
    if (!is.na(pages[i])) {
      pageNum<-pages[i]
      web <- paste("http://eol.org/api/hierarchy_entries/1.0/", pageNum, sep="")
      if(to.file) {
        write(getURL(web, ...), file=paste("hier", pages[i], ".xml", sep=""))
        if(verbose)
          print(paste("Downloaded ", "hier", pages[i], ".xml", sep=""))
      }
      else {
        hierpages[[i]] <- getURL(web, ...)
        names(hierpages)[[i]] <- paste("hier", pages[i], sep="")
        if(verbose)
          print(paste("hier", pages[i], " saved as R object", sep=""))
      }
      Sys.sleep(1)
    }
  }
  if(to.file)
    return(paste("hier", pages, ".xml", sep=""))
  else
    return(hierpages)
}

#' @rdname DownloadHierarchy
#' @export
ProviderCount <- function(MyEOLs, verbose=FALSE) {
  #Returns vector of provider coverage
  results <- GatherProviderDataFrame(MyEOLs, extended.output=FALSE)[,-1:-2]
  results <- results[, -dim(results)[2]]
  counts <- apply(results, 2, sum)
  names(counts) <- colnames(results)
  counts <- sort(counts, decreasing=TRUE)
  if (verbose)
    print(t(t(counts)))
  return(counts)
}

#' @rdname DownloadHierarchy
#' @export
BestProvider <- function(MyEOLs) {
  #Returns the provider with the most taxonomic coverage
  return(names(ProviderCount(MyEOLs))[1])
}