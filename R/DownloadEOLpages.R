#' Download EOL Pages
#' 
#' @export
#' 
#' @param pages EOL page numbers to download
#' @param to.file Whether to download data to a file
#' @param MyKey An optional user identification key to identify yourself to EOL
#' @param verbose An optional print statement during download
#' 
#' @details \code{DownloadEOLpages} will download EOL pages based on the EOL unique identifyer 
#' number (EOL ID). Each taxon is associated with a unique identifier.  These numbers are used to 
#' match EOL pages with hierarchy pages and keep track of taxonomic changes.
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
#' }

DownloadEOLpages <- function(pages, to.file=TRUE, MyKey=NULL, verbose=TRUE, ...) {
  if(Sys.getlocale("LC_ALL") == "C")
    warning("Sys.getlocale is set to C. In order to read UTF characters, you need to set the 
            locale aspect to UTF-8 using Sys.setlocale")
  EOLpages <- vector("list", length=length(pages))
  for (i in sequence(length(pages))) {
    pageNum <- pages[i]
    web <- paste("http://eol.org/api/pages/", pageNum, ".xml?images=75&amp;videos=75&amp;sounds=75&amp;maps=75&amp;text=75&amp;iucn=true&amp;subjects=all&amp;details=true&amp;common_names=true&amp;references=true", sep="")	
	if(!is.null(MyKey))
      web <- paste(web, "&amp;key=", MyKey, sep="")
    if(to.file) {
      write(getURL(web, ...), file=paste("eol", pages[i], ".xml", sep=""))
      if(verbose)
        print(paste("Downloaded ", "eol", pages[i], ".xml", sep=""))
    }
    else {
      EOLpages[[i]] <- getURL(web, ...)
      names(EOLpages)[[i]] <- paste("eol", pages[i], sep="")
      if(verbose)
        print(paste("eol", pages[i], " saved as R object", sep=""))
    }  
    Sys.sleep(1)
  }
  if(to.file)
    return(paste("eol", pages, ".xml", sep=""))
  else
    return(EOLpages)
}