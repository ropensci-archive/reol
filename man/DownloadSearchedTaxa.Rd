\name{DownloadSearchedTaxa}
\alias{DownloadEOLpages}
\alias{DownloadSearchedTaxa}
\title{Download Page Content From EOL}
\description{
	These functions will take a string of EOL IDs or taxonomic names and search EOL database for the pages. If it finds a match, it will download the EOL page.  
}
\usage{
DownloadEOLpages(pages, to.file=TRUE, MyKey=NULL, verbose=TRUE)
DownloadSearchedTaxa(ListOfTaxa, to.file=TRUE, MyKey=NULL, exact=TRUE, verbose=TRUE)
}
\arguments{
	\item{pages}{EOL page numbers to download}
	\item{to.file}{Whether to download data to a file}
	\item{MyKey}{An optional user identification key to identify yourself to EOL}
	\item{verbose}{An optional print statement during download}	
	\item{ListOfTaxa}{List of EOL taxa to search and download EOL pages}
	\item{exact}{Should taxon name match exactly in EOL or fuzzy match}
}
\details{
	\code{DownloadEOLpages} will download EOL pages based on the EOL unique identifyer number (EOL ID). Each taxon is associated with a unique identifier.  These numbers are used to match EOL pages with hierarchy pages and keep track of taxonomic changes. If you are unsure of these numbers you can use \code{DownloadSearchedTaxa}, which will search for either an exact taxonomic match (exact=TRUE) or use fuzzy name matching to catch spelling errors (exact=FALSE).  This will automatically recover the matching EOL ID and download or save the XML data accordingly. 
	
	To generate an api key (MyKey), register with EOL and find it under your profile.  
}
\value{
	Either an XML file(s) downloaded to working directory or as an R object saved in the workspace.
}
\seealso{
	\code{\link{DownloadHierarchy}}
}
\examples{
#Download taxa files to working directory in R
\dontrun{DownloadEOLpages(c(1,2,3), to.file=TRUE, MyKey)}
\dontrun{DownloadSearchedTaxa(c("Anolis_carolinensis", 
	"Anolis garmani"), to.file=TRUE, exact=TRUE)}

#Save data as an R object rather than download files

MyTaxa <- c("Camelus dromedarius")
MyEOLs <- DownloadSearchedTaxa(MyTaxa, to.file=FALSE)
#save(MyEOLs, file="MyEOLs.rda")
}
