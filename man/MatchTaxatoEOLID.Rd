\name{MatchTaxatoEOLID}
\alias{MatchTaxatoEOLID}
\title{Matches Taxa to and eolID}
\description{
	This function will take a sting of taxa and search EOL database for the EOL ID. These IDs can then be used with the DownloadEOLpages function.    
}
\usage{
MatchTaxatoEOLID(ListOfTaxa, exact=TRUE)
}
\arguments{
	\item{ListOfTaxa}{List of EOL taxa to search and download EOL pages}
	\item{exact}{Should taxon name match exactly in EOL or fuzzy match}
}
\value{
	Returns a table with the submitted taxon name, returned taxon name, and eol ID number.
}
\seealso{
	\code{\link{DownloadEOLpages}}
	\code{\link{DownloadSearchedTaxa}}
}
\examples{
	bears<-c("Ursus americanus", "Ursus thibetanus", "Ursus maritimus")
	MatchTaxatoEOLID(bears, exact=TRUE)
}
