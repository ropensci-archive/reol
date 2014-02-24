\name{GetRichnessScores}
\alias{GetRichnessScores}
\title{Gather EOL Richness Score Information}
\description{
EOL hosts richness scores for each species ranging from 0-100.  These are calculated on how much information data is available for that species, based on the amount of text on a page, how many multimedia files are present, how many different topics are covered, how many sources contribute, and whether the information has been reviewed.  
}
\usage{
GetRichnessScores(MyEOLs)
}
\arguments{
	\item{MyEOLs}{A vector of filenames for downloaded EOL pages OR a list of XML data stored as an R object}
}
\value{
	Returns a data frame with taxon, eol ID and numerical richness score. 
}
\seealso{
	\code{\link{GetCommonNames}}
	\code{\link{GetIUCNStat}}
	\code{\link{GetReferences}}
	\code{\link{DataObjectOverview}}
}
\examples{
data(MyEOLs)
GetRichnessScores(MyEOLs[1:2])
}
