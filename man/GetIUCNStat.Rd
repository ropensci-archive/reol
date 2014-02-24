\name{GetIUCNStat}
\alias{GetIUCNStat}
\title{Gather IUCN Status for EOL Pages}
\description{
	This function gathers IUCN threat statuses from EOL pages if they are
	reported.  
}
\usage{
GetIUCNStat(MyEOLs)
}
\arguments{
	\item{MyEOLs}{A vector of filenames or a list of XMLs for downloaded EOL pages}
}
\value{
	Returns a data frame with taxon, eol ID, and IUCN status. 
}
\seealso{
	\code{\link{GetRichnessScores}}
	\code{\link{GetCommonNames}}
	\code{\link{GetReferences}}
	\code{\link{DataObjectOverview}}
}
\examples{
data(MyEOLs)
\dontrun{GetIUCNStat(MyEOLs)}
GetIUCNStat(MyEOLs[3])
}

