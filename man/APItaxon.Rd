\name{APItaxon}
\alias{APItaxon}
\title{Encode Taxon Name}
\description{
	This function will take a taxon name and encode spaces, and underscores to + for the EOL API.
}
\usage{
	APItaxon(taxon)
}
\arguments{
	\item{taxon}{Single taxonomic name}
}
\value{
	Returns an encoded taxon name.
}
\examples{
	APItaxon("Ursus americanus")
}
