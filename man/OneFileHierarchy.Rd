\name{OneFileHierarchy}
\alias{OneFileHierarchy}
\title{Creates A Taxonomic Hierarchy for a single Taxon}
\description{
	This function uses a provider hierarchy file to gather information about all taxonomic levels and their associated IDs.    
}
\usage{
OneFileHierarchy(MyHier)
}
\arguments{
	\item{MyHier}{A single of filename for downloaded hierarchy page}
}
\value{
	Returns a data frame where each row is a hierarchical taxonomic unit. 
}
\examples{
	data(MyHiers)
	OneFileHierarchy(MyHiers[1])
}
