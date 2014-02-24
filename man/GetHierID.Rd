\name{GetHierID}
\alias{GetHierID}
\title{Gets Hierarchy page ID}
\description{
	This function uses the name of the file to return numerical ID.    
}
\usage{
GetHierID(MyHier)
}
\arguments{
	\item{MyHier}{A single filename for downloaded hierarchy page}
}
\value{
	Returns a hierarchical concept ID. 
}
\examples{
	#Works with a single page
	GetHierID("hier51323249.xml")

	#Or works with a list or vector of names
	data(MyHiers)
	GetHierID(MyHiers)
}
