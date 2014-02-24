\name{CombineHierarchyInfo}
\alias{CombineHierarchyInfo}
\title{Combine Hierarchy Info}
\description{
	This function combines different hierarchy dataframes into one. Mostly an
	internal function for tree and edge label building. 
}
\usage{
CombineHierarchyInfo(MyHiers)
}
\arguments{
	\item{MyHiers}{A vector of hier pages to combine}
}
\value{
	Returns a combined data frame. 
}
\examples{
	data(MyHiers)
	CombineHierarchyInfo(MyHiers)
}
