\name{subsetDataForHierTrees}
\alias{subsetDataForHierTrees}
\title{Subset Data For Hier Trees}
\description{
	This internal function removes repeat taxonomic units, undefined clades, and NAs from hierarchy information. 
}
\usage{
subsetDataForHierTrees(oneFileHier, HierID)
}
\arguments{
	\item{oneFileHier}{A dataframe of taxonomic hierarchy information out of OneFileHierarchy function}
	\item{HierID}{Hierarchy ID value of interest}
}
\value{
	Returns a subset data frame with only parent nodes from HierID up for building taxonomic trees. 
}
