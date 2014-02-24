\name{GatherSynonyms}
\alias{GatherSynonyms}
\title{Gathers A List Of Taxonomic Synonyms}
\description{
	This function goes through the hierarchy pages to collect taxonomic synonyms.    
}
\usage{
GatherSynonyms(MyHiers, output = c("detail", "counts"))
}
\arguments{
	\item{MyHiers}{A vector of filenames for downloaded hierarchy pages}
	\item{output}{Detail will return a data frame with each taxon and their taxonomic 
	synonyms; counts will return a dataframe with each species and synonym counts.}
}
\value{
	When output is set to detail, it returns a data frame where each row is the 
	taxonomic unit that has a synonym, the hierarchy ID, and the synonym.  
	When output="counts", each taxon is included whether it has a synonym or not.  
	Rather than report the synonyms, it reports the number of synonyms for each.
}
\examples{
data(MyHiers)
GatherSynonyms(MyHiers, output="detail")
GatherSynonyms(MyHiers, "c")
}
