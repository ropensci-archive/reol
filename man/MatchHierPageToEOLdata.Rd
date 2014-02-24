\name{MatchHierPageToEOLdata}
\alias{MatchHierPageToEOLdata}
\alias{MatchDataToTreeTips}
\title{Match Data Functions}
\description{
	These functions match hierarchy concept IDs with EOL page data and/or taxonomic tree tips.    
}
\usage{
MatchHierPageToEOLdata(MyHiers, EOLdata)
MatchDataToTreeTips(Tree, Data)
}
\arguments{
	\item{MyHiers}{A vector of filenames for downloaded hierarchy pages or a list of XML data stored as an R object}
	\item{EOLdata}{Any output dataframe from Reol functions}
	\item{Tree}{Taxonomic tree in the class phylo}
	\item{Data}{Data output from one of the EOL or Hierarchy gathering functions}
}
\value{
	\code{MatchHierPageToEOLdata} returns a data frame where each row is a taxonomic unit. This is especially helpful if you wish to plot EOL data on a taxonomic tree (see example), because it matches the hierarchy taxon names with the EOL data (since taxon names will not always match).  \code{MatchDataToTreeTips} will match the tip labels on a tree with those from a data frame.  It orders the data in the same way as is in the trees, so that you can plot data on a tree without distortion (either in number of data points, or in taxonomic name).  
}
\examples{
	#plotting richness scores
	\dontrun{data(MyEOLs)
     data(MyHiers)
     Tree <- MakeHierarchyTree(MyHiers, includeNodeLabels=FALSE)  
     MyData <- MatchHierPageToEOLdata(MyHiers, GetRichnessScores(MyEOLs))
     plot(Tree, label.offset=1, x.lim=10)
     tiplabels(round(as.numeric(MyData[,3])), 1:6, col="Blue", frame="none",
		bg="clear",adj = -0.5)
     title(main="Richness scores")
	}
}
