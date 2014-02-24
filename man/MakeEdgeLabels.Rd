\name{MakeEdgeLabels}
\alias{MakeEdgeLabels}
\alias{node.leaves}
\alias{node.offspring}
\alias{getTipList}
\alias{WhatToDoWithDuplicateEdgeNames}
\alias{whichEdge}
\title{Creates Edge Labels for Hierarchy Trees}
\description{
	These functions will create edge labels for hierarchy trees. 
}
\usage{
getTipList(phy)
whichEdge(phy, taxa)
node.leaves(phy, node)
node.offspring(phy, node)
WhatToDoWithDuplicateEdgeNames(edgeLabels, duplicateEdgeLabels)
MakeEdgeLabels(MyHiers, label="all", missingData=NULL, duplicateEdgeLabels="oldest")
}
\arguments{
	\item{phy}{A tree in the class phylo}	
	\item{node}{Any node number in the tree}
	\item{MyHiers}{A vector of hier pages OR a list of XMLs as an R object}
	\item{taxa}{Vector of tip taxa}
	\item{edgeLabels}{Vector of edge labels with the same associated branch}
	\item{duplicateEdgeLabels}{Choice of which edge label to prefer: recent, oldest, or combined}
	\item{missingData}{If tip taxa are not all the same taxonomic rank, should Reol cleave out taxa or hierarchical rank first}
	\item{label}{Which hierarchical units should be included in the edge labels}
}
\value{
	\code{MakeEdgeLabels} returns a vector of edges and their clade names to be used in apes edgeLabels function.  \code{getTipList}, \code{WhatToDoWithDuplicateEdgeNames}, and \code{whichEdge} are internal functions for \code{MakeEdgeLabel}. 
}
\details{
	Note that edges are slightly different than node labels, in that edges are plotted along the center of the branch rather than at a node.  Plotting both is redundant, but one or the other may look better aesthetically. 
	Also edges in the edgeLabels function is not the actual edge number, but the row.  Our functions reflect this.    
	There will likely be cases where edges have more than one hierarchical name (for example, the branch to camel species will have the genus Camelus and the family Camelidae). When making edge labels, you have the choice of using the most recent hierarchical name (genus in the camel example) or the oldest (family in camels), or you can choose to combine the names so that you can see all of them (Camilidae.Camelus).  
}
\seealso{
\code{\link{MakeHierarchyTree}}
}
\examples{
data(MyHiers)
Tree <- MakeHierarchyTree(MyHiers, includeNodeLabels=FALSE)
edges <- MakeEdgeLabels(MyHiers)	
plot(Tree, show.node.label=FALSE)
edgelabels(text=names(edges), edge=edges)  


\dontrun{
edges <- MakeEdgeLabels(MyHiers, missingData="pruneTaxa", duplicateEdgeLabels="recent")
plot(Tree, show.node.label=FALSE)
edgelabels(text=names(edges), edge=edges)  
}
}
