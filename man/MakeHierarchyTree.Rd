\name{MakeHierarchyTree}
\alias{MakeTreeData}
\alias{AutofillTaxonNames}
\alias{MakeHierarchyTree}
\alias{ReturnTaxSet}
\alias{NodeLabelList}
\alias{MergeTaxonomies}
\title{Creates Hierarchical Trees}
\description{
	These functions will create a taxonomic tree (dendrogram) based on ranking from EOLs provider (hierarchy) pages. 
}
\usage{
MakeTreeData(MyHiers)
AutofillTaxonNames(TreeData)
MakeHierarchyTree(MyHiers, missingData=NULL, includeNodeLabels=TRUE, userRanks=NULL)
ReturnTaxSet(Taxon, TreeData)
NodeLabelList(MyHiers, label="all", missingData)
MergeTaxonomies(i, j)
}
\arguments{
	\item{MyHiers}{A vector of filenames or a list of XMLs for downloaded EOL pages}
	\item{TreeData}{A dataframe of taxonomic hierarchy information out of MakeTreeData function}
	\item{missingData}{If tip taxa are not all the same taxonomic rank, should Reol cleave out taxa or hierarchical rank first}
	\item{includeNodeLabels}{Option to write node labels to phylogenetic tree (Note, this can also be done separately using \code{NodeLabelList}}
	\item{userRanks}{Option for the user to define their own hierarchical pattern to make a tree.  This will define which ranked classifications to include in the final tree.  If left NULL, it will try to keep as much information as possible.}
	\item{Taxon}{Taxonomic group that contains subunits}
	\item{label}{Which hierarchical units should be included in the node labels}
	\item{i}{A Hierarchical Taxonomy (ex: Kingdom, Class, Species)}
	\item{j}{A Hierarchical Taxonomy (ex: Class, Genus, Species)}
}
\value{
\code{MakeTreeData} returns a data frame with data for use in \code{MakeHierarchyTree} function, but can also be used independently to examine the hierarchical structure. \code{AutofillTaxonNames} is an internal function that deals with missing internal data in TreeData. If taxa are of varying hierarchical ranks (for example a mix of genera and species) then tips can not be aligned in the tree.  You will need to select what data you would like to drop from the analysis; for example, either taxa with missing species information OR all species names that will then make a tree of genera. \code{MakeHierarchyTree} returns taxonomic tree in the class phylo. \code{ReturnTaxSet} will return the tree tips for the taxonomic group requested, this is mostly an internal function for creating node labels. \code{MakeNodeLabels} will return a list of tip labels per internal node.  This can be used to create node labels to plot on the tree. \code{MergeTaxonomies} will merge two taxonomies into a single list that preserves hierarchical structure. It is mostly for internal use.  
}
\details{
	This tree displays taxonomic structuring only and is not the result of a phylogenetic analysis. Alos note that not all providers return hierarchy information, if errors or no tree is returned it is likely that information is missing and you may have to use another provider.
}
\seealso{
\code{\link{ProviderCount}}
\code{\link{DownloadHierarchy}}
\code{\link{MakeEdgeLabels}}
}
\examples{
data(MyHiers)
TreeData <- MakeTreeData(MyHiers)
Tree <- MakeHierarchyTree(MyHiers, includeNodeLabels=TRUE)
labels <- NodeLabelList(MyHiers, "all")
plot(Tree, show.node.label=TRUE)

plot(Tree, "c", show.node.label=TRUE, adj=0.5, font=3, edge.color="gray",
	tip.color=rainbow(10))

}