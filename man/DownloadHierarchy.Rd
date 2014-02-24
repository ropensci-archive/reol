\name{DownloadHierarchy}
\alias{DownloadHierarchy}
\alias{ProviderCount}
\alias{BestProvider}
\title{Download Hierarchy Content From Contributors}
\description{
	This function will use the EOL pages to find the provider information (ie which providers contribute information to the pages).  Hierarchies get downloaded as hier pages rather than eol pages. Providers contribute information on taxonomic rankings, hierarchical tree structure, synonyms, etc.  Some providers only contribute certain kinds of information; for example not all will provide synonyms or taxonomic hierarchy information.  
}
\usage{
ProviderCount(MyEOLs,verbose=FALSE)
BestProvider(MyEOLs)
DownloadHierarchy(MyEOLs, to.file=TRUE, database=NULL, verbose=TRUE)
}
\arguments{
	\item{MyEOLs}{A vector of filenames for downloaded EOL pages}
	\item{to.file}{Whether to download data to a file}
	\item{database}{Provider hierarchy information to use}
	\item{verbose}{An optional print statement during download}	
}

\value{
	\code{ProviderCount} will give a named vector back of all of the providers that contribute to the set of MyEOLs taxa, and the number of taxa they contribute.  The maximum number of species a provider can contribute will be the number of taxa in MyEOLs.  Usually, ITIS, GBIF, and NCBI are top contributors (but this varies depending on scale). \code{BestProvider} will choose the top provider from this list (even if there are ties).  \code{DownloadHierarchy} will download the XML information either to a file (to.file=TRUE) or as a single R object as a list (to.file=FALSE).
}
\seealso{
	\code{\link{DownloadEOLpages}}
	\code{\link{DownloadSearchedTaxa}}
}
\examples{
data(MyEOLs)
\dontrun{ProviderCount(MyEOLs[6], verbose=TRUE)
BestProvider(MyEOLs[6])}
DownloadHierarchy(MyEOLs[6], FALSE, database="NCBI Taxonomy")
	
#Download data from whichever provider has the most coverage
\dontrun{MyHiers <- DownloadHierarchy(MyEOLs, to.file=TRUE, BestProvider(MyEOLs))}
	
#Or download from a specific provider
\dontrun{MyHiers <- DownloadHierarchy(MyEOLs, FALSE, database="NCBI Taxonomy")}


}
