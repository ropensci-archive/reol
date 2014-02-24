\name{CollectDataforWeb}
\alias{CollectDataforWeb}
\alias{DataProcessing}
\alias{CNCount}
\alias{DOCount}
\alias{providerCount}

\title{Collect Data from EOL Pages for Website}
\description{
	    These functions will read and scrape content off the downloaded EOL
	    pages. This is for building the data table on eoldata.org.
}
\usage{
CollectDataforWeb(MyEOL, do.higher.taxonomy)
CNCount(res)
DOCount(res)
providerCount(res)
DataProcessing(res, do.higher.taxonomy)
}
\arguments{
	\item{MyEOL}{A single filename for downloaded EOL pages}
	\item{res}{XML object}
	\item{do.higher.taxonomy}{TRUE/FALSE on whether to download higher level taxonomy}
}
\value{
	Appends EOL data to a table. 
}
\examples{
	data(MyEOLs)
	CollectDataforWeb(MyEOLs[1])
}
