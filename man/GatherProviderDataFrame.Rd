\name{GatherProviderDataFrame}
\alias{GatherProviderDataFrame}
\title{Gather EOL Provider Information}
\description{
	This function will read in the downloaded xml files and amass a data frame with provider information. Information will include taxon name, eol number, and information from each provider in the following order: provider taxon, provider taxon ID, provider scientific name, provider taxonomy rank.
}
\usage{
GatherProviderDataFrame(MyEOLs, extended.output=FALSE)
}
\arguments{
	\item{MyEOLs}{A vector of filenames or a list of XMLs for downloaded EOL pages}
	\item{extended.output}{If set to TRUE, then a large dataframe will be returned with information from each provider in the following order: provider taxon, provider taxon ID, provider scientific name, provider taxonomy rank.}
}
\value{
	Returns a data frame with presence/absence of provider information or if extended.output=TRUE ID values for each provider.  
}
\examples{
data(MyEOLs)
\dontrun{GatherProviderDataFrame(MyEOLs, extended.output=FALSE)}

GatherProviderDataFrame(MyEOLs[1], extended.output=FALSE)
}
