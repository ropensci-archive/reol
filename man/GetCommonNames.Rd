\name{GetCommonNames}
\alias{GetCommonNames}
\title{Gather Common Name Information}
\description{
	This function gathers each species common names and the language with which
	they are associated. 
}
\usage{
GetCommonNames(MyEOLs, output = c("detail", "counts"))
}
\arguments{
	\item{MyEOLs}{A vector of filenames or a list of XMLs for downloaded EOL pages}
	\item{output}{Detail will return a data frame with common names and their language; counts will return a dataframe with each taxon and language counts for common names.}
}
\value{
	Returns a data frame with taxon, eol ID, common name, and language. 
}
\seealso{
	\code{\link{GetRichnessScores}}
	\code{\link{GetIUCNStat}}
	\code{\link{GetReferences}}
	\code{\link{DataObjectOverview}}
}
\examples{
data(MyEOLs)

GetCommonNames(MyEOLs[1], "d")

\dontrun{GetCommonNames(MyEOLs, output="detail")}
\dontrun{GetCommonNames(MyEOLs, "c")}
}
