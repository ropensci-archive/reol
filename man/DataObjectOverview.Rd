\name{DataObjectOverview}
\alias{GatherDataObjectInformation}
\alias{CombineDataObjectInformation}
\alias{DataObjectOverview}
\title{Data Object Information}
\description{
	These functions gather information about the number of kinds of data
	objects. For example, it will tell you that there are 3 gifs 12 jpgs, and 34
	text objects.        
}
\usage{
GatherDataObjectInformation(MyEOL)
CombineDataObjectInformation(MyEOLs, verbose=TRUE)
DataObjectOverview(MyEOLs, verbose=TRUE)
}
\arguments{
	\item{MyEOL}{A single EOL file or R object}
	\item{MyEOLs}{A vector of filenames or a list of XMLs for downloaded EOL pages}
	\item{verbose}{An optional print statement while combining}	
}
\value{
	 \code{GatherDataObjectInformation} will return information from one single EOL file all of the data objects and their associated accession info.  \code{CombineDataObjectInformation} will combine the data gathered singly into a huge matrix.  Printing the results from either of these functions may hang up R or cause memory issues, so it isnt recommended (though you can pull of pieces of it).  \code{DataObjectOverview} returns a data frame where each row is a single taxon and columns are a type of data object. 
}
\seealso{
	\code{\link{GetRichnessScores}}
	\code{\link{GetCommonNames}}
	\code{\link{GetIUCNStat}}
	\code{\link{GetReferences}}
}
\examples{
data(MyEOLs)
DataObjectOverview(MyEOLs[1], verbose=FALSE)
\dontrun{DataObjectOverview(MyEOLs, verbose=FALSE)}

#Data from the first data object from the first EOL page on the list
\dontrun{GatherDataObjectInformation(MyEOLs[1])[1,]}

#See searchable variables for data objects using names and view contents
\dontrun{names(GatherDataObjectInformation(MyEOLs[1]))}
\dontrun{GatherDataObjectInformation(MyEOLs[1])$source}

#One way to get all the english text back from a single EOL file
\dontrun{
  DOI <- GatherDataObjectInformation(MyEOLs[1])
  text <- which(which(DOI$mimeType == "text/html") %in% which(DOI$language ==
    "en"))
  DOI$description[text]
}
}