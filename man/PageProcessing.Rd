\name{PageProcessing}
\alias{PageProcessing}
\alias{RemoveNAFiles}
\title{Process XML Data into tree format}
\description{
	This function will read in the XML data and parse it into a tree structure for R to read. 
}
\usage{
PageProcessing(MyEOL)
RemoveNAFiles(MyFiles)
}
\arguments{
	\item{MyEOL}{A filename or R object for downloaded EOL pages}
	\item{MyFiles}{A vector of filenames or R objects for EOL  or Hier pages}
}
\value{
	\code{PageProcessing} returns XML tree as an R object. \code{RemoveNAFiles} is an internal function that will take a vector of filenames and check to make sure they all have data associated (sometimes requested pages will be empty and contain no information).  If they are empty, they are cleaved from the analysis.  
}
\examples{
	#Reads in a file
	#PageProcessing("eol1.xml") 
	
	#or reads in an R object
	data(MyEOLs)
	PageProcessing(MyEOLs[1])

}
