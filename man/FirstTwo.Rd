\name{FirstTwo}
\alias{FirstTwo}
\title{Get Binomial Nomenclature}
\description{
	This function strips a taxon name to the first two words.  Many times, and EOL species name will come in with additional data (such as the describer, subspecies, variety, etc).  This will prune it down to just two names.       
}
\usage{
FirstTwo(name)
}
\arguments{
	\item{name}{any taxonomic unit}
}
\value{
	Returns a name 
}
\examples{
	FirstTwo("Galanthus cilicicus Baker")
	FirstTwo("Galanthus peshmenii A.P.Davis & C.D.Brickell")
	FirstTwo("Galanthus")
}
