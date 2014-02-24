\name{PingAPI}
\alias{PingAPI}
\title{Test the working order of the EOL API}
\description{
	This function will send out a ping to the EOL API to make sure it is in working order. The EOL server will return either a message of success or failure.
}
\usage{
	PingAPI(MyKey=NULL)
}
\arguments{
	\item{MyKey}{An optional user identification key to identify yourself to EOL}
}
\value{
	Returns a message with success or failure.
}
\examples{
	PingAPI()
	#PingAPI(MyKey)
}
