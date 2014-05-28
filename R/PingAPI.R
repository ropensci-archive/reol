#' Test the working order of the EOL API
#' 
#' This function will send out a ping to the EOL API to make sure it is in working order. The EOL 
#' server will return either a message of success or failure.
#' 
#' @export
#' 
#' @param MyKey An optional user identification key to identify yourself to EOL
#' 
#' @return Returns a message with success or failure
#' 
#' @examples \dontrun{
#' PingAPI()
#' #PingAPI(MyKey)
#' }

PingAPI <- function(MyKey=NULL) {
  web <- "http://eol.org/api/ping.xml"
  if(!is.null(MyKey))
    web <- paste(web, "?key=", MyKey, sep="")
  a <- getURL(web)
  xmlToList(xmlRoot(xmlParse(a, getDTD=FALSE)))$message
}
