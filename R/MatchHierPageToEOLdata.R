#' Match Data Functions
#'
#' These functions match hierarchy concept IDs with EOL page data and/or taxonomic tree tips.
#'
#' @export
#'
#' @param MyHiers A vector of filenames for downloaded hierarchy pages or a list of XML data 
#' stored as an R object
#' @param EOLdata Any output dataframe from Reol functions
#' @param Tree Taxonomic tree in the class phylo
#' @param Data Data output from one of the EOL or Hierarchy gathering functions`
#'
#' @return \code{MatchHierPageToEOLdata} returns a data frame where each row is a taxonomic unit.
#' This is especially helpful if you wish to plot EOL data on a taxonomic tree (see example),
#' because it matches the hierarchy taxon names with the EOL data (since taxon names will not
#' always match).  \code{MatchDataToTreeTips} will match the tip labels on a tree with those from
#' a data frame.  It orders the data in the same way as is in the trees, so that you can plot data
#' on a tree without distortion (either in number of data points, or in taxonomic name).
#' 
#' @examples \dontrun{
#' # plotting richness scores
#' data(MyEOLs)
#' data(MyHiers)
#' Tree <- MakeHierarchyTree(MyHiers, includeNodeLabels=FALSE)
#' MyData <- MatchHierPageToEOLdata(MyHiers, GetRichnessScores(MyEOLs))
#' plot(Tree, label.offset=1, x.lim=10)
#' tiplabels(round(as.numeric(MyData[,3])), 1:6, col="Blue", frame="none",
#'         bg="clear",adj = -0.5)
#' title(main="Richness scores")
#' }


MatchHierPageToEOLdata <- function(MyHiers, EOLdata){
  MyHiers <- RemoveNAFiles(MyHiers)
  if(class(MyHiers) == "list")
    fileNames <- names(MyHiers)
  else
    fileNames <- MyHiers
  matchedData <- matrix(nrow=length(MyHiers), ncol=3)
  matchedData[,2] <- as.character(sapply(fileNames, GetHierID))
  for(i in sequence(length(MyHiers))) {
    resOneFile<-OneFileHierarchy(MyHiers[i])
    matchedData[i,1] <- resOneFile[which(matchedData[i,2]==resOneFile[,6]), 1] #taxonName
    matchedData[i,3] <- resOneFile[which(matchedData[i,2]==resOneFile[,6]), 4] #HierID
  }
  matches <- match(matchedData[,3], EOLdata[,2])
  matchedData <- cbind(matchedData, EOLdata[matches, 3:dim(EOLdata)[2]])
  colnames(matchedData) <- c( "HierTaxon", "HierID", "eolID", colnames(EOLdata[3:dim(EOLdata)[2]]))
  matchedData <- data.frame(matchedData, row.names=1, stringsAsFactors=FALSE)
  return(matchedData)
}

#' @rdname MatchHierPageToEOLdata
#' @export
MatchDataToTreeTips <- function(Tree, Data){
  if(length(grep("HierID", colnames(Data), ignore.case=T)) == 0)
    stop("EOLdata must be matched to HierarchyID first, use MatchHierPageToEOLdata(MyHiers, EOLdata)")
  if(any(is.na(Data)))
    Data <- Data[-unique(which(is.na(Data), arr.ind=TRUE)[,1]),]
  if(length(grep("Taxon", colnames(Data), ignore.case=T)) > 0)  #if taxon names are a column, then make them rownames
    rownames(Data) <- Data[,grep("Taxon", colnames(Data), ignore.case=T)]
  NewOrderMatchedData <- data.frame(matrix(ncol=dim(Data)[2], nrow=length(Tree$tip.label)))
  colnames(NewOrderMatchedData) <- colnames(Data)
  for(i in sequence(length(Tree$tip.label))){
    if(any(rownames(Data) %in% Tree$tip.label[i])) {
      NewOrderMatchedData[i,] <- Data[which(rownames(Data) %in% Tree$tip.label[i]),]
      rownames(NewOrderMatchedData)[i] <- rownames(Data[which(rownames(Data) %in% Tree$tip.label[i]),])
    }
    else {
      NewOrderMatchedData[i,] <- rep(NA, dim(NewOrderMatchedData)[2])
      rownames(NewOrderMatchedData)[i] <- Tree$tip.label[i]
    }
  }
  if(all(rownames(NewOrderMatchedData) == Tree$tip.label))
    return(NewOrderMatchedData)
  else
    stop("something wonky in data")
}
