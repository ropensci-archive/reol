#' Creates Edge Labels for Hierarchy Trees
#' 
#' These functions will create edge labels for hierarchy trees.
#' 
#' @export
#' 
#' @param phy tree in the class phylo
#' @param node Any node number in the tree
#' @param MyHiers A vector of hier pages OR a list of XMLs as an R object
#' @param taxa Vector of tip taxa
#' @param edgeLabels Vector of edge labels with the same associated branch
#' @param duplicateEdgeLabels Choice of which edge label to prefer: recent, oldest, or combined
#' @param missingData If tip taxa are not all the same taxonomic rank, should Reol cleave out taxa or hierarchical rank first
#' @param label Which hierarchical units should be included in the edge labels
#'
#' @return \code{MakeEdgeLabels} returns a vector of edges and their clade names to be used in apes
#' edgeLabels function.  \code{getTipList}, \code{WhatToDoWithDuplicateEdgeNames}, and
#' \code{whichEdge} are internal functions for \code{MakeEdgeLabel}.
#'
#' @details Note that edges are slightly different than node labels, in that edges are plotted
#' along the center of the branch rather than at a node.  Plotting both is redundant, but one or
#' the other may look better aesthetically.
#' Also edges in the edgeLabels function is not the actual edge number, but the row.  Our functions
#' reflect this.
#'
#' There will likely be cases where edges have more than one hierarchical name (for example, the
#' branch to camel species will have the genus Camelus and the family Camelidae). When making edge
#' labels, you have the choice of using the most recent hierarchical name (genus in the camel
#' example) or the oldest (family in camels), or you can choose to combine the names so that you
#' can see all of them (Camilidae.Camelus).
#'
#' @seealso \code{\link{MakeHierarchyTree}}
#'
#' @examples \dontrun{
#' data(MyHiers)
#' Tree <- MakeHierarchyTree(MyHiers, includeNodeLabels=FALSE)
#' dges <- MakeEdgeLabels(MyHiers)
#' plot(Tree, show.node.label=FALSE)
#' edgelabels(text=names(edges), edge=edges)
#'
#' edges <- MakeEdgeLabels(MyHiers, missingData="pruneTaxa", duplicateEdgeLabels="recent")
#' plot(Tree, show.node.label=FALSE)
#' edgelabels(text=names(edges), edge=edges)
#' }

MakeEdgeLabels <- function(MyHiers, label="all", missingData=NULL, duplicateEdgeLabels="oldest"){
  MyHiers <- RemoveNAFiles(MyHiers)
  nodeList <- NodeLabelList(MyHiers, label="all", missingData=missingData)
  if(length(nodeList) == 0)
    stop("Node Labels can not be created, because hierarchy information doesn't overlap")
  phy <- MakeHierarchyTree(MyHiers, missingData=missingData, includeNodeLabels=FALSE)
  tipList <- getTipList(phy)
  edges <- c(lapply(nodeList, whichEdge, phy=phy), recursive=T)
  if(any(duplicated(names(edges))))
    edges <- edges[-which(duplicated(names(edges)))]
  if(any(duplicated(edges))){
    for(i in unique(edges[which(duplicated(edges))])){
      duplicateEdges <- edges[which(edges == i)]
      names(edges)[which(edges == i)] <- WhatToDoWithDuplicateEdgeNames(duplicateEdges, duplicateEdgeLabels)
    }
  }
  for(i in sequence(length(edges))){
    tipList[which(tipList[,2] == edges[i]),4] <- names(edges)[i]
  }
  justInts <- which(tipList[,3] == "internal")   #to get row to use in ape
  names(justInts) <- tipList[tipList[,3] == "internal",4]  #associate taxon name with row
  if(any(names(justInts) == 0))
    justInts <- justInts[-which(names(justInts) == 0)]
  return(justInts)
}

#' @rdname MakeEdgeLabels
#' @export
getTipList <- function(phy) {
  tipList <- cbind(phy$edge, phy$edge[,2] %in% phy$edge[,1], rep(0, dim(phy$edge)[1]))
  tips <- which(tipList[,3] == 0)
  tipList[tips,3] <- "tip"
  tipList[tips,4] <- phy$tip.label[as.numeric(tipList[tips,2])]
  ints <- which(tipList[,3] == 1)
  tipList[ints,3] <- "internal"
  return(data.frame(tipList, stringsAsFactors=F))
}

#' @rdname MakeEdgeLabels
#' @export
whichEdge <- function(phy, taxa) {
  tipList <- getTipList(phy)
  nodes <- tipList[tipList[, 4] %in% taxa, 1]
  foundBranch <- FALSE
  while(foundBranch==FALSE) {
    for(i in as.numeric(unique(nodes))){
      leaves <- node.leaves(phy, i)
      if(all(leaves %in% taxa) && all(taxa %in% leaves)){
        foundBranch <- TRUE
        return(as.numeric(i))
      }
    }
    nodes <- tipList[which(tipList[,2] %in% nodes), 1]
  }
}

#' @rdname MakeEdgeLabels
#' @export
node.leaves<-function(phy, node) {
  n<-length(phy$tip.label)
  if(node<= n) return(phy$tip.label[as.numeric(node)])
    l<-character();
  d<-node.offspring(phy, node);
  for(j in d) {
    if(j <= n) l<-c(l, phy$tip.label[as.numeric(j)])
    else l<-c(l, node.leaves(phy, j));
  }
  return(l);
}

#' @rdname MakeEdgeLabels
#' @export
node.offspring<-function(phy, node) {
  r<-which(phy$edge[,1]==node)
  return(phy$edge[r,2])
}

#' @rdname MakeEdgeLabels
#' @export
WhatToDoWithDuplicateEdgeNames <- function(edgeLabels, duplicateEdgeLabels){
  if(duplicateEdgeLabels == "recent")
    return(names(edgeLabels[length(edgeLabels)]))
  if(duplicateEdgeLabels == "oldest")
    return(names(edgeLabels[1]))
  if(duplicateEdgeLabels == "combined")
    return(paste(names(edgeLabels), sep="", collapse="."))
}