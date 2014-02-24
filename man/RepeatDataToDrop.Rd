\name{RepeatDataToDrop}
\alias{RepeatDataToDrop}
\alias{DropADim}
\title{Repeat Data To Drop}
\description{
	These internal functions determine columns or rows of data that are either all the same (thus providing no hierarchical information) or contain NAs. \code{DropADim} walks through the TreeData table and deletes either a row or a column that has the most amount of missing data.  It continues this stepwise until there are no longer any NAs in TreeData.  This preserves as much data as possible.  Then \code{RepeatDataToDrop} will delete any columns of TreeData that have all the same information (these can not be used with ape tree plotting)
}
\usage{
RepeatDataToDrop(TreeData)
DropADim(TreeData)
}
\arguments{
	\item{TreeData}{A data frame from }
}
\value{
\code{DropADim} returns a new TreeData table with no NAs.  \code{RepeatDataToDrop} returns boolean response of whether to drop data or not.
}
