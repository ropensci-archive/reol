Reol
====



[![Build Status](https://travis-ci.org/ropensci/reol.png)](https://travis-ci.org/ropensci/reol)
[![codecov.io](https://codecov.io/github/ropensci/reol/coverage.svg?branch=master)](https://codecov.io/github/ropensci/reol?branch=master)

## Reol - Interface to the Encyclopedia for Life

* [EOL API](http://eol.org/info/152)

## Installation

Stable version from CRAN


```r
install.packages("Reol")
```

or, Development version from GitHub


```r
install.packages("devtools")
devtools::install_github("ropensci/reol")
```

## Examples


```r
library("Reol")
data(MyEOLs)
```

Provider count 


```r
ProviderCount(MyEOLs[6], verbose=TRUE)
#>                                                          [,1]
#> Integrated Taxonomic Information System (ITIS)              1
#> GBIF Nub Taxonomy                                           1
#> IUCN Red List (Species Assessed for Global Conservation)    1
#> NCBI Taxonomy                                               1
#> Species 2000 & ITIS Catalogue of Life: April 2013           1
#>           Integrated Taxonomic Information System (ITIS) 
#>                                                        1 
#>                                        GBIF Nub Taxonomy 
#>                                                        1 
#> IUCN Red List (Species Assessed for Global Conservation) 
#>                                                        1 
#>                                            NCBI Taxonomy 
#>                                                        1 
#>        Species 2000 & ITIS Catalogue of Life: April 2013 
#>                                                        1
```

Download data


```r
res <- DownloadSearchedTaxa(c("Anolis_carolinensis", "Anolis garmani"), to.file=FALSE, exact=TRUE)
#> [1] "eol795869 saved as R object"
#> [1] "eol1055934 saved as R object"
```

Then parse


```r
out <- GatherDataObjectInformation(res)
out[1:6, 1:6]
#>                 Taxon  eolID                     dataObjectID
#> 1 Anolis carolinensis 795869 e1034e748a8bc9e9828ddede77bc9bab
#> 2 Anolis carolinensis 795869 74c855fd7ec1b579505cf3774cd03bf5
#> 3 Anolis carolinensis 795869 d48f7b24ebac13339a43aea3ec524416
#> 4 Anolis carolinensis 795869 0ecf604ab616f4e7f895da90c861cf28
#> 5 Anolis carolinensis 795869 fcb91b611541d29d311bf8ea0bd112d3
#> 6 Anolis carolinensis 795869 5b9e32ea5f57e778ac1d9cadf7a7e738
#>   taxonConceptID                         dataType  mimeType
#> 1         795869 http://purl.org/dc/dcmitype/Text text/html
#> 2         795869 http://purl.org/dc/dcmitype/Text text/html
#> 3         795869 http://purl.org/dc/dcmitype/Text text/html
#> 4         795869 http://purl.org/dc/dcmitype/Text text/html
#> 5         795869 http://purl.org/dc/dcmitype/Text text/html
#> 6         795869 http://purl.org/dc/dcmitype/Text text/html
```

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/reol/issues)
* License: GPL (>= 2)
* Get citation information for `Reol` in R doing `citation(package = 'Reol')`

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
