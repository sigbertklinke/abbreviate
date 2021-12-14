# abbreviate

Strings are abbreviated to at least "minlength" characters, such that they remain unique (if they were). The abbreviations should be recognisable.

# Installation  

## From CRAN

```R
install.packages("abbreviate")
```

## From github

Note that from github you install the current development version.

```R
library("devtools")
install_github("sigbertklinke/abbreviate")
```

# Examples


```R
# unique with first letters is possible
txt <- c("euclidean", "maximum", "manhattan", "canberra", "minimum")
abbreviate_text(txt, 3)
```

```R
# if identical strings used then same abbreviation
txt <- c("euclidean", "maximum", "manhattan", "manhattan", "canberra", "minimum")
abbreviate_text(txt, 3)
```

```R
# unique abbreviations
txt <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
abbreviate_text(txt, 3)
# unique abbreviations, but not really intuitive
#bbreviate_text(txt, 0)
```

```R
# factor variable
abbreviate_text(unique(iris[,5]))
abbreviate_text(iris[,5])
```

# History
  * 2021-12-12 version 0.1 
