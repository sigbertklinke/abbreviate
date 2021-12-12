#' abbreviate_text
#'
#' Abbreviate strings to at least \code{minlength} characters, such that they remain unique (if they were).
#' Duplicate strings or \code{NA}s are allowed.
#'
#' @param txt character: vector of strings to abbreviate
#' @param minlength integer: the minimum length of the abbreviations
#' @param alnum logical: should strings be reduced to alpha-numeric characters before abbreviation (default: \code{TRUE})
#'
#' @seealso [base::abbreviate] or \href{https://CRAN.R-project.org/package=uniqtag}{package \code{uniqtag}}
#' @md
#' @return abbreviated strings
#' @export
#'
#' @examples
#' # unique with first letters is possible
#' txt <- c("euclidean", "maximum", "manhattan", "canberra", "minimum")
#' abbreviate_text(txt, 3)
#' # if identical strings used then same abbreviation
#' txt <- c("euclidean", "maximum", "manhattan", "manhattan", "canberra", "minimum")
#' abbreviate_text(txt, 3)
#' # warnings that identical strings and identical abbreviations used
#' txt <- c("euclidean", "maximum", "manhattan", NA, "canberra", "minimum", "abc", "abc")
#' abbreviate_text(txt, 3)
#' # unique abbreviations
#' txt <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
#' abbreviate_text(txt, 3)
#' # unique abbreviations, but not really intuitive
#' abbreviate_text(txt, 0)
abbreviate_text <- function(txt, minlength=3, alnum=TRUE) {
  txtc <- as.character(txt)
  txtc <- txtc[!is.na(txtc)]
  txtc <- txtc[!duplicated(txtc)]
  # if (anyDuplicated(txt)) warning ("duplicate strings")
  n   <- length(txtc)
  df  <- data.frame(ret=rep(NA_character_, n), txt=txtc, wrk=txtc)
  if (alnum) {
    res <- gsub("[^[:alnum:]]", "", txtc)
    if (!anyDuplicated(res)) df$wrk <- res
  }
  #
  df$ret  <- substring(df$wrk, 1, minlength)
  # step 1: sort by group length singleton and string length
  eqm  <- outer(df$ret, df$ret, "==")
  eqm  <- matrix(eqm[!duplicated(eqm),], ncol=n)
  grps <- lapply(seq_len(nrow(eqm)), function(i) {
    ind <- which(eqm[i,])
    if (length(ind)) return(ind[order(nchar(df$wrk[ind]))])
    i
  })
  grps <- grps[order(lengths(grps))]
  df   <- df[unlist(grps),]
  # step 2 : work on all groups
  eqm  <- outer(df$ret, df$ret, "==")
  eqm  <- matrix(eqm[!duplicated(eqm),], ncol=n)
  grps <- lapply(seq_len(nrow(eqm)), function(i) { which(eqm[i,]) })
  ngrp <- lengths(grps)
  for (i in 1:length(grps)) {
    if (ngrp[i]>1) {
      grpi <- grps[[i]]
      nr   <- max(nchar(df$wrk[grpi]))
      ind  <- unlist(grps[1:i])
      if (nr>minlength) {
        for (j in (minlength+1):nr) {
          ret       <- df$ret[ind]
          ret[grpi] <- paste0(ret[grpi], substr(df$wrk[grpi], j, j))
          ndup      <- setdiff(grpi, which(duplicated(ret[ind])))
          if (length(ndup)) {
            df$ret[ndup] <- ret[ndup]
            remi         <- setdiff(grpi, ndup)
            if (length(remi)==0) break
            # sort choosen them up
            df[grpi,] <- df[c(ndup,remi),]
            grpi      <- (length(ind)-length(remi)+1):length(ind)
          }
        }
      }
    }
  }
  #if (anyDuplicated(df$ret)) warning ("duplicate abbreviations")
  ret <- rep(NA_character_, length(txt))
  for (i in seq(length(txt))) {
    if (!is.na(txt[i])) ret[i] <- df$ret[which(df$txt==txt[i])]
  }
  structure(ret, names=txt)
}
