test_that("abbreviate", {
  # unique with first letters is possible
  txt <- c("euclidean", "maximum", "manhattan", "canberra", "minimum")
  res <- c("euc", "max", "man", "can" ,"min")
  names(res) <- txt
  expect_equal(abbreviate_text(txt, 3), res)
  # identical strings used
  txt <- c("euclidean", "maximum", "manhattan", "manhattan", "canberra", "minimum")
  res <- c("euc", "max", "man", "man", "can", "min")
  names(res) <- txt
  expect_equal(abbreviate_text(txt, 3), res)
  #' # identical strings and NA used
  txt <- c("euclidean", "maximum", "manhattan", NA, "canberra", "minimum", "abc", "abc")
  res <- c("euc", "max", "man", NA, "can", "min", "abc", "abc")
  names(res) <- txt
  expect_equal(abbreviate_text(txt, 3), res)
  # unique abbreviations, but longer than minlength
  txt <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
  res <- c("ward", "warD", "sin", "com", "ave", "mcq", "med", "cen")
  names(res) <- txt
  expect_equal(abbreviate_text(txt, 3), res)
  #' # unique abbreviations, but not really intuitive
  res <- c("w", "r", "s", "c", "a", "q", "m", "e")
  names(res) <- txt
  expect_equal(abbreviate_text(txt, 0), res)
})
