#' Example schema template
#'
#' Named character vector mapping column names to SQLite types.
#'
#' @format A named character vector of length 3.
"example_template"

#' Default schema template
#'
#' Named character vector mapping column names to SQLite types.
#'
#' @format A named character vector with fields:
#' \describe{
#'   \item{id}{SQLite type.}
#'   \item{numeric_feature}{SQLite type.}
#'   \item{text_feature}{SQLite type.}
#'   \item{logical_feature}{SQLite type.}
#' }
"default_template"

#' Random sentences
#'
#' Toy text data for examples/tests.
#'
#' @format A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{id}{Character identifier.}
#'   \item{text}{Sentence text.}
#' }
"random_sentences"
