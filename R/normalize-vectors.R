#' Normalize vectors to have length 1
#'
#' Takes in one or more vectors and normalizes their lengths to 1.
#'
#' @param vectors A n x d matrix or data frame that contains one vector per row,
#' and one column per dimension.
#'
#' @return The return value is of the same type as \code{vectors}, and contains
#' the normalized vectors.
#'
#' @examples
#' set.seed(123)
#' # Generate random 2D vectors
#' vectors = matrix(runif(200, -1, 1), ncol = 2)
#' # The lengths are varied
#' summary(sqrt(rowSums(vectors^2)))
#' # Normalize vectors
#' normalized.vectors = normalize_vectors(vectors)
#' # The lengths are all equal to 1
#' summary(sqrt(rowSums(normalized.vectors^2)))
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
normalize_vectors = function(vectors) {
  if (is.null(dim(vectors))) {
    vectors = matrix(vectors, nrow = 1)
  }

  lengths = rep(0, nrow(vectors))
  for (i in 1:ncol(vectors)) {
    lengths = lengths + vectors[, i]^2
  }
  lengths = sqrt(lengths)
  nonzero.elems = lengths > 0

  result = vectors
  result[nonzero.elems, ] = result[nonzero.elems, ] / lengths[nonzero.elems]
  return(result)
}
