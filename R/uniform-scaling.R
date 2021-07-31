#' Transform values to be uniformly distributed
#'
#' Applies a rescaling that transforms the values into being uniformly
#' distributed on the interval [0, 1].
#'
#' @param values A numeric vector of values
#'
#' @return A numeric vector that contains the rescaled values
#'
#' @examples
#' set.seed(123)
#' # Generate values from normal distribution
#' original.values = rnorm(100, -4, 5)
#' # The values lie between approximately -15.5 and 7
#' hist(original.values)
#' # Rescale values uniformly to the interval [0, 1]
#' uniform.values = uniform_scaling(original.values)
#' hist(uniform.values)
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
uniform_scaling = function(values) {
  ecdf.function = stats::ecdf(values)
  rescaled.values = ecdf.function(values)
  return(rescaled.values)
}
