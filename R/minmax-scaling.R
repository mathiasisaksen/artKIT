#' Rescale values using min-max scaling
#'
#' Applies a rescaling that linearly transforms the values into a specified
#' range of values.
#'
#' @param values A numeric vector of values
#' @param lower,upper The lower and upper bounds of the range that the values
#' are mapped onto
#'
#' @return A numeric vector that contains the rescaled values
#'
#' @examples
#' set.seed(123)
#' # Generate values from normal distribution
#' original.values = rnorm(100, -4, 5)
#' # The values lie between approximately -15.5 and 7
#' hist(original.values)
#' # Rescale values to the interval [0, 1]
#' rescaled.values = minmax_scaling(original.values)
#' hist(rescaled.values)
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
minmax_scaling = function(values, lower = 0, upper = 1) {
  if (length(values) < 2) {
    stop("values must contain at least two elements.")
  }
  m = min(values)
  M = max(values)
  rescaled.values = lower + (upper - lower)*(values - m)/(M - m)
  return(rescaled.values)
}
