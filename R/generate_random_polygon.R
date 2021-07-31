#' Generate random polygon
#'
#' This function generates a random polygon. It uses the polar coordinate
#' representation, where the angles are evenly spaced over the interval [0, 2pi]
#' and the radiuses are sampled from a uniform distribution.
#'
#' @param center The "center" of the polygon, which determines its position. Should
#' either be a 1 x 2 data frame/matrix or a numeric vector with 2 elements.
#' @param min.radius,max.radius The minimum and maximum bounds used in the uniform
#' distribution that samples the radius values
#' @param num.vertices The number of vertices that the random polygon should contain
#' @param seed The seed used when sampling the radius value
#'
#' @return A data frame that contains one row per vertex in the polygon, and the
#' columns \code{x} and \code{y}.
#'
#' @examples
#' # A random polygon with center (5, 10) and 10 vertices. The distance from
#' # the center to the vertices is between 4 and 10
#' vertex.df = generate_random_polygon(
#'   center = c(5, 10),
#'   min.radius = 4,
#'   max.radius = 10,
#'   num.vertices = 10,
#'   seed = 123
#' )
#'
#' library(ggplot2)
#' # The center of the polygon is shown in red
#' ggplot()+
#'   geom_polygon(data = vertex.df, aes(x = x, y = y))+
#'   geom_point(aes(x = 5, y = 10), color = "red")+
#'   coord_fixed()
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
generate_random_polygon = function(center = c(0, 0), min.radius = 0.5, max.radius = 1, num.vertices = 5, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  center = as.numeric(center)
  theta = seq(0, 2*pi, length.out = num.vertices + 1)[-(num.vertices + 1)]
  radius = stats::runif(num.vertices, min.radius, max.radius)
  vertex.df = data.frame(
    x = center[1] + radius*cos(theta),
    y = center[2] + radius*sin(theta)
  )
  return(vertex.df)
}
