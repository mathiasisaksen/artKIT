#' Compute regular polygons
#'
#' This function computes one or more regular polygons.
#'
#' @param center Specifies the center(s) of the polygon(s). Can either be a
#' numeric vector of length 2 (every polygon has the same center), or a
#' data frame/matrix with 1 row per polygon. If the data frame/matrix has
#' columns named \code{x} and \code{y}, these are used as center coordinates.
#' Otherwise, the first two columns are used.
#' @param radius The radius of the circumscribed circle of the polygon. Can either
#' be a single number or a vector containing one radius per polygon.
#' @param rotation The amount of rotation applied to the polygon(s), in radians.
#' If \code{rotation = 0}, then the first vertex is at the "top" of the polygon.
#' Can either be a single number or a vector containing a different rotation per polygon.
#' @param num.edges The number of edges that the polygon(s) contain. Can either
#' be a single integer or a vector containing a different number of edges per polygon.
#' @param include.info If true, the returned data frame will contain the values for
#' \code{radius}, \code{rotation} and \code{num.edges} as columns.
#' @param ... Custom properties that can be used to store additional information about
#' the polygon(s), such as color and opacity. Can either be a single value or a
#' vector containing a different value per polygon.
#'
#' @return A data frame where each row corresponds to the vertex of a polygon.
#' The following columns are included:
#' \item{x, y}{The coordinates of the vertex}
#' \item{group}{The group number of the polygon}
#' \item{radius, rotation, num.edges}{ The parameters that are specified in the function call.
#' Only included if \code{include.info = TRUE}}
#' \item{...}{Custom columns that are specified in the function call.}
#'
#' @examples
#' set.seed(123)
#' # This generates 10 polygons with different centers, an individual number of edges
#' # and a custom property "color"
#' vertex.df = compute_regular_polygons(
#'   center = data.frame(x = 1:10, y = 1:10),
#'   radius = 0.5,
#'   num.edges = 3:12,
#'   color = sample(c("red", "blue", "green"), 10, replace = TRUE)
#' )
#'
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = vertex.df, aes(x = x, y = y, group = group, fill = color))+
#'   scale_fill_identity()+
#'   coord_fixed()
#'
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
compute_regular_polygons = function(center = c(0, 0), radius = 1, rotation = 0, num.edges = 50, include.info = TRUE, ...) {
  if (contains_columns(center, c("x", "y"))) {
    x = center[, "x"]
    y = center[, "y"]
  } else if (length(dim(center)) > 1 && ncol(center) == 2) {
    x = center[, 1]
    y = center[, 2]
  } else if (is.null(dim(center)) && is.numeric(center) && length(center) == 2) {
    x = center[1]
    y = center[2]
  } else {
    stop('center must either be a numeric vector of length 2, or a data ',
         'frame/matrix that contains 2 columns or has columns named "x" and "y".')
  }

  if (any(radius < 0)) {
    warning("radius contains negative elements.")
  }

  if (any(num.edges < 3)) {
    warning("num.edges contains elements less than 3.")
  }

  properties = c(list(radius = radius, rotation = rotation, num.edges = num.edges), list(...))
  n = max(length(x), sapply(properties, length))

  if (length(x) == 1) {
    x = rep(x, n)
    y = rep(y, n)
  } else if (length(x) != n) {
    stop("center must contain either a single point, or one point per polygon.")
  }


  for (name in names(properties)) {
    if (length(properties[[name]]) == 1) {
      properties[[name]] = rep(properties[[name]], n)
    } else if (length(properties[[name]]) != n) {
      stop(sprintf('%s must contain either a single value, or one value per polygon.', name))
    }
  }

  total.edges = sum(properties$num.edges)
  # Vector that contains polar coordinate angles for each polygon. Add pi/2 to make first vertex on top
  theta.total = 2*pi*(0:(total.edges - 1) - rep(cumsum(c(0, properties$num.edges))[1:n], properties$num.edges)) /
    rep(properties$num.edges, properties$num.edges) + pi/2

  x.total = rep(x, properties$num.edges)
  y.total = rep(y, properties$num.edges)
  radius.total = rep(properties$radius, properties$num.edges)
  rotation.total = rep(properties$rotation, properties$num.edges)
  theta.rotation.total = theta.total + rotation.total
  group.total = rep(1:n, properties$num.edges)

  result.df = data.frame(
    x = x.total + radius.total*cos(theta.rotation.total),
    y = y.total + radius.total*sin(theta.rotation.total),
    group = group.total
    )
  if (include.info) {
    for (name in names(properties)) {
      result.df[[name]] = rep(properties[[name]], properties$num.edges)
    }
  } else {
    dot.properties = list(...)
    for (name in names(dot.properties)) {
      result.df[[name]] = rep(dot.properties[[name]], properties$num.edges)
    }
  }
  return(result.df)
}

