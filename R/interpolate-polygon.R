#' Interpolation of polygon boundary
#'
#' This function takes in a single polygon and returns a function that interpolates
#' the boundary of the polygon. This is useful for "filling in"
#'
#' @param vertex.df A data frame where each row corresponds to a vertex in the polygon.
#' It must contain the columns \code{x} and \code{y} where \code{x} and \code{y}
#' specify the coordinates of the vertex,
#' @param method Can be either "linear" (linear interpolation) or "spline" (periodic
#' cubic spline interpolation). When "linear" is used, the shape traced out by
#' the interpolation function has the same shape as the original polygon, and the
#' edges remain straight. With "spline", the interpolation function is guaranteed
#' to go through the vertices of the original polygon, but the shape will be smooth
#' (i.e. no straight edges and sharp corners).
#'
#' @details When \code{method = "linear"}, the interpolation function has the nice
#' property that evenly distributed inputs will lead to vertices that are evenly distributed
#' along the boundary of the polygon (i.e. the distance between consecutive vertices is constant).
#' In other words: If L is the perimeter of the original polygon, then the arc length
#' traced by the interpolation function from time 0 to t is L*t.
#'
#' @note The results obtained with \code{method = "spline"} are generally unpredictable,
#' such shapes that are self-intersecting, or perfect circles when the input is a regular polygon.
#' This option should therefore be used with caution. If the goal is to get a
#' smoothed version of the polygon, \code{\link{round_polygon_corners}} might be a better choice.
#'
#' @return The return value is a function that interpolates along the boundary
#' of the polygon. It takes in two parameters:
#' \item{time.values}{A single value or a vector of n values between 0 and 1, where
#' 0 corresponds to the position of the first vertex, 0.5 gives the position exactly halfway
#' around the perimeter, and 1 is the "end" of the polygon (which is back at the first vertex).}
#' \item{original.vertices}{Can either be "ignore", "replace" or "add". If "ignore",
#' the interpolation function is evaluated in the exact values given in \code{time.values}.
#' If "replace", some of the values in \code{time.values} will be replaced so that
#' the returned vertices are guaranteed to contain the original vertices. With "add",
#' the original vertices are added inbetween the vertices obtained with \code{time.values}.}
#' The function returns an n x 2 (plus some extra rows when \code{original.vertices = add}) data frame that contains the x- and y-coordinates of
#' the interpolated vertices.
#'
#' @examples
#' # Example: Interpolation of pentagon and comparison of original.vertices
#' # Create pentagon using compute_regular_polygons
#' vertex.df = compute_regular_polygons(center = c(0, 0), radius = 1, num.edges = 5)
#' # How the pentagon looks:
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = vertex.df, aes(x = x, y = y), fill = "pink")+
#'   geom_point(data = vertex.df, aes(x = x, y = y), color = "black")+
#'   coord_fixed()
#' # Get boundary interpolation function from interpolate_polygon
#' interpolation.function = interpolate_polygon(vertex.df)
#' # The number of vertices that we want to end up with
#' num.interp = 18
#' # Create 18 evenly distributed values between 0 and 1. Remove last element,
#' # as 1 leads to same position as 0
#' time.interp = seq(0, 1, length.out = num.interp + 1)[-(num.interp + 1)]
#'
#' # One data frame per value of original.vertices
#' ignore.df = interpolation.function(time.interp, original.vertices = "ignore")
#' replace.df = interpolation.function(time.interp, original.vertices = "replace")
#' add.df = interpolation.function(time.interp, original.vertices = "add")
#'
#' # Comparison of interpolated vertices with different original.vertices:
#' ggplot()+
#'   geom_polygon(data = ignore.df, aes(x = x, y = y, fill = "1. ignore"))+
#'   geom_point(data = ignore.df, aes(x = x, y = y), color = "black")+
#'   geom_polygon(data = replace.df, aes(x = x + 2, y = y, fill = "2. replace"))+
#'   geom_point(data = replace.df, aes(x = x + 2, y = y), color = "black")+
#'   geom_polygon(data = add.df, aes(x = x + 4, y = y, fill = "3. add"))+
#'   geom_point(data = add.df, aes(x = x + 4, y = y), fill = NA, color = "black")+
#'   scale_fill_manual("", values = c("red", "green", "blue"))+
#'   coord_fixed()
#' # When original.vertices = "ignore", the interpolated vertices contain only the
#' # first vertex from the original polygon, which leads to a different shape.
#' # The vertices are, however, evenly distributed (i.e. the distance between consecutive
#' # vertices is constant). With "replace", the interpolated vertices contain all
#' # of the original vertices, but they are not evenly distributed along
#' # the boundary. The same holds for "add", where the interpolated vertices
#' # contains both the vertices shown in "ignore" and the original vertices.
#' # Note: These differences are very noticeable since num.interp is small.
#' # Usually, we will use more than 18 interpolated vertices.
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
interpolate_polygon = function(vertex.df, method = "linear") {
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)

  # Add first vertex to end to simplify computations
  vertex.df = vertex.df[c(1:n, 1), ]

  # Vector of distances between vertices
  edge.length = sqrt(diff(vertex.df$x)^2 + diff(vertex.df$y)^2)

  # The proportion of each edge relative to the whole perimeter
  proportion = edge.length / sum(edge.length)
  proportion.cumulative = c(0, cumsum(proportion))

  if (method == "linear") {
    spline.x = stats::approxfun(proportion.cumulative, vertex.df$x)
    spline.y = stats::approxfun(proportion.cumulative, vertex.df$y)
  } else if (method == "spline") {
    spline.x = stats::splinefun(proportion.cumulative, vertex.df$x, method = "periodic")
    spline.y = stats::splinefun(proportion.cumulative, vertex.df$y, method = "periodic")
  } else {
    stop("Invalid value for method, must be either \"linear\" or \"spline\".")
  }

  original.time = proportion.cumulative[-(n + 1)]

  interpolation_function = function(time.values, original.vertices = "ignore") {
    if (0 %in% time.values && 1 %in% time.values) {
      warning("time.values contains both 0 and 1. These correspond to the same ",
              "point on the polygon.")
    }
    if (original.vertices == "replace") {
      if (length(time.values) < n) {
        stop('When original.vertices == "replace", the length of time.values ',
             'must be equal to or greater than nrow(vertex.df).')
      }
      index.match = multiple_closest_values(time.values, original.time, num.closest = n)
      fixed.time = time.values
      fixed.time[index.match[, 1]] = original.time[index.match[, 2]]

    } else if (original.vertices == "add") {
      fixed.time = sort(unique(c(time.values, original.time)))
    } else if (original.vertices == "ignore") {
      fixed.time = time.values
    } else {
      stop('Invalid values for original.vertices, must be either "replace", "add", ',
           'or "ignore".')
    }

    x = spline.x(fixed.time)
    y = spline.y(fixed.time)
    return(data.frame(x = x, y = y, time = fixed.time))
  }

  return(interpolation_function)
}
