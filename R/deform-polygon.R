#' Deform polygon
#'
#' This function deforms a polygon by moving each vertex a specified distance.
#' If you want a smooth deformation of a polygon with few vertices (e.g. a square),
#' then \code{\link{interpolate_polygon}} can be used to create an interpolated version
#' of the polygon (see example below).
#'
#' @inheritParams cut_polygons
#' @param deform.distance The distance that each vertex is to be moved. Can
#' either be a single value or a vector containing one value per vertex.
#' If the value is positive, the vertex is moved "outwards", while a negative
#' value moves the vertex "inwards".
#' @param method Must be one of the following values:
#' \itemize{
#'   \item{"bisector", where the vertex is moved along the bisector of the two adjacent edges}
#'   \item{"centroid", where the vertex is moved along the line between the vertex
#'   and the centroid of the polygon}
#' }
#'
#' @note This function is not robust, and can lead to strange results. While
#' \code{method = "bisector"} generally looks better, it struggles in certain
#' situations (see example #3 below), and should only be used for expanding convex
#' polygons. If the polygon is concave and \code{deform.distance} contains
#' both positive and negative values, \code{method = "centroid"} might be a
#' better choice.
#'
#' @return The same data frame that is given in \code{vertex.df}, the only difference
#' being that the \code{x}- and \code{y}-columns are changed due to the deformation.
#'
#' @examples
#' # Example 1: Deformation of rounded square
#' # Start with a square
#' square.df = data.frame(x = c(-1, 1, 1, -1), y = c(1, 1, -1, -1))
#' # Round the corners of the square
#' rounded.df = round_polygon_corners(square.df, corner.radius = 0.3)
#' # The rounded polygon has many vertices along the corners, but none along the
#' # straight edges. We need to resample!
#' # Create function that allows us to resample vertices along the boundary of the polygon
#' interpolation.function = interpolate_polygon(rounded.df)
#' # How many vertices should resampled polygon contain?
#' num.vertices = 1000
#' # Create vector with n values evenly spaced between 0 and 1
#' # (0 and 1 correspond to the same point, so the latter is removed)
#' time = seq(0, 1, length.out = num.vertices + 1)[-(num.vertices + 1)]
#'
#' # Polygon that consists of 1000 vertices placed evenly along the boundary of
#' # the rounded polygon
#' resampled.df = interpolation.function(time)
#'
#' # Function that gives the amount of deformation for time between 0 and 1
#' deform.function = function(t, freq) 0.1 + (0.35 - 0.1)*(1 + cos(freq*2*pi*t)) / 2
#' # Vector that gives the amount of deformation for each vertex in resampled.df
#' deform.amount = deform.function(time, 4)
#'
#' # Deformed polygon
#' deform.df = deform_polygon(resampled.df, deform.amount, method = "bisector")
#'
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = square.df, aes(x = x, y = y, color = "1. Original"), fill = NA)+
#'   geom_polygon(data = rounded.df, aes(x = x, y = y, color = "2. Rounded"), fill = NA)+
#'   geom_polygon(data = deform.df, aes(x = x, y = y, color = "3. Deformed"), fill = NA)+
#'   scale_color_manual(values = c("black", "blue", "red"))+
#'   coord_fixed()
#'
#' # Example 2: Comparison of "bisector" and "centroid" methods
#' # Start with a 2 x 10 rectangle
#' rectangle.df = data.frame(x = c(-1, 1, 1, -1), y = c(5, 5, -5, -5))
#'
#' # Resample polygon, this time without rounding
#' interpolation.function = interpolate_polygon(rectangle.df)
#' resampled.df = interpolation.function(time)
#'
#' # Deformation that oscillates faster than in example 1
#' deform.amount = deform.function(time, 20)
#'
#' # Deformed polygons
#' bisector.df = deform_polygon(resampled.df, deform.amount, method = "bisector")
#' centroid.df = deform_polygon(resampled.df, deform.amount, method = "centroid")
#'
#' # From left to right, the plot shows the original rectangle, and the deformed polygons
#' # from the "bisector" and "centroid" methods, respectively.
#' # The result from "bisector" looks better overall, but struggles at sharp corners,
#' # where the deformed polygon ends up having large gaps between the vertices.
#' # This is avoided by rounding the polygon before deformation.
#' # The deformed polygon from the "centroid" method handles the corners without problem,
#' # but the amount of deformation is not consistent along the vertical sides.
#' ggplot()+
#'   geom_polygon(data = rectangle.df, aes(x = x, y = y), fill = "red")+
#'   geom_polygon(data = bisector.df, aes(x = x + 3, y = y), fill = "red")+
#'   geom_polygon(data = centroid.df, aes(x = x + 6, y = y), fill = "red")+
#'   geom_point(data = bisector.df, aes(x = x + 3, y = y), color = "black", size = 0.5)+
#'   geom_point(data = centroid.df, aes(x = x + 6, y = y), color = "black", size = 0.5)+
#'   coord_fixed()
#'
#' # Example 3: Situation where "bisector" method fails
#' set.seed(123)
#' # Generate random polygon using polar coordinates
#' num.vertices = 10
#' theta = seq(0, 2*pi, length.out = num.vertices + 1)[-(num.vertices + 1)]
#' radius = runif(num.vertices, 0.5, 1)
#' vertex.df = data.frame(x = radius*cos(theta), y = radius*sin(theta))
#' # How the original polygon looks:
#' ggplot()+
#'   geom_polygon(data = vertex.df, aes(x = x, y = y), fill = NA, color = "black")+
#'   coord_fixed()
#'
#' # Resample polygon before deforming
#' interpolation.function = interpolate_polygon(vertex.df)
#' resampled.df = interpolation.function(time)
#'
#' # Deform function goes from -0.2 to 0.2, and back to -0.2
#' deform.function = function(t) -0.2*cos(2*pi*t)
#' deform.amount = deform.function(time)
#'
#' # Deform resampled polygon
#' deform.df = deform_polygon(resampled.df, deform.amount)
#'
#' # How the deformed polygon looks:
#' ggplot()+
#'   geom_polygon(data = deform.df, aes(x = x, y = y), fill = NA, color = "black")+
#'   coord_fixed()
#' # The combinations convex corner/negative deformation (right side of plot)
#' # and concave corner/positive deformation (left side) leads to a shape that is
#' # self-intersecting.
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
deform_polygon = function(vertex.df, deform.distance, method = "bisector") {
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)

  if (!(length(deform.distance) %in% c(1, n))) {
    stop("deform.distance must either be a single value or a vector of length ",
         "nrow(vertex.df).")
  }

  if (method == "bisector") {
    unit.deform.vector = compute_deform_direction_bisector(vertex.df)
  } else if (method == "centroid") {
    unit.deform.vector = compute_deform_direction_centroid(vertex.df)
  } else {
    stop('method must be either "bisector" or "centroid".')
  }

  deform.df = vertex.df
  deform.df[, c("x", "y")] = deform.df[, c("x", "y")] + deform.distance * unit.deform.vector

  return(deform.df)
}

compute_deform_direction_bisector = function(vertex.df) {
  n = nrow(vertex.df)
  vector.sign = ifelse(is_polygon_ccw(vertex.df), 1, -1)

  vertex.before = vertex.df[c(n, 1:(n-1)), c("x", "y")]
  vertex.after = vertex.df[c(2:n, 1), c("x", "y")]

  angle.before = atan2(vertex.before$y - vertex.df$y, vertex.before$x - vertex.df$x) %% (2*pi)
  angle.after = atan2(vertex.after$y - vertex.df$y, vertex.after$x - vertex.df$x) %% (2*pi)
  angle.internal = angle.after - angle.before
  angle.middle = (angle.before + angle.after) / 2

  fix.indices = angle.internal < 0
  angle.middle[fix.indices] = angle.middle[fix.indices] + pi

  unit.deform.vector = vector.sign*data.frame(x = cos(angle.middle), y = sin(angle.middle))
  return(unit.deform.vector)
}

compute_deform_direction_centroid = function(vertex.df) {
  centroid = compute_polygon_centroid(vertex.df)

  deform.vector = data.frame(
    x = vertex.df$x - centroid$x,
    y = vertex.df$y - centroid$y
  )
  unit.deform.vector = normalize_vectors(deform.vector)
  return(unit.deform.vector)
}
