#' Transition between polygons
#'
#' Function that interpolates/transitions between two polygons. This is done by
#' computing \code{(1 - time)*start.polygon + time*end.polygon}.
#'
#' @param start.polygon,end.polygon Dataframes/matrices containing the vertices of the
#' polygons to interpolate between. The coordinates of the vertices must be stored in columns named "x" and "y",
#' and the polygons must contain the same number of vertices.
#' @param time The times at which we are interested in interpolating the polygons. \code{time = 0} gives
#' \code{start.polygon}, while \code{time = 1} gives \code{end.polygon}. Can be either a single number
#' or a numeric vector. If time contains values outside [0, 1], a warning will be given.
#' @param vertex.order Determines whether and how the vertices in \code{start.polygon} and \code{end.polygon}
#' are reordered before the interpolation is computed. If \code{vertex.order = "preserve"}, no
#' reordering is applied, and the polygons are used as-is. If \code{vertex.order = "reorder"},
#' the function first ensures that the polygons have the same orientation (i.e. clockwise/counter-clockwise).
#' Then, it attempts to shift the indices of the vertices so that the corresponding vertices on
#' \code{start.polygon} and \code{end.polygon} are "aligned".
#'
#' @return A data frame that contains one row per vertex. If \code{start.polygon}
#' and \code{end.polygon} contain n vertices, and \code{time} contains m values, then
#' the returned data frame will contain n*m rows. following columns:
#' \item{x, y}{The coordinates of the vertex}
#' \item{group}{Which polygon the vertex belongs to (1 for the first value in \code{time}, 2 for the second and so on)}
#' \item{time}{The time value of the associated polygon}
#'
#' @note It is recommended to ensure that the start and end polygons have the correct
#' orientation and numbering of vertices before computing the transition, and then using
#' \code{vertex.order = "preserve"}.
#'
#' @examples
#' # Example: Transition from hexagon to square
#' # Create hexagon
#' hexagon.df = compute_regular_polygons(
#'   center = c(0, 0),
#'   radius = 1,
#'   rotation = 0,
#'   num.edges = 6
#' )
#' # Round corners slightly
#' hexagon.df = round_polygon_corners(hexagon.df, corner.radius.scale = 0.3)
#'
#' # Create square
#' square.df = compute_regular_polygons(
#'   center = c(20, -20),
#'   radius = 2,
#'   rotation = 0,
#'   num.edges = 4
#' )
#' # Round corners slightly
#' square.df = round_polygon_corners(square.df, corner.radius.scale = 0.3)
#'
#' # Resample polygons with many vertices, so that the transition becomes smooth
#' num.vertices = 1000
#' resample.time = seq(0, 1, length.out = num.vertices + 1)[-(num.vertices + 1)]
#' hexagon.resample = interpolate_polygon(hexagon.df)(resample.time)
#' square.resample = interpolate_polygon(square.df)(resample.time)
#'
#' # Show transition over 10 steps
#' num.transition = 10
#' transition.time = seq(0, 1, length.out = num.transition)
#' # Use vertex.order = "preserve" (both polygons are CCW, and have the top vertex
#' # as the first in hexagon.df and square.df)
#' transition.df = transition_between_polygons(
#'   hexagon.resample,
#'   square.resample,
#'   transition.time,
#'   "preserve")
#'
#' # Show the result:
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = transition.df, aes(x = x, y = y, group = group), fill = NA, color = "black")+
#'   coord_fixed()
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
transition_between_polygons = function(start.polygon, end.polygon, time, vertex.order = "reorder") {
  check_vertex_df(start.polygon, "start.polygon")
  check_vertex_df(end.polygon, "end.polygon")

  if (nrow(start.polygon) != nrow(end.polygon)) {
    stop(paste(c("start.polygon and end.polygon must contain the same number of vertices. ",
                 "If you wish to interpolate between polygons of different sizes, ",
                 "then interpolate_polygon can be used to make them the same size.")))
  }

  if (min(time) < 0 || max(time) > 1) {
    warning("time contains values outside the interval [0, 1].")
  }
  n = nrow(start.polygon)
  n.times = length(time)

  if (is_polygon_ccw(start.polygon) != is_polygon_ccw(end.polygon)) {
    if (vertex.order != "preserve") {
      end.polygon = end.polygon[n:1, ]
    } else {
      warning(paste(c("start.polygon and end.polygon do not have the same orientation ",
                      "(one is clockwise and the other is counter-clockwise.)")))
    }
  }

  if (vertex.order == "reorder") {
    reordered.polygons = reorder_polygons(start.polygon, end.polygon)
    start.polygon = reordered.polygons$polygon.1
    end.polygon = reordered.polygons$polygon.2
  }

  start.polygon.total = start.polygon[rep(1:n, n.times), c("x", "y")]
  end.polygon.total = end.polygon[rep(1:n, n.times), c("x", "y")]
  time.total = rep(time, each = n)
  group = rep(1:n.times, each = n)

  result = (1 - time.total)*start.polygon.total + time.total*end.polygon.total
  result$time = time.total
  result$group = group
  return(result)
}

reorder_polygons = function(polygon.1, polygon.2) {
  n = nrow(polygon.1)
  centroid.1 = compute_polygon_centroid(polygon.1)
  centroid.2 = compute_polygon_centroid(polygon.2)

  centered.polygon.1 = polygon.1[, c("x", "y")] - centroid.1[rep(1, n), c("x", "y")]
  centered.polygon.2 = polygon.2[, c("x", "y")] - centroid.2[rep(1, n), c("x", "y")]

  closest.indices = closest_pair_of_points(centered.polygon.1, centered.polygon.2)
  reordered.indices.start = (1:n + closest.indices[1] - 2) %% n + 1
  reordered.indices.end = (1:n + closest.indices[2] - 2) %% n + 1

  reordered.polygon.1 = polygon.1[reordered.indices.start, ]
  reordered.polygon.2 = polygon.2[reordered.indices.start, ]
  return(list(polygon.1 = reordered.polygon.1, polygon.2 = reordered.polygon.2))
}

