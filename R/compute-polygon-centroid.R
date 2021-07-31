#' Compute polygon centroid
#'
#' This function takes in a data frame containing one or more polygons,
#' and computes the centroid (geometric center) of each polygon.
#'
#' @inheritParams  cut_polygons
#'
#' @return A data frame that contains three columns:
#' \item{group}{The group number of the polygon}
#' \item{x, y}{The coordinates of the centroid}
#'
#'
#' @examples
#' # Example with multiple polygons
#' vertex.df = compute_regular_polygons(
#'  center = data.frame(x = c(0, 10), y = c(0, 0)),
#'  radius = 1,
#'  num.edges = c(4, 1000)
#' )
#' centroid.df = compute_polygon_centroid(vertex.df)
#'
#' # Centroids should be (0, 0) and c(10, 0)
#' print(centroid.df)
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
compute_polygon_centroid = function(vertex.df) {
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)

  if (!("group" %in% colnames(vertex.df))) {
    vertex.df$group = 1
  }

  area.df = compute_polygon_area(vertex.df, signed = TRUE)
  group.count = as.numeric(table(vertex.df$group))
  group.count = group.count[order(unique(vertex.df$group))]
  num.groups = length(unique(vertex.df$group))
  area.df = area.df[rep(1:num.groups, group.count), ]
  x1 = vertex.df$x
  y1 = vertex.df$y
  # Picks out the index of the next vertex in each polygon, where the first vertex
  # is repeated after the last
  next.index = 1:n - rep(c(0, cumsum(group.count))[1:num.groups], group.count)
  next.index = c(next.index[-1], 1)
  next.index = next.index + rep(c(0, cumsum(group.count))[1:num.groups], group.count)
  x2 = vertex.df$x[next.index]
  y2 = vertex.df$y[next.index]
  # Convert group to factor to preserve order
  vertex.df$group = factor(vertex.df$group, levels = unique(vertex.df$group))

  x.parts = 1/(6*area.df$area)*(x1 + x2)*(x1*y2 - x2*y1)
  x.centroid = stats::aggregate(x.parts, by = list(group = vertex.df$group), FUN = sum)

  y.parts = 1/(6*area.df$area)*(y1 + y2)*(x1*y2 - x2*y1)
  y.centroid = stats::aggregate(y.parts, by = list(group = vertex.df$group), FUN = sum)

  centroid.df = x.centroid
  centroid.df$y = y.centroid$x
  centroid.df$group = as.numeric(levels(centroid.df$group))
  return(centroid.df)
}
