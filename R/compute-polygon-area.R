#' Compute polygon area
#'
#' This function takes in a data frame containing one or more polygons,
#' and computes the (signed) area of each polygon.
#'
#' @inheritParams cut_polygons
#' @param signed If true, the signed areas of the polygons are returned (positive for
#' counter-clockwise oriented polygons, negative for clockwise oriented.)
#'
#' @return A data frame that contains two columns:
#' \item{group}{The group number of the polygon}
#' \item{area}{The area of the polygon}
#'
#' @examples
#' # Example with multiple polygons
#' vertex.df = compute_regular_polygons(
#'   center = data.frame(x = c(0, 10), y = c(0, 0)),
#'   radius = 1,
#'   num.edges = c(4, 1000)
#' )
#' area.df = compute_polygon_area(vertex.df)
#'
#' # Note that the area of polygon 2 is close to pi!
#' print(area.df)
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
compute_polygon_area = function(vertex.df, signed = FALSE) {
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)

  if (!("group" %in% colnames(vertex.df))) {
    vertex.df$group = 1
  }
  #vertex.df = vertex.df[order(vertex.df$group), ]
  group.count = as.numeric(table(vertex.df$group))
  group.count = group.count[order(unique(vertex.df$group))]
  num.groups = length(unique(vertex.df$group))
  x1 = vertex.df$x
  y1 = vertex.df$y
  # Picks out the index of the next vertex in each polygon, where the first vertex
  # is repeated after the last
  next.index = 1:n - rep(c(0, cumsum(group.count))[1:num.groups], group.count)
  next.index = c(next.index[-1], 1)
  next.index = next.index + rep(c(0, cumsum(group.count))[1:num.groups], group.count)
  x2 = vertex.df$x[next.index]
  y2 = vertex.df$y[next.index]
  cross.prod = 1/2*(x1*y2 - x2*y1)
  # Convert group to factor to preserve order
  vertex.df$group = factor(vertex.df$group, levels = unique(vertex.df$group))
  # Compute sum of cross.prod per group
  area.df = stats::aggregate(cross.prod, by = list(group = vertex.df$group), FUN = sum)
  # Turn group back to numeric
  area.df$group = as.numeric(levels(area.df$group))
  colnames(area.df) = c("group", "area")

  if (!signed) area.df$area = abs(area.df$area)
  return(area.df)
}
