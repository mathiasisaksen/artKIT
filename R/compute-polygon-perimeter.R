#' Compute polygon perimeter
#'
#' This function takes in a data frame containing one or more polygons,
#' and computes the perimeter of each polygon
#'
#' @inheritParams cut_polygons
#'
#' @return A data frame that contains two columns:
#' \item{group}{The group number of the polygon}
#' \item{perimeter}{The perimeter of the polygon}
#'
#' @examples
#' # Example with multiple polygons
#' vertex.df = compute_regular_polygons(
#'   center = data.frame(x = c(0, 10), y = c(0, 0)),
#'   radius = 1,
#'   num.edges = c(4, 1000)
#' )
#' perimeter.df = compute_polygon_perimeter(vertex.df)
#'
#' # The perimeters should be around 4*sqrt(2) and 2*pi
#' print(perimeter.df)
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
compute_polygon_perimeter = function(vertex.df) {
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)

  if (!("group" %in% colnames(vertex.df))) {
    vertex.df$group = 1
  }
  vertex.df = vertex.df[order(vertex.df$group), ]
  group.count = as.numeric(table(vertex.df$group))
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
  edge.length = sqrt((x2 - x1)^2 + (y2 - y1)^2)

    # Compute sum of edge.length per group
  perimeter.df = stats::aggregate(edge.length, by = list(group = vertex.df$group), FUN = sum)
  colnames(perimeter.df) = c("group", "perimeter")

  return(perimeter.df)
}
