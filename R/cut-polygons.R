#' Repeated cutting of polygons
#'
#' This function takes in an arbitrary number of polygons and cuts them a given
#' number of times. For each iteration, every polygon is cut by a randomly generated
#' line, which divides it into several parts.
#'
#' @param vertex.df A data frame where each row corresponds to the vertex of a polygon.
#' It must contain the columns \code{x}, \code{y}, and optionally \code{group} (must be integer).
#' \code{x} and \code{y} specify the coordinates of the vertex,
#' and \code{group} is used to indicate which polygon the vertex belongs to.
#' If \code{vertex.df} contains only a single polygon, \code{group} can be omitted.
#' @param number.of.iterations The number of times that the polygons are cut in
#' half. Either a single value, or a numeric vector with one value per polygon in \code{vertex.df}.
#' @param use.centroid If \code{TRUE}, the randomly generated line goes through the centroid
#' of the polygon. Otherwise, the line goes through a point sampled uniformly
#' from the interior of the polygon.
#' @param seed The seed used when generating the directions of the cut lines.
#' When no value is specified, the seed is not set.
#' @param return.all If \code{TRUE}, the function will return a data frame
#' that contains every iteration (i.e. the state after iteration 1,...,\code{number.of.iterations}).
#'
#' @note \code{number.of.iterations} should be chosen with care, as each
#' iteration at least doubles the number of polygons, leading to exponential growth.
#'
#' @details The directions of the lines are sampled from a uniform distribution.
#'
#' @return A data frame where each row corresponds to the vertex of a polygon,
#' with the following columns:
#' \item{x, y}{The coordinates of the vertex}
#' \item{group}{The index of the polygon that the vertex belongs to}
#' \item{iteration}{The iteration that the polygon belongs to (only present when \code{return.all = TRUE})}
#'
#' @examples
#' # Example with single initial polygon (square)
#' vertex.df = data.frame(
#'   x = c(-1, 1, 1, -1),
#'   y = c(1, 1, -1, -1)
#' )
#' cut.df = cut_polygons(vertex.df, number.of.iterations = 10, seed = 123)
#'
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = cut.df,
#'                aes(x = x, y = y, group = group),
#'                fill = NA, color = "black")+
#'   coord_fixed()
#'
#' # Example with multiple initial polygons (square and triangle)
#' vertex.df = data.frame(
#'   x = c(-1, 1, 1, -1, 1, -1, 0),
#'   y = c(1, 1, -1, -1, 2, 2, 2+sqrt(3)),
#'   group = c(rep(1, 4), rep(2, 3))
#' )
#'
#' # Cut square and triangle 10 and 9 times, respectively
#' cut.df = cut_polygons(vertex.df, number.of.iterations = c(10, 9), seed = 123)
#'
#' ggplot()+
#'   geom_polygon(data = cut.df,
#'                aes(x = x, y = y, group = group),
#'                fill = NA, color = "black")+
#'   coord_fixed()
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
cut_polygons = function(vertex.df, number.of.iterations, use.centroid = TRUE, seed = NULL, return.all = FALSE) {
  check_vertex_df(vertex.df)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cut_function = if(return.all) cutEveryPolygonReturnAllRcpp else cutEveryPolygonRcpp

  if (!contains_columns(vertex.df, "group")) {
    vertex.df$group = 1
  }

  groups = unique(vertex.df$group)
  num.groups = length(groups)

  if (length(number.of.iterations) == 1) {
    cut.df = cut_function(vertex.df$x, vertex.df$y, vertex.df$group, number.of.iterations, use.centroid)
  } else if (length(number.of.iterations) == num.groups) {
    cut.df = data.frame()
    max.group = 0
    for (i in 1:num.groups) {
      group.df = vertex.df[vertex.df$group == groups[i], ]
      current.df = cut_function(group.df$x, group.df$y, group.df$group, number.of.iterations[i], use.centroid)
      current.df$group = current.df$group + max.group
      cut.df = rbind(cut.df, current.df)
      max.group = max(current.df$group)
    }
  } else {
    stop("number.of.iterations must either be a single integer or ",
         "a numeric vector containing one integer per group in vertex.df.")
  }
  return(cut.df)
}
