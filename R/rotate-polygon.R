#' Rotate polygons
#'
#' Takes in one or more polygons and rotates each one a specified amount.
#'
#' @inheritParams cut_polygons
#' @param rotation The amount each polygon is to be rotated in radians. Either a single
#' number or a vector containing one number per polygon in \code{vertex.df}.
#' @param center.of.rotation The center of rotation, i.e. the point which the polygons
#' will be rotated around. Can be one of the following:
#' \itemize{
#'   \item{"centroid" (default), where each polygon is rotated around its centroid}
#'   \item{A 1 x 2 matrix data frame/matrix that specifices the x- and y-coordinate
#'   of the center of rotation used for every polygon (a numeric vector of length 2
#'    is also accepted, i.e. c(x, y))}
#'   \item{A n x 2 data frame/matrix, where row number i contains the x- and y-coordinates of
#'   the center of rotation for polygon number i in \code{vertex.df}, and n is the number of polygons.}
#' }
#'
#' @return The returned data frame is equal to \code{vertex.df} everywhere, except
#' the \code{x}- and \code{y}-columns that are changed due to the rotation.
#'
#' @examples
#' polygon.center = data.frame(x = c(0, 2, 4), y = c(0, 2, 4))
#' original.df = compute_regular_polygons(
#'   center = polygon.center,
#'   radius = c(1, 1, 1.5),
#'   num.edges = 3:5
#' )
#'
#' # Rotating every polygon the same amount, using the centroids as centers of rotation
#' centroid.df = rotate_polygon(original.df, rotation = pi/8, center.of.rotation = "centroid")
#' # Rotating each polygon a different amount, around a different center of rotation
#' center.of.rotation = data.frame(x = c(2, 0, 4), y = c(0, 2, 0))
#' manual.df = rotate_polygon(
#'   original.df,
#'   rotation = c(pi/4, pi, -pi/2),
#'   center.of.rotation = center.of.rotation
#' )
#'
#' # The plot below shows both the original and rotated polygons.
#' # The black lines connect the center of rotation used in manual.df to the centroids
#' # of the original polygons.
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = original.df, aes(x = x, y = y, group = group, color = "1. Original"),
#'                fill = NA)+
#'   geom_polygon(data = centroid.df, aes(x = x, y = y, group = group, color = "2. Centroid"),
#'                fill = NA)+
#'   geom_polygon(data = manual.df, aes(x = x, y = y, group = group, color = "3. Manual"),
#'                fill = NA)+
#'   geom_segment(aes(x = polygon.center$x, xend = center.of.rotation$x,
#'                    y = polygon.center$y, yend = center.of.rotation$y))+
#'   scale_color_manual("", values = c("red", "green", "blue"))+
#'   coord_fixed()
#'
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
rotate_polygon = function(vertex.df, rotation, center.of.rotation = "centroid") {
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)

  if (!contains_columns(vertex.df, "group")) {
    vertex.df$group = 1
  }

  group.count = as.numeric(table(vertex.df$group))
  num.groups = length(unique(vertex.df$group))

  if (length(rotation) == 1) {
    rotation = rep(rotation, num.groups)
  } else if (length(rotation) != num.groups) {
    stop("\"rotation\" must either be a single number or a numeric vector containing ",
         "one number per group in vertex.df.")
  }

  dims = dim(center.of.rotation)
  uses.centroids = FALSE
  if (!is.null(dims)) {
    if (dims[2] != 2) {
      stop("Invalid value for \"center.of.rotation\". See ?rotate_polygon for valid formats.")
    } else if (dims[1] == 1) {
      center = center.of.rotation[rep(1, num.groups), ]
    } else if (dims[1] == num.groups) {
      center = center.of.rotation
    } else {
      stop("Invalid value for \"center.of.rotation\". See ?rotate_polygon for valid formats.")
    }
  } else if (is.null(dims) && length(center.of.rotation) == 2) {
    center = matrix(center.of.rotation, nrow = num.groups, ncol = 2, byrow = TRUE)
  } else if (center.of.rotation == "centroid") {
    center = compute_polygon_centroid(vertex.df)
    uses.centroids = TRUE
  } else {
    stop("Invalid value for \"center.of.rotation\". See ?rotate_polygon for valid formats.")
  }

  if  (!uses.centroids) {
    colnames(center) = c("x", "y")
  }

  rotation.total = rep(rotation, group.count)
  center.total = center[rep(1:num.groups, group.count), ]

  rotated.df = vertex.df
  rotated.df[, "x"] = center.total[, "x"] +
    (vertex.df[, "x"] - center.total[, "x"])*cos(rotation.total) -
    (vertex.df[, "y"] - center.total[, "y"])*sin(rotation.total)
  rotated.df[, "y"] = center.total[, "y"] +
    (vertex.df[, "x"] - center.total[, "x"])*sin(rotation.total) +
    (vertex.df[, "y"] - center.total[, "y"])*cos(rotation.total)
  return(rotated.df)
}
