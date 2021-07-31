#' Generate rectangle with rounded corners
#'
#' This function computes the vertices of a rectangle with rounded corners.
#'
#' @param center The center/centroid of the shape. Can either be a 1 x 2 data
#' frame/matrix or a numeric vector length 2.
#' @param width,height The width and height of the rectangle before the corners
#' are rounded.
#' @param rotation The amount of rotation applied to the shape, in radians.
#' @param corner.radius The radius of the circular arcs. Should be less than min(width, height) / 2.
#' @param vertices.per.corner The number of vertices that each circular arc consists of.
#'
#' @note The result may contain less than \code{4*vertices.per.corner} vertices.
#' In certain situations, consecutive vertices will end up having the same coordinates.
#' The duplicates are then removed.
#'
#' @return A data frame containing the columns \code{x} and \code{y}, where each
#' row is a vertex in the rounded rectangle.
#'
#' @examples
#' # Create a rounded rectangle
#' vertex.df = rounded_rectangle(
#'   center = c(2, 5),
#'   width = 3,
#'   height = 6,
#'   rotation = pi/4,
#'   corner.radius = 1
#' )
#'
#' # How the rounded rectangle looks:
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = vertex.df, aes(x = x, y = y))+
#'   coord_fixed()
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
rounded_rectangle = function(center = c(0, 0), width = 1, height = 1, rotation = 0, corner.radius = NULL, vertices.per.corner = 50) {
  if (is.null(corner.radius)) {
    corner.radius = min(width, height) / 4
  } else if (corner.radius > min(width, height) / 2) {
    warning("corner.radius should be less than min(width, height) / 2.")
  }
  center = as.numeric(center)
  inner.width = width - 2*corner.radius
  inner.height = height - 2*corner.radius
  corner.offsets = data.frame(x = inner.width*c(1, -1, -1, 1), y = inner.height*c(1, 1, -1, -1)) / 2
  corner.centers = matrix(center, nrow = 4, ncol = 2, byrow = TRUE) + corner.offsets

  corner.centers.ext = corner.centers[rep(1:4, each = vertices.per.corner), ]
  theta = rep(seq(0, pi/2, length.out = vertices.per.corner), 4)
  theta = theta + rep(seq(0, 2*pi, length.out = 5)[-5], each = vertices.per.corner)
  circle = corner.radius*cbind(cos(theta), sin(theta))
  rounded.rectangle = corner.centers.ext + circle

  rotated.rectangle = rotate_polygon(rounded.rectangle, rotation, center)[, c("x", "y")]
  rotated.rectangle = remove_duplicate_vertices(rotated.rectangle)
  return(rotated.rectangle)
}
