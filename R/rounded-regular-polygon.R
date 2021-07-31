#' Generate regular polygon with rounded corners
#'
#' This function computes the vertices of a regular polygon with rounded corners.
#'
#' @param num.edges The number of edges that the regular polygon contains.
#' @param center The center/centroid of the shape. Can either be a 1 x 2 data
#' frame/matrix or a numeric vector length 2.
#' @param radius The radius of the circumscribed circle of the regular polygon.
#' @param rotation The amount of rotation applied to the shape, in radians.
#' @param corner.radius The radius of the circular arcs.
#' @param vertices.per.corner The number of vertices that each circular arc consists of.
#'
#' @note The result may contain less than \code{num.edges*vertices.per.corner} vertices.
#' In certain situations, consecutive vertices will end up having the same coordinates.
#' The duplicates are then removed.
#'
#' @return A data frame containing the columns \code{x} and \code{y}, where each
#' row is a vertex in the rounded polygon.
#'
#' @examples
#' # Create a rounded hexagon
#' vertex.df = rounded_regular_polygon(
#'   num.edges = 6,
#'   center = c(5, 6),
#'   radius = 2,
#'   rotation = pi/4,
#'   corner.radius = 0.5
#' )
#'
#' # How the rounded hexagon looks:
#' library(ggplot2)
#' ggplot()+
#'   geom_polygon(data = vertex.df, aes(x = x, y = y))+
#'   coord_fixed()
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
rounded_regular_polygon = function(num.edges = 3, center = c(0, 0), radius = 1, rotation = 0, corner.radius = 0.25, vertices.per.corner = 50) {
  interior.angle = (num.edges - 2) / num.edges * pi
  corner.centers = compute_regular_polygons(center, radius, -pi/2, num.edges, include.info = FALSE)[, c("x", "y")]
  corner.centers = deform_polygon(corner.centers, -corner.radius/sin(interior.angle / 2))
  corner.centers.total = corner.centers[rep(1:num.edges, each = vertices.per.corner), ]

  theta.arc = seq(-pi/num.edges, pi/num.edges, length.out = vertices.per.corner)
  theta.offset = seq(0, 2*pi, length.out = num.edges + 1)[-(num.edges + 1)]
  theta.arc.total = rep(theta.arc, num.edges) + rep(theta.offset, each = vertices.per.corner)

  rounded.parts = corner.radius*cbind(cos(theta.arc.total), sin(theta.arc.total))
  rounded.polygon = corner.centers.total + rounded.parts

  # Offset by pi/2 to move first vertex to top when rotation = 0
  rotation = pi/2 + rotation
  rotated.polygon = rotate_polygon(rounded.polygon, rotation, center.of.rotation = center)[, c("x", "y")]
  return(rotated.polygon)
}
