#' Rounds the corners of a polygon
#'
#' Takes in a single polygon and rounds it by replacing the corner vertices with circular arcs.
#'
#' @inheritParams interpolate_polygon
#' @param corner.radius Determines the corner radius of each circular art. Can be one
#' of the following:
#' \itemize{
#'   \item{A single value, which will be used for every circular arc/corner}
#'   \item{A numeric vector of length nrow(vertex.df), where each value specifies
#'   the corner radius used for the corresponding vertex}
#'   \item{"constant", where every corner uses the same, optimal radius. This is the
#'   largest possible radius while keeping things "nice and smooth"}
#'   \item{"varying", which computes the largest possible corner radius for each vertex}
#' }
#' @param corner.radius.scale A number that each corner radius is multiplied by.
#' Useful when \code{corner.radius} is "constant" or "varying", and you wish to
#' reduce the size of the corner radius values.
#' @param max.vertices.per.corner Controls the number of vertices used in the circular arcs
#' that replace the corners. This depends on how sharp the corner is: the sharper the
#' corner, the more vertices are needed to create a smooth arc.
#'
#' @note If the corner radius values are specified manually, the results will not necessarily
#' look good, and it may take some trial and error. The option "varying" is experimental,
#' and can be bit aggressive. See the example for a comparison between the different
#' options.
#'
#' @return A data frame that contains the x- and y-coordinates of the rounded polygon.
#'
#' @examples
#' # Generate a random polygon based on polar coordinates
#' set.seed(321)
#' n = 20 # Number of vertices
#' theta = rev(seq(0, 2*pi, length.out=n)[-(n + 1)])
#' radius = runif(n, 10, 20)
#' vertex.df = data.frame(x = radius*cos(theta), y = radius*sin(theta))
#' #'
#' # Plot original polygon
#' library(ggplot2)
#' ggplot() + geom_polygon(data = vertex.df, aes(x = x, y = y))+coord_fixed()
#' #'
#' # Every corner is rounded with the same specified radius
#' rounded.1 = round_polygon_corners(vertex.df, corner.radius = 1)
#' ggplot() + geom_polygon(data = rounded.1, aes(x = x, y = y)) + coord_fixed()
#' #'
#' # The corners are rounded with individual, specified radius
#' rounded.2 = round_polygon_corners(vertex.df, corner.radius = runif(n, 0, 1.9))
#' ggplot() + geom_polygon(data = rounded.2, aes(x = x, y = y)) + coord_fixed()
#' #'
#' # Every corner is rounded with the same optimal radius
#' rounded.3 = round_polygon_corners(vertex.df, corner.radius = "constant")
#' ggplot() + geom_polygon(data = rounded.3, aes(x = x, y = y)) + coord_fixed()
#' #'
#' # The corners are rounded with optimal individual radius
#' rounded.4 = round_polygon_corners(vertex.df, corner.radius = "varying")
#' ggplot() + geom_polygon(data = rounded.4, aes(x = x, y = y)) + coord_fixed()
#'
#' # Comparison of different options
#' ggplot()+
#'   geom_polygon(data = rounded.1, aes(x = x, y = y, fill = "1. Constant, manual"))+
#'   geom_polygon(data = rounded.2, aes(x = x + 40, y = y, fill = "2. Varying, manual"))+
#'   geom_polygon(data = rounded.3, aes(x = x, y = y - 40, fill = "3. Constant, optimal"))+
#'   geom_polygon(data = rounded.4, aes(x = x + 40, y = y - 40, fill = "4. Varying, optimal"))+
#'   coord_fixed()
#'
#' # Below we what happens if the corner radius is too large:
#' # The circular arcs are not connected in a smooth way
#' rounded.5 = round_polygon_corners(vertex.df, corner.radius = 4)
#' ggplot() + geom_polygon(data = rounded.5, aes(x = x, y = y)) + coord_fixed()
#'
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
round_polygon_corners = function(vertex.df, corner.radius = "constant", corner.radius.scale = 1, max.vertices.per.corner = 50) {
  if ("group" %in% colnames(vertex.df) && length(unique(vertex.df$group)) > 1) {
    stop("The group column in vertex.df contains more than one unique value. round_polygon_corners ",
         "can only handle one polygon per call.")
  }
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)
  is.ccw = is_polygon_ccw(vertex.df)

  vertex.previous = vertex.df[c(n, 1:(n-1)), c("x", "y")]
  vertex.next = vertex.df[c(2:n, 1), c("x", "y")]

  edge.lengths = sqrt(rowSums((vertex.df[, c("x", "y")] - vertex.next)^2))

  angle.previous = atan2(vertex.previous$y - vertex.df$y, vertex.previous$x - vertex.df$x)
  angle.next = atan2(vertex.next$y - vertex.df$y, vertex.next$x - vertex.df$x)

  # The interior angle is found by taking the difference between
  # the angle between the the current vertex and the previous,
  # and the angle between the current vertex and the next
  if (is.ccw) {
    angle.interior = (angle.previous - angle.next) %% (2*pi)
  } else {
    angle.interior = (angle.next - angle.previous) %% (2*pi)
  }

  # A corner is convex if the interior angle is less than 180 degrees
  is.convex = angle.interior < pi
  # How much of a circle is needed to represent the corner?
  arc.size = ifelse(is.convex, pi - angle.interior, angle.interior - pi)
  # The smallest of angle.interior and 2*pi - angle.interior
  angle.span = ifelse(is.convex, angle.interior, 2*pi - angle.interior)
  # Should the rouding be done inwards (concave) or outwards (convex)?
  offset.direction = ifelse(is.convex, -1, 1)

  if (length(corner.radius) == 1 && corner.radius == "varying") {
    corner.radius = compute_varying_corner_radius(edge.lengths, angle.span)
  } else if (length(corner.radius) == 1 && corner.radius == "constant") {
    corner.radius = compute_constant_corner_radius(edge.lengths, angle.span)
  } else if (is.numeric(corner.radius) && length(corner.radius) == 1) {
    corner.radius = rep(corner.radius, n)
  } else if (!(is.numeric(corner.radius) & length(corner.radius) == n)) {
    stop("corner.radius must be either \"varying\", \"constant\", a single number or a numeric of length nrow(vertex.df)")
  }
  corner.radius = corner.radius.scale*corner.radius
  # How far should the center of the rounding circle be from the vertex?
  offset.amount = corner.radius / sin(angle.span / 2)
  # deform.polygon deals with computing the centers of the rounding circles
  arc.centers = deform_polygon(vertex.df, offset.amount*offset.direction)

  # The angle between the vertices and the circle centers
  vertex.angle = atan2(vertex.df$y - arc.centers$y, vertex.df$x - arc.centers$x)

  # How many vertices should be used in the rounding portion?
  vertices.per.corner = ceiling(max.vertices.per.corner * arc.size / pi)
  # Using an arbitrary lower limit of 3
  vertices.per.corner[vertices.per.corner < 3] = 3

  # Numerics needed for computing rounded polygon
  arc.size.total = rep(arc.size, vertices.per.corner)
  arc.centers.total = arc.centers[rep(1:n, vertices.per.corner), ]
  vertex.angle.total = rep(vertex.angle, vertices.per.corner)
  is.convex.total = rep(is.convex, vertices.per.corner)
  corner.radius.total = rep(corner.radius, vertices.per.corner)

  total.vertices = sum(vertices.per.corner)

  # Gives the polar coordinate angles for every rounded corner, going from 0 to arc.size
  arc.basis = arc.size.total*(0:(total.vertices - 1) - rep(cumsum(c(0, vertices.per.corner))[1:n], vertices.per.corner)) / rep(vertices.per.corner - 1, vertices.per.corner)
  # Offsets so that the angles are symmetric around 0 (going from -arc.size / 2 to arc.size / 2)
  arc.symmetric = arc.basis - arc.size.total / 2
  # Flip direction if polygon is not ccw
  arc.symmetric = if(is.ccw) arc.symmetric else -arc.symmetric
  # Flip direction of concave corners, as these corners are rounded inwards
  arc.symmetric[!is.convex.total] = -arc.symmetric[!is.convex.total]
  # Rotate according to position of circle center relative to vertex position
  arc.angles = arc.symmetric + vertex.angle.total

  arc.points = corner.radius.total * data.frame(x = cos(arc.angles), y = sin(arc.angles))
  rounded.df = data.frame(
    x = arc.centers.total$x + arc.points$x,
    y = arc.centers.total$y + arc.points$y
  )
  # Remove consecutive vertices in the same location
  rounded.df = remove_duplicate_vertices(rounded.df)

  return(rounded.df)
}
