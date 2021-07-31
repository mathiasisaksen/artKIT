#' Create image based on color function
#'
#' This function takes in a function that maps every location over some rectangular
#' area to a color, and saves a PNG image based on the output of the function.
#'
#' @param filename The filename of the output PNG file. It does not need to end
#' in ".png".
#' @param color.function A function that takes in an n x 2 data frame of locations
#' inside the rectangle specified in \code{bounds}, and returns one color per location.
#' The data frame has columns named \code{x} and \code{y}.
#' The function can return the colors in one of the following formats:
#' \itemize{
#'   \item{A vector of n hex codes, e.g. "#FF0000" or "#123456AB" (where "AB" in the latter is alpha / transparency)}
#'   \item{A vector of n greyscale values between 0 and 1, where 0 is black and 1 is white}
#'   \item{An n x 2 matrix of values between 0 and 1, where the first column is the grayscale value and the second is alpha}
#'   \item{An n x 3 matrix of values between 0 and 1, where the columns correspond to R, G and B}
#'   \item{An n x 4 matrix of values between 0 and 1, where the columns correspond to R, G, B and alpha}
#' }
#' @param bounds A four-element vector that specifies the rectangular area used
#' when evaluating \code{color.function}. The first two elements are the lower and
#' upper bounds for the x-coordinate, while the last two are the lower and upper
#' bounds for the y-coordinate.
#' @param width,height The number of columns and rows in the output image, so that
#' the total number of pixels is \code{width*height}.
#' @param num.portions When this parameter is greater than 1, the output image
#' will be divided into \code{num.portions} vertical slices of approximately
#' the same height. This is useful for reducing memory use when \code{width} and
#' \code{height} are large. The slices can be merged into the original image with
#' image editing software.
#'
#' @note The aspect ratio of the output image (\code{width / height}) should be
#' the same as the aspect ratio of \code{bounds}. If not, a warning is issued.
#'
#' @examples
#' \dontrun{
#' # Example with hex values
#' color.function.hex = function(locations) {
#'   colors = c("#df361f", "#21747a", "#edd324")
#'   return(sample(colors, size = nrow(locations), replace = TRUE))
#' }
#'
#' image_from_function("hex.png", color.function.hex,
#'                     width = 1000, height = 1000)
#'
#' # Example with grayscale
#' color.function.gray = function(locations) {
#'   gray = minmax_scaling(locations$x^2 + locations$y^2)
#'   return(gray)
#' }
#'
#' image_from_function("grayscale.png", color.function.gray,
#'                     width = 1000, height = 1000)
#'
#'
#' # Example with RGB matrix
#' color.function.rgb = function(locations) {
#'   red = minmax_scaling(locations$x^2)
#'   green = minmax_scaling(sin(pi*locations$y), 0, 0.5)
#'   blue = minmax_scaling(locations$x*locations$y)
#'   return(cbind(red, green, blue))
#' }
#'
#' image_from_function("rgb.png", color.function.rgb, bounds = c(-2, 2, -1, 1),
#'                     width = 2000, height = 1000)
#'
#' # Example with RGBA matrix
#' color.function.rgba = function(locations) {
#'   red = minmax_scaling(sin(pi*locations$x))
#'   green = minmax_scaling(sin(2*pi*locations$x + 0.25))
#'   blue = minmax_scaling(sin(4*pi*locations$x + 0.5))
#'   alpha = minmax_scaling(sin(6*pi*locations$x + 0.75))
#'   return(cbind(red, green, blue, alpha))
#' }
#'
#' image_from_function("rgba.png", color.function.rgba,
#'                     width = 1000, height = 1000)
#'}
#' @export
#' @author Mathias Isaksen \email{mathiasleanderi@@gmail.com}
image_from_function = function(filename, color.function, bounds = c(-1, 1, -1, 1),  width, height, num.portions = 1) {
  fn.split = strsplit(x = filename, split = "[.]")[[1]]
  if (length(fn.split) > 1 && tolower(fn.split[length(fn.split)]) == "png") {
    filename = substr(filename, 1, nchar(filename) - 4)
  }

  ratio.1 = width/height
  ratio.2 = (bounds[2] - bounds[1])/(bounds[4] - bounds[3])
  if (abs(ratio.1 - ratio.2) > 10^(-5)) {
    warning('The aspect ratio of the output (width/height = ', round(ratio.1, 5), ') is not equal to ',
            'the aspect ratio of bounds (', round(ratio.2, 5), ')')
  }

  if (num.portions < 1) {
    stop("num.portions must be greater than or equal to 1.")
  }

  if (width < 1 || height < 1) {
    stop("Both width and height must be greater than 0.")
  }

  portion.rows = round(seq(0, height, length.out = num.portions+1))
  portion.size = diff(portion.rows)

  height.portions = bounds[3] + portion.rows/height*(bounds[4] - bounds[3])

  for (i in 1:num.portions) {
    num.selected.rows = portion.size[i]
    # Let height.portions[i+1] and height.portions[i] be lower and upper to
    # make y increase upwards in output
    pixel.locations = generate_grid_centers(bounds[1], bounds[2], height.portions[i+1], height.portions[i],
                                            width, num.selected.rows)

    color.values = color.function(pixel.locations)
    color.matrix = handle_color_output(color.values)

    if (any(color.matrix < 0 || color.matrix > 1)) {
      warning("color.function returns colors values outside the interval [0, 1].")
    }

    output.array = array(dim = c(num.selected.rows, width, ncol(color.matrix)))
    for (j in 1:ncol(color.matrix)) {
      output.array[, , j] = matrix(color.matrix[, j], nrow = num.selected.rows, ncol = width, byrow = TRUE)
    }
    current.filename = ifelse(num.portions > 1, paste(filename, i, sep = "-"), filename)
    png::writePNG(image = output.array, target = paste(current.filename, ".png", sep = ""))
  }
}

handle_color_output = function(color_output) {
  dims = dim(color_output)

  if (!is.null(dims)) {
    result = color_output
  } else if (is.numeric(color_output[1])) {
    result = matrix(color_output, ncol = 1)
  } else if (is.character(color_output[1])) {
    result = t(grDevices::col2rgb(color_output, alpha = TRUE)) / 255
  } else {
    stop("Return type from color.function is invalid. See ?image-from-function for ",
         "for valid return formats.")
  }
  return(result)
}
