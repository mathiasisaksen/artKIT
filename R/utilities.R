
generate_grid_centers = function(x.lower, x.upper, y.lower, y.upper, resolution.x, resolution.y, params, separate = FALSE) {
  if (!missing(params)) {
    # Unpack parameters to function scope
    list2env(params, environment())
  }
  h.x = (x.upper-x.lower)/resolution.x
  h.y = (y.upper-y.lower)/resolution.y
  x.grid = seq(x.lower+h.x/2, x.upper-h.x/2, length.out = resolution.x)
  y.grid = seq(y.lower+h.y/2, y.upper-h.y/2, length.out = resolution.y)
  if (separate) {
    return(list(x = x.grid, y = y.grid))
  } else {
    s = expand.grid(x.grid, y.grid)
    colnames(s) = c("x", "y")
    return(s)
  }
}

gaussian_kernel = function(x, sd = 1) {
  exp(-(x/sd)^2)
}

contains_columns = function(input.df, column.names) {
  all(sapply(column.names, function(name) name %in% colnames(input.df)))
}

check_vertex_df = function(vertex.df, df.name = "vertex.df") {
  if (!contains_columns(vertex.df, c("x", "y"))) {
    stop(df.name, " must contain columns \"x\" and \"y\".")
  }
}

is_polygon_ccw = function(vertex.df) {
  check_vertex_df(vertex.df)
  n = nrow(vertex.df)

  x.min = min(vertex.df$x)
  pivot.ind = which(vertex.df$x == x.min)
  if (length(pivot.ind) > 1) {
    # If there are ties for vertices with smallest x-coordinate, use the one
    # with the largest y-coordinate
    pivot.ind = pivot.ind[which.max(vertex.df$y[pivot.ind])]
  }
  A = vertex.df[ifelse(pivot.ind - 1 == 0, n, pivot.ind - 1), ]
  B = vertex.df[pivot.ind, ]
  C = vertex.df[ifelse(pivot.ind + 1 == n + 1, 1, pivot.ind + 1), ]

  det = (B$x - A$x)*(C$y - A$y) - (C$x - A$x)*(B$y - A$y)
  return(det > 0)
}

# Inefficient, should be improved
closest_pair_of_points = function(points.1, points.2) {
  n.1 = nrow(points.1)
  n.2 = nrow(points.2)

  repeat.1 = points.1[rep(1:n.1, each = n.2), ]
  repeat.2 = points.2[rep(1:n.2, n.1), ]

  diff = repeat.1 - repeat.2
  dist2 = rep(0, nrow(diff))
  for (i in 1:ncol(diff)) {
    dist2 = dist2 + diff[, i]^2
  }
  closest.index = unname(which.min(dist2))
  index.1 = ceiling(closest.index / n.2)
  index.2 = (closest.index - 1) %% n.2 + 1
  return(c(index.1, index.2))
}

# Inefficient, should be improved
multiple_closest_values = function(values.1, values.2, num.closest = 1) {
  n.1 = length(values.1)
  n.2 = length(values.2)
  if (num.closest > n.1 || num.closest > n.2) {
    stop("num.closest cannot be greater than the length either values.1 or values.2.")
  }

  matrix.1 = matrix(values.1, nrow = n.1, ncol = n.2, byrow = FALSE)
  matrix.2 = matrix(values.2, nrow = n.1, ncol = n.2, byrow = TRUE)

  diff = abs(matrix.1 - matrix.2)
  col.names = 1:n.2
  row.names = 1:n.1
  index.matrix = data.frame(index.1 = rep(NA, num.closest), index.2 = rep(NA, num.closest))

  for (i in 1:num.closest) {
    closest.indices = arrayInd(which.min(diff), dim(diff))
    index.matrix[i, 1] = row.names[closest.indices[1]]
    index.matrix[i, 2] = col.names[closest.indices[2]]
    if (nrow(diff) == 2 || ncol(diff) == 2) {
      diff = matrix(diff[-closest.indices[1], -closest.indices[2]],
                    nrow = nrow(diff) - 1, ncol = ncol(diff) - 1, byrow = nrow(diff) == 2)
    } else {
      diff = diff[-closest.indices[1], -closest.indices[2]]
    }

    row.names = row.names[-closest.indices[1]]
    col.names = col.names[-closest.indices[2]]
  }
  return(index.matrix)
}

remove_duplicate_vertices = function(vertex.df, min.distance = 1e-14) {
  vertex.distance2 = rowSums((vertex.df-vertex.df[c(2:nrow(vertex.df), 1), ])^2)
  vertex.df = vertex.df[vertex.distance2 > min.distance^2, ]
  return(vertex.df)
}
