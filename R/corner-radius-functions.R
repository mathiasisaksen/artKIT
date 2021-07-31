
compute_varying_corner_radius = function(edge.lengths, angles) {
  n = length(edge.lengths)

  # Linear programming with solution (l[1],..., l[n], Z).
  # Using L = edge.lengths, we need l[i] + l[i+1] <= L[i] and l[i] > Z, where Z
  # is a dummy variable that ensures that min(l) is as large as possible.
  # The first constraint leads to A.top, while the latter leads to A.bottom.
  A.top = diag(1, nrow = n, ncol = n + 1)
  A.top[cbind(1:(n - 1), 2:n)] = 1
  A.top[n, 1] = 1
  A.bottom = diag(-1, nrow = n, ncol = n + 1)
  A.bottom[, n + 1] = 1
  A = rbind(A.top, A.bottom)

  # Give equal weight to all variables
  obj = c(rep(1, n), 1)
  #Gives more weight where the angle is big, needs to be explored further
  #obj = c(pi - angles/2, 1)
  # All inequalities are less-than-or-equal
  dir = rep("<=", nrow(A))
  # The RHS of the first constraint is edge.lengths, while the second
  # constraint has RHS equal to 0
  rhs = c(edge.lengths, rep(0, n))

  lp.sol = lpSolve::lp("max", obj, A, dir, rhs)
  l = lp.sol$solution[1:n]
  corner.radius = l*tan(angles / 2)
  return(corner.radius)
}

compute_constant_corner_radius = function(edge.lengths, angles) {
  n = length(edge.lengths)
  corner.radius = min(edge.lengths / (1/tan(angles/2) + 1/tan(angles[c(2:n, 1)]/2)))
  return(rep(corner.radius, n))
}
