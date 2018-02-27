calc_progeny_score = function(expr, ctrl, M, scaling) {
  if (is.matrix(expr)) {
    expr = data.frame(expr)
  }
  # extract intersection of expr and M
  z = intersect(toupper(rownames(expr)), rownames(M))
  expr.matched = expr[match(z, toupper(rownames(expr))), ]
  M.matched = M[match(z, toupper(rownames(M))), ]
  
  if (scaling) {
    m = rowMeans2(as.matrix(expr.matched[, ctrl]))
    s = rowSds(as.matrix(expr.matched[, ctrl]))
    l = lowess(m,s) # Additional step for data transformation to avoid SD=0 (returning NA after scaling)
    
    expr.matched = t(scale(t(expr.matched), center = m, scale=l$y))
  }

  # delete samples in expr where at least one genes is NA
  expr.matched = expr.matched[, colSums(is.na(expr.matched)) == 0]
  
  # if the rownames of expr and M are not totally identical NULL is returned
  if (any(toupper(rownames(expr.matched)) != rownames(M.matched))) {
    return(NULL)
  } else {
    P = data.frame(t(expr.matched) %*% M.matched)
    return(P)
  }
}
