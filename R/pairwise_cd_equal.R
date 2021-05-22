#' Approximate a pairwise Cramér distance between
#' two equally-spaced vectors of quantiles
#'
#' @param q_F vector containing the quantiles of F
#' @param q_G vector containing the quantiles of G
#' @param approx_rule string specifying which formula to use
#' for approximation. Valid rules are "first_rule" and "second_rule".
#' See Details for more information.
#' @return a single value of approximated pairwise Cramér distance
#' between q_F and q_G
#' @details This is function requires the two vectors of quantiles to be
#' of equal length. The approximation methods are formulated based on
#' two sets of quantiles corresponding to equally-spaced probability levels.
#' The approximation formula for "first_rule" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{K(K+1)}\sum^{2K-1}_{i=1}b_i(b_i+1)(q_{i+1}-q_i)\right }{ (non-Latex version) }
#' and the approximation formula for "second_rule" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{(K+1)^2}\sum^{2K-1}_{i=1}b_i^2(q_{i+1}-q_i)\right. }{ (non-Latex version) }
#' where \eqn{q_i} is an element in a vector of an ordered pooled quantiles
#' of `q_F` and `q_G` and \eqn{b_i} is an element of a vector of the absolute
#' values of cumulative sums of \eqn{\mathbf{a}}, whose element is 1 if
#' \eqn{q_i} is a quantile of F or -1 if \eqn{q_i} is a quantile of G.
#' @example
#' f_vector <- 1:10
#' g_vector <- seq(2,20,2)
#' pairwise_cd_equal(f_vector,g_vector,"second_rule")
#' @export
#'
pairwise_cd_equal <- function(q_F, q_G, approx_rule) {
  # check conditions
  if (!(approx_rule %in% c("first_rule", "second_rule"))) {
    stop("invalid approximation rule")
  }
  if (length(q_F) != length(q_G)) {
    stop("q_F and q_G need to be of the same length")
  }
  # check quantile order
  q_F_ordered <- sort(q_F)
  q_G_ordered <- sort(q_G)
  if (sum(q_F != q_F_ordered)>0) {
    warning("q_F has been re-ordered to correspond to increasing probability levels")
  }
  if (sum(q_G != q_G_ordered)>0) {
    warning("q_G has been re-ordered to correspond to increasing probability levels")
  }
  
  # run select rule
  # compute quantile levels from length of provided quantile vectors:
  K <- length(q_F_ordered)
  p <-
    (1:K) / (K + 1) # function assumes that the quantile levels are equally spaced
  
  # pool quantiles:
  q0 <- c(q_F_ordered, q_G_ordered)
  # vector of grouping variables, with 1 for values belonging to F, -1 for values
  # belonging to G
  a0 <-
    c(rep(1, length(q_F_ordered)), rep(-1, length(q_G_ordered)))
  
  # re-order both vectors:
  q <- q0[order(q0)]
  a <- a0[order(q0)]
  # and compute "how many quantiles ahead" F or G is at a given segment:
  b <- abs(cumsum(a))
  
  # compute the lengths of segments defined by sorted quantiles:
  diffs_q <-
    c(diff(q), 0) # zero necessary for indexing below, but we could put
  # anything (gets multiplied w zero)
  
  # and approximate CD
  if (approx_rule == "first_rule") {
    cvm <- sum(diffs_q * b * (b + 1)) / (K + 1) / (K)
  } else if (approx_rule == "second_rule") {
    cvm <- sum(diffs_q * b ^ 2 / K ^ 2)
  }
  return(cvm)
}