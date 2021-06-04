#' Approximate the Cramer’s distance between a pair of distributions
#' F and G that are represented by a collection of equally-spaced quantiles.
#'
#' @param q_F vector containing the quantiles of F
#' @param tau_F vector containing the probability levels corresponding to
#' the quantiles of F.
#' @param q_G vector containing the quantiles of G
#' @param tau_G vector containing the probability levels corresponding to
#' the quantiles of G.
#' @param approx_rule string specifying which formula to use
#' for approximation. Valid rules are "approximation1" and
#' "approximation2".
#' See Details for more information.
#' @return a single value of approximated pairwise Cramér distance
#' between q_F and q_G
#' @details This function requires the two vectors of quantiles to be
#' of equal length. The approximation methods are formulated based on
#' two collections of quantiles corresponding to equally-spaced
#' probability levels. The approximation formula for "approximation1" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{K(K+1)}\sum^{2K-1}_{i=1}b_i(b_i+1)(q_{i+1}-q_i)\right }{ (non-Latex version) }
#' and the approximation formula for "approximation2" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{(K+1)^2}\sum^{2K-1}_{i=1}b_i^2(q_{i+1}-q_i)\right. }{ (non-Latex version) }
#' where \eqn{q_i} is an element in a vector of an ordered pooled quantiles
#' of `q_F` and `q_G` and \eqn{b_i} is an element of a vector of the absolute
#' values of cumulative sums of \eqn{\mathbf{a}}, whose element is 1 if
#' \eqn{q_i} is a quantile of F or -1 if \eqn{q_i} is a quantile of G.
#' @example
#' f_vector <- 1:9
#' tau_F_vector <- tau_G_vector <- seq(0.1,0.9,0.1)
#' g_vector <- seq(2,18,2)
#' calc_cramers_dist_equal_space(f_vector,tau_F_vector,g_vector,tau_G_vector, "approximation1")
#' @export
#'
#'
calc_cramers_dist_equal_space <-
  function(q_F, tau_F, q_G, tau_G, approx_rule) {
    # check rules
    if (!(approx_rule %in% c("approximation1", "approximation2"))) {
      stop("invalid approximation rule")
    }
    # check probability level order
    tau_F_ordered <- sort(tau_F)
    tau_G_ordered <- sort(tau_G)
    if (sum(tau_F != tau_F_ordered) > 0) {
      warning("tau_F has been sorted to in an increasing order")
    }
    if (sum(tau_G != tau_G_ordered) > 0) {
      warning("tau_G has been sorted to in an increasing order")
    }
    # check quantile order
    q_F_ordered <- sort(q_F)
    q_G_ordered <- sort(q_G)
    if (sum(q_F != q_F_ordered) > 0) {
      warning("q_F has been re-ordered to correspond to increasing probability levels")
    }
    if (sum(q_G != q_G_ordered) > 0) {
      warning("q_G has been re-ordered to correspond to increasing probability levels")
    }
    # check conditions
    if (length(q_F_ordered) != length(q_G_ordered)) {
      stop("q_F_ordered and q_G_ordered need to be of the same length")
    }
    if (length(tau_F_ordered) != length(tau_G_ordered)) {
      stop("tau_F_ordered and tau_G_ordered need to be of the same length")
    }
    if (sum(tau_F_ordered == tau_G_ordered) != length(tau_F_ordered)) {
      stop("tau_F_ordered and tau_G_ordered need to have the same values")
    }
    if (length(q_F_ordered) != length(tau_F_ordered)) {
      stop("The lengths of q_F_ordered and tau_F_ordered need to be equal")
    }
    if (length(q_G_ordered) != length(tau_G_ordered)) {
      stop("The lengths of q_G_ordered and tau_G_ordered need to be equal")
    }
    if (sum(tau_F_ordered <= 1) != length(tau_F_ordered) |
        sum(tau_F_ordered >= 0) != length(tau_F_ordered) |
        sum(tau_G_ordered <= 1) != length(tau_G_ordered) |
        sum(tau_G_ordered >= 0) != length(tau_G_ordered)) {
      stop("The values of tau_F_ordered and tau_G_ordered have to be between 0 and 1")
    }
    if (isTRUE(all.equal((range(diff(tau_F_ordered))/mean(diff(tau_F_ordered)))[1], 
                         (range(diff(tau_F_ordered))/mean(diff(tau_F_ordered)))[2],.Machine$double.eps ^ 0.5))==FALSE|
        isTRUE(all.equal((range(diff(tau_G_ordered))/mean(diff(tau_G_ordered)))[1], 
                          (range(diff(tau_G_ordered))/mean(diff(tau_G_ordered)))[2],.Machine$double.eps ^ 0.5))==FALSE) {
      warning(
        "tau_F_ordered and tau_G_ordered are not equaly-spaced, this approximation rule is based on equally-spaced probability levels"
      )
    }
    # run select rule
    # compute quantile levels from length of provided quantile vectors:
    K <- length(q_F_ordered)
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
    if (approx_rule == "approximation1") {
      cvm <- sum(diffs_q * b * (b + 1)) / ((K + 1) * (K))
    } else if (approx_rule == "approximation2") {
      cvm <- sum(diffs_q * b ^ 2 / (K+1) ^ 2)
    }
    return(cvm)
  }