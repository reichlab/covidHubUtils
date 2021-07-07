#' Approximate the Cramer’s distance between a pair of distributions
#' F and G that are represented by a collection of unequally-spaced quantiles.
#'
#' @param q_F vector containing the quantiles of F
#' @param tau_F vector containing the probability levels corresponding to
#' the quantiles of F.
#' @param q_G vector containing the quantiles of G
#' @param tau_G vector containing the probability levels corresponding to
#' the quantiles of G.
#' @param approx_rule string specifying which formula to use
#' for approximation. Valid rules are "left_sided_riemann" and 
#' "trapezoid_riemann". See Details for more information.
#' @return a single value of approximated pairwise Cramér distance
#' between q_F and q_G
#' @details This function accommodates cases when the lengths of
#' `q_F` and `q_G` are not equal and when `tau_F` and `tau_G` are not equal.
#' A Riemann sum is used to approximate a pairwise Cramér distance.
#' The approximation formula for "left_sided_riemann" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\sum^{2K-1}_{j=1}(\tau^F_j-\tau^G_j)^2(q_{i+1}-q_i)\right }{ (non-Latex version) }
#' and the approximation formula for "trapezoid_riemann" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{(K+1)^2}\sum^{2K-1}_{i=1}\frac{(\tau^F_j-\tau^G_j)^2+(\tau^F_{j+1}-\tau^G_{j+1})^2}{2}(q_{i+1}-q_i)\right. }{ (non-Latex version) }
#' where \eqn{q_i} is an element in a vector of an ordered pooled quantiles
#' of `q_F` and `q_G` and \eqn{\tau^F_j} and \eqn{\tau^G_j} are defined as
#' the probability level of a quantile in `q_F` when \eqn{q_i} comes from \eqn{F} and
#' the probability level of a quantile in `q_G` when \eqn{q_i} comes from \eqn{G},
#' respectively.
#' @example
#' f_vector <- 1:9
#' tau_F_vector <- tau_G_vector <- seq(0.1,0.9,0.1)
#' g_vector <- seq(4,20,2)
#' calc_cramers_dist_unequal_space(f_vector,tau_F_vector,g_vector,tau_G_vector,"left_sided_riemann")
#' @export
#'
#'
calc_cramers_dist_unequal_space <-
  function(q_F, tau_F, q_G, tau_G, approx_rule) {
    # check rules
    if (!(approx_rule %in% c("left_sided_riemann", "trapezoid_riemann"))) {
      stop("invalid approximation rule")
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
    # check probability level order
    tau_F_ordered <- sort(tau_F)
    tau_G_ordered <- sort(tau_G)
    if (sum(tau_F != tau_F_ordered)>0) {
      warning("tau_F has been sorted to in an increasing order")
    }
    if (sum(tau_G != tau_G_ordered)>0) {
      warning("tau_G has been sorted to in an increasing order")
    }
    # check conditions
    if (length(q_F_ordered) != length(tau_F_ordered)) {
      stop("The lengths of q_F_ordered and tau_F_ordered need to be equal")
    }
    if (length(q_G_ordered) != length(tau_G_ordered)) {
      stop("The lengths of q_G_ordered and tau_G_ordered need to be equal")
    }
    if (sum(tau_F_ordered<=1)!=length(tau_F_ordered)|sum(tau_F_ordered>=0)!=length(tau_F_ordered)) {
      stop("The values of tau_F_ordered have to be between 0 and 1")
    }
    if (sum(tau_G_ordered<=1)!=length(tau_G_ordered)|sum(tau_G_ordered>=0)!=length(tau_G_ordered)) {
      stop("The values of tau_G_ordered have to be between 0 and 1")
    }
    if (length(q_F_ordered) != length(q_G_ordered)) {
      message("The lengths of q_F_ordered and q_G_ordered are not equal")
    }
    N <- length(q_F_ordered)
    M <- length(q_G_ordered)
    # pool quantiles:
    q0 <- c(q_F_ordered, q_G_ordered)
    # indicator whether the entry is from F or G
    q <- q0[order(q0)]
    tf <- unlist(sapply(q, function(x) ifelse(x %in% q_F_ordered,tau_F_ordered[which(x == q_F_ordered)],0)))
    tg <- unlist(sapply(q, function(x) ifelse(x %in% q_G_ordered,tau_G_ordered[which(x == q_G_ordered)],0)))
    diffs_q <- diff(q)
    # probability level vectors
    tau_F_v <- cummax(tf)
    tau_G_v <- cummax(tg)
    if (approx_rule == "left_sided_riemann") {
      cvm <-
        sum(((tau_F_v[1:(N + M) - 1] - tau_G_v[1:(N + M) - 1]) ^ 2) * diffs_q)
    } else if (approx_rule == "trapezoid_riemann") {
      cvm <-
        sum((((tau_F_v[1:(N + M) - 1] - tau_G_v[1:(N + M) - 1]) ^ 2 + (tau_F_v[2:(N +
                                                                                       M)] - tau_G_v[2:(N + M)]) ^ 2
        ) / 2) * diffs_q)
    }
    return(cvm)
  }