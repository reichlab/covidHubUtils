#' Approximate a pairwise Cramér distance between
#' two unequally-spaced vectors of quantiles
#'
#' @param q_F vector containing the quantiles of F
#' @param tau_F vector containing the probability levels corresponding to
#' the quantiles of F.
#' @param q_G vector containing the quantiles of G
#' @param tau_G vector containing the probability levels corresponding to
#' the quantiles of G.
#' @param approx_rule string specifying which formula to use
#' for approximation. Valid rules are "left_sided" and "trapezoid".
#' See Details for more information.
#' @return a single value of approximated pairwise Cramér distance
#' between q_F and q_G
#' @details This is function can accommodate cases when the lengths of
#' `q_F` and `q_G` are not equal and when `tau_F` and `tau_G` are not equal.
#' A Riemann sum is used to approximate a pairwise Cramér distance.
#' The approximation formula for "left_sided", referring to a left-sided rule is
#' \deqn{ \text{CD}(F,G) \approx \left\{\sum^{2K-1}_{j=1}(\tau^F_j-\tau^G_j)^2(q_{i+1}-q_i)\right }{ (non-Latex version) }
#' and "trapezoid", referring to a trapezoidal rule is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{(K+1)^2}\sum^{2K-1}_{i=1}\frac{(\tau^F_j-\tau^G_j)^2+(\tau^F_{j+1}-\tau^G_{j+1})^2}{2}(q_{i+1}-q_i)\right. }{ (non-Latex version) }
#' where \eqn{q_i} is an element in a vector of an ordered pooled quantiles
#' of `q_F` and `q_G` and \eqn{\tau^F_j} and \eqn{\tau^G_j} are defined as
#' the probability level of a quantile in `q_F` when \eqn{q_i} comes from \eqn{F} and
#' the probability level of a quantile in `q_G` when \eqn{q_i} comes from \eqn{G},
#' respectively.
#' @example
#' f_vector <- 1:9
#' tauF_vector <- tauG_vector <- seq(0.1,0.9,0.1)
#' g_vector <- seq(4,20,2)
#' pairwise_cd_unequal(f_vector,tauF_vector,g_vector,tauG_vector,"left-sided")
#' @export
#'
#'
pairwise_cd_unequal <-
  function(q_F, tau_F, q_G, tau_G, approx_rule) {
    # check conditions
    if (!(approx_rule %in% c("left-sided", "trapezoid"))) {
      stop("invalid approximation rule")
    }
    if (length(q_F) != length(tau_F)) {
      stop("The lengths of q_F and tau_F need to be equal")
    }
    if (length(q_G) != length(tau_G)) {
      stop("The lengths of q_G and tau_G need to be equal")
    }
    if (sum(tau_F<=1)!=length(tau_F)|sum(tau_F>=0)!=length(tau_F)) {
      stop("The values of tau_F have to be between 0 and 1")
    }
    if (sum(tau_G<=1)!=length(tau_G)|sum(tau_G>=0)!=length(tau_G)) {
      stop("The values of tau_G have to be between 0 and 1")
    }
    if (length(q_F) != length(q_G)) {
      warning("The lengths of q_F and q_G are not equal")
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
    N <- length(q_F)
    M <- length(q_G)
    # pool quantiles:
    q0 <- c(q_F, q_G)
    a0 <- c(rep(1, N), rep(0, M))
    # indicator whether the entry is from F or G
    a <- a0[order(q0)]
    q <- q0[order(q0)]
    tau <- c(tau_F, tau_G)[order(q0)]
    diffs_q <- diff(q)
    # probability level vectors
    tau_F_v <- a * tau
    tau_F_v2 <- sapply(1:(N + M),
                       function(x)
                         ifelse(tau_F_v[x] != 0,
                                tau_F_v[x],
                                ifelse(
                                  min(which(tau_F_v != 0)) > 1 &&
                                    x <= min(which(tau_F_v != 0)), 0,
                                  tau_F_v[max(which(tau_F_v[1:x] !=
                                                      0))]
                                )))
    tau_G_v <- abs(a - 1) * tau
    tau_G_v2 <- sapply(1:(N + M),
                       function(x)
                         ifelse(tau_G_v[x] != 0,
                                tau_G_v[x],
                                ifelse(
                                  min(which(tau_G_v != 0)) > 1 &&
                                    x <= min(which(tau_G_v != 0)), 0,
                                  tau_G_v[max(which(tau_G_v[1:x] !=
                                                      0))]
                                )))
    if (approx_rule == "left-sided") {
      cvm <-
        sum(((tau_F_v2[1:(N + M) - 1] - tau_G_v2[1:(N + M) - 1]) ^ 2) * diffs_q)
    } else if (approx_rule == "trapezoid") {
      cvm <-
        sum((((tau_F_v2[1:(N + M) - 1] - tau_G_v2[1:(N + M) - 1]) ^ 2 + (tau_F_v2[2:(N +
                                                                                       M)] - tau_G_v2[2:(N + M)]) ^ 2
        ) / 2) * diffs_q)
    }
    return(cvm)
  }