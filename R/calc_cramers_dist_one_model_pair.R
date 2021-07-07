#' Calculate the approximated Cramer’s distance between a pair of distributions
#' F and G that are represented by a collection of quantiles using a specific
#' approximation rule.
#'
#' @param q_F vector containing the quantiles of F
#' @param tau_F vector containing the probability levels corresponding to
#' the quantiles of F.
#' @param q_G vector containing the quantiles of G
#' @param tau_G vector containing the probability levels corresponding to
#' the quantiles of G.
#' @param approx_rule string specifying which formula to use
#' for approximation. Valid rules are "approximation1", "approximation2",
#' "left_sided_riemann", and "trapezoid_riemann". See Details for more
#' information.
#' @return a single value of approximated pairwise Cramér distance
#' between q_F and q_G
#' @details This function calculate the aprroximated Cramer’s distance
#' between a pair of distributions F and G that are represented by
#' a collection of quantiles using a specified approximation rule.
#' Specifying "approximation1" or "approximation2" as `approx_rule`
#' requires the two vectors of quantiles to be of equal length.
#' These approximation methods are formulated based on equally-spaced
#' probability levels. The approximation formula for "approximation1" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{K(K+1)}\sum^{2K-1}_{i=1}b_i(b_i+1)(q_{i+1}-q_i)\right }{ (non-Latex version) }
#' and the approximation formula for "approximation2" is
#' \deqn{ \text{CD}(F,G) \approx \left\{\frac{1}{(K+1)^2}\sum^{2K-1}_{i=1}b_i^2(q_{i+1}-q_i)\right. }{ (non-Latex version) }
#' where \eqn{q_i} is an element in a vector of an ordered pooled quantiles
#' of `q_F` and `q_G` and \eqn{b_i} is an element of a vector of the absolute
#' values of cumulative sums of \eqn{\mathbf{a}}, whose element is 1 if
#' \eqn{q_i} is a quantile of F or -1 if \eqn{q_i} is a quantile of G.
#'
#' Specifying "left_sided_riemann" or "trapezoid_riemann" as `approx_rule`
#' accommodates cases when the lengths of `q_F` and `q_G` are not equal
#' and when `tau_F` and `tau_G` are not equal. A Riemann sum  approach is
#' used to approximate a pairwise Cramér distance.
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
#' calc_cramers_dist_one_model_pair(f_vector,tau_F_vector,g_vector,tau_G_vector,"left_sided_riemann")
#' @export
#'
#'
calc_cramers_dist_one_model_pair <-
  function(q_F, tau_F, q_G, tau_G, approx_rule) {
    # check rules
    if (length(approx_rule) != 1) {
      stop("only one approximation rule can be specified")
    }
    if (!(
      approx_rule %in% c(
        "approximation1",
        "approximation2",
        "left_sided_riemann",
        "trapezoid_riemann"
      )
    )) {
      stop("invalid approximation rule")
    }
    # assign a helper function
    if (approx_rule %in% c("approximation1", "approximation2")) {
      cvm <-
        calc_cramers_dist_equal_space(q_F, tau_F, q_G, tau_G, approx_rule)
    }  else {
      cvm <-
        calc_cramers_dist_unequal_space(q_F, tau_F, q_G, tau_G, approx_rule)
    }
    return(cvm)
  }