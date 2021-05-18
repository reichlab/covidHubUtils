#' Approximate pairwise Cramér distance with two vectors
#'
#' @param q_F vector containing the quantiles of F
#' @param tau_F vector containing the probability levels corresponding to
#' the quantiles of F. The default value is NULL, but needs to be specified
#' if approx_rule is either "left-sided" or "trapezoid".
#' @param q_G vector containing the quantiles of G
#' @param tau_G vector containing the probability levels corresponding to
#' the quantiles of G. The default value is NULL, but needs to be specified
#' if approx_rule is either "left-sided" or "trapezoid".
#' @param approx_rule a string specifying which formula to use
#' for approximation. Valid rules are "left-sided" and "trapezoid".
#' The default rule is "trapezoid".
#' @return a single value of approximated pairwise Cramér distance
#' between q_F and q_G
#'
#' @export
#'
cd_pairwise <-
  function(q_F,
           q_G,
           approx_rule,
           tau_F = NULL,
           tau_G = NULL) {
    # check conditions
    if (!(approx_rule %in% c("equal_1", "equal_2", "left-sided", "trapezoid"))) {
      stop("invalid approximation rule")
    }
    if (length(q_F) != length(q_G)) {
      stop("q_F and q_G need to be of the same length")
    }
    if (approx_rule %in% c("left-sided", "trapezoid") &&
        (is.null(tau_F) | is.null(tau_G))) {
      stop("tau_F and tau_G are needed for the approximation using the rules for unequal interval")
    }
    # run select rule
    if (approx_rule == "equal_1" | approx_rule == "equal_2") {
      # compute quantile levels from length of provided quantile vectors:
      K <- length(q_F)
      p <-
        (1:K) / (K + 1) # function assumes that the quantile levels are equally spaced
      
      # pool quantiles:
      q0 <- c(q_F, q_G)
      # vector of grouping variables, with 1 for values belonging to F, -1 for values
      # belonging to G
      a0 <- c(rep(1, length(q_F)), rep(-1, length(q_G)))
      
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
      if (approx_rule == "equal_1") {
        cvm <- sum(diffs_q * b * (b + 1)) / (K + 1) / (K)
      } else if (approx_rule == "equal_2") {
        cvm <- sum(diffs_q * b ^ 2 / K ^ 2)
      }
      metric <- mean(cvm)
    }
    else if (approx_rule == "left-sided" |
             approx_rule == "trapezoid") {
      N <- length(q_F)
      M <- length(q_F)
      # pool quantiles:
      q0 <- c(q_F, q_G)
      a0 <- c(rep(1, N), rep(0, M))
      # indicator whether the entry is from F or G
      a <- a0[order(q0)]
      q <- q0[order(q0)]
      tau <- c(tau_F, tau_G)[order(q0)]
      diffs_q <- diff(q)
      # prob level vectors
      tau_F_v <- a * tau
      tau_F_v2 <- sapply(1:(N + M),
                         function(x)
                           ifelse(
                             tau_F_v[x] != 0,
                             tau_F_v[x],
                             ifelse(min(which(tau_F_v != 0)) > 1 &&
                                      x <= min(which(tau_F_v != 0)), 0,
                                    tau_F_v[max(which(tau_F_v[1:x] !=
                                                        0))])
                           ))
      tau_G_v <- abs(a - 1) * tau
      tau_G_v2 <- sapply(1:(N + M),
                         function(x)
                           ifelse(
                             tau_G_v[x] != 0,
                             tau_G_v[x],
                             ifelse(min(which(tau_G_v != 0)) > 1 &&
                                      x <= min(which(tau_G_v != 0)), 0,
                                    tau_G_v[max(which(tau_G_v[1:x] !=
                                                        0))])
                           ))
      if (approx_rule == "left-sided") {
        cvm <-
          sum(((tau_F_v2[1:(N + M) - 1] - tau_G_v2[1:(N + M) - 1]) ^ 2) * diffs_q)
      } else if (approx_rule == "trapezoid") {
        cvm <-
          sum((((tau_F_v2[1:(N + M) - 1] - tau_G_v2[1:(N + M) - 1]) ^ 2 + (tau_F_v2[2:(N +
                                                                                         M)] - tau_G_v2[2:(N + M)]) ^ 2
          ) / 2) * diffs_q)
      }
      metric <- cvm
    }
    return(metric)
  }

#' Approximate pairwise Cramér distance for a single target,
#' date, location data frame
#'
#' @param single_frame a data frame containing correctly-formatted forecasts
#' for a single target, location, target_end_date
#' @param approx_rule a string specifying which formula to use
#' for approximation. Valid rules are "left-sided" and "trapezoid".
#' The default rule is "trapezoid".
#' @return a data frame with approximated pairwise Cramér distances
#' for all possible pairs of models
#'
#' @export
#'
cd_combination <- function(single_frame, end_date, approx_rule) {
  if (approx_rule == "left-sided" |
      approx_rule == "trapezoid") {
    tau_F <- tau_G <- unique(single_frame$quantile)
  } else {
    tau_F <- tau_G <- NULL
  }
  # remove any models with NA values
  single_tarloc <-
    single_frame[, colSums(is.na(single_frame)) == 0] %>%
    dplyr::filter(target_end_date == end_date) %>%
    dplyr::arrange(quantile)
  # pairwise column calculation
  tmp <- single_tarloc %>%
    dplyr::select(
      -c(
        "target_variable",
        "target_end_date",
        "location",
        "type",
        "quantile",
        "horizon",
        "abbreviation"
      )
    )
  nc <- ncol(tmp)
  cnames <- colnames(tmp)
  eg <- expand.grid(1:nc, 1:nc)
  nr <- nrow(eg)
  v <- vector(length = nr)
  for (i in 1:nr) {
    cc <- cd_pairwise(as.numeric(unlist(tmp[, eg[i, 1]])),
                      as.numeric(unlist(tmp[, eg[i, 2]])),
                      approx_rule,
                      tau_F,
                      tau_G)
    v[i] <- cc
  }
  single_tarloc_cvm <- data.frame(
    model_1 = rep(cnames, nc),
    model_2 = rep(cnames, each = nc),
    approx_cd = v,
    target_end_date = end_date
  )
  return(single_tarloc_cvm)
}

#' Approximate pairwise Cramér distance for a filtered and
#' correctly-formatted data frame from pairwise_filter
#'
#' @param forecasts a data frame containing correctly-formatted forecasts
#' for multiple targets, locations, target_end_dates
#' @param approx_rule a string specifying which formula to use
#' for approximation. Valid rules are "left-sided" and "trapezoid".
#' The default rule is "trapezoid".
#' @return a data frame with approximated pairwise Cramér distances
#' for all possible pairs of models
#'
#' @export
#'
approx_distance <- function(forecasts, approx_rule) {
  main_frame <- forecasts %>%
    dplyr::mutate(
      horizon = as.numeric(as.character(horizon)),
      target_variable = as.character(target_variable),
      location = as.character(location),
      target_end_date = as.Date(target_end_date)
    )
  abbrev <- main_frame %>%
    dplyr::select(location, abbreviation) %>%
    dplyr::distinct()
  if ("forecast_date" %in% c(colnames(main_frame))) {
    main_frame <- main_frame %>%
      dplyr::select(-"forecast_date")
  }
  ## apply distance_combination function
  locations <- unique(main_frame$location)
  target_list <- unique(main_frame$target_variable)
  horizon_list <- unique(main_frame$horizon)
  dist_frame <- data.frame()
  for (loc in locations) {
    for (target in target_list) {
      for (horiz in horizon_list) {
        tar_loc <- main_frame %>%
          dplyr::filter(location == loc,
                        target_variable == target,
                        horizon == horiz)
        end_dates <- unique(tar_loc$target_end_date)
        single_tarloc_frame <- purrr::map_dfr(end_dates,
                                              function(date) {
                                                cd_combination(tar_loc, date, approx_rule)
                                              }) %>%
          dplyr::mutate(
            horizon = horiz,
            location = loc,
            target_variable = target
          ) %>%
          dplyr::left_join(abbrev, by = "location")
        rbind(dist_frame, single_tarloc_frame) -> dist_frame
      }
    }
  }
  return(dist_frame)
}