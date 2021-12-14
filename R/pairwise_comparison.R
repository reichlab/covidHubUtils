# function for pairwise comparison of models
pairwise_comparison_NA <- function(scores, mx, my, subset = rep(TRUE, nrow(scores)),
                                   permutation_test = FALSE){
  
  
  scores <- scores %>% 
    mutate(centralized_forecast_date = 
             as.Date(calc_target_week_end_date(forecast_date, 1))-5)
  
  # subsets of available scores for both models:
  subx <- subset(scores, model == mx)
  suby <- subset(scores, model == my)
  # merge together and restrict to overlap:
  sub <- merge(subx, suby, by = 
                 c("centralized_forecast_date", "location", "horizon"),
               all.x = FALSE, all.y = FALSE)
  ##### catch common problems:
  ##### no overlap between targets covered by x and y:
  if(nrow(sub) == 0){
    warning("No overlap of covered forecast targets for ", mx, "and", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  ##### unavailable scores (likely because a model issues only point forecasts?)
  if(any(is.na(subx$wis))){
    warning("Some or all wis values are NA for ", mx, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  if(any(is.na(suby$wis))){
    warning("Some or all wis values are NA for ", my, ". Returning NA.")
    return(list(ratio = NA, pval = NA, pval_fcd = NA, mx = mx, my = my))
  }
  # compute ratio:
  ratio <- sum(sub$wis.x) / sum(sub$wis.y)
  # perform permutation tests:
  if(permutation_test){
    pval <- surveillance::permutationTest(sub$wis.x, sub$wis.y,
                            nPermutation = 999)$pVal.permut
    ##### aggregate by forecast date:
    sub_fcd <- aggregate(cbind(wis.x, wis.y) ~ centralized_forecast_date, data = sub, FUN = mean)
    # catch error if too many observations
    if(nrow(sub_fcd) > 5){
      pval_fcd <- surveillance::permutationTest(sub_fcd$wis.x, sub_fcd$wis.y,
                                  nPermutation = 999)$pVal.permut
    }else{
      warning("Too few observations to compute p-value for ", mx, " and ", my, " with aggregation by forecast date. Returning NA.")
      pval_fcd <- NA
    }
  }else{
    pval <- NULL
    pval_fcd <- NULL
  }
  return(list(ratio = ratio, pval = pval, pval_fcd = pval_fcd, mx = mx, my = my))
}
