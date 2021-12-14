
compare_forecasts <- function(scores, baseline="COVIDhub-baseline", 
                              permutation_test=FALSE){
  # select relevant columns:
  scores <- scores %>% 
    # mutate(centralized_forecast_date = 
    #          as.Date(calc_target_week_end_date(forecast_date, 1))-5) %>% 
    select("model", "forecast_date", "location", 
           "horizon", "abs_error", "wis") %>% 
    droplevels()
  
  # the included models and locations:
  models <- unique(scores$model)
  locations <- unique(scores$location)
  #location_names <- unique(scores$location_name)
  
  
  results_ratio <- results_pval <- results_pval_fcd <- matrix(ncol = length(models),
                                                              nrow = length(models),
                                                              dimnames = list(models, models))
  
  
  set.seed(123) # set seed for permutation tests
  
  for(mx in seq_along(models)){
    for(my in 1:mx){
      pwc <- pairwise_comparison_NA(scores = scores, mx = models[mx], my = models[my],
                                    permutation_test = permutation_test)
      results_ratio[mx, my] <- pwc$ratio
      results_ratio[my, mx] <- 1/pwc$ratio
      
      if (permutation_test) {
        results_pval[mx,my] <- results_pval[my,mx] <- pwc$pval
        results_pval_fcd[mx,my] <- results_pval_fcd[my,mx] <- pwc$pval_fcd
        
      }
      
    }
  }
  
  
  ind_baseline <- which(rownames(results_ratio) == baseline)
  geom_mean_ratios <- exp(rowMeans(log(results_ratio[, -ind_baseline]), na.rm = TRUE))
  ratios_baseline <- results_ratio[, baseline]
  ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios[baseline]
  
  if (permutation_test){
    pval <- results_pval[,baseline]
    pval_fcd <- results_pval_fcd[,baseline]
    
    tab <- data.frame(model = names(geom_mean_ratios),
                    geom_mean_ratios = geom_mean_ratios,
                    ratios_baseline = ratios_baseline,
                    ratios_baseline2 = ratios_baseline2,
                    pval = pval,
                    pval_fcd = pval_fcd)
  } else {
    tab <- data.frame(model = names(geom_mean_ratios),
                      geom_mean_ratios = geom_mean_ratios,
                      ratios_baseline = ratios_baseline,
                      ratios_baseline2 = ratios_baseline2)
    }
  
  tab <- tab[order(tab$ratios_baseline2), ]
  return(tab)
  
  
  
}

