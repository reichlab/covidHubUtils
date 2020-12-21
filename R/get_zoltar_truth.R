#' Generate versioned truth data for zoltar for a specified target
#' It only includes national and state-level truth data.
#' 
#' @param target string specifying target. 
#' Currently only support "Cumulative Deaths" and "Incident Deaths".
#' @param issue_date optional date specifying issue date of truth data.
#' Default to NULL which will load the latest truth data from covidData. 
#' 
#' @return data frame with timezero, unit, target and value
#'
#' @export
preprocess_truth_for_zoltar <- function(target, issue_date = NULL){
  
  # validate target 
  target <- match.arg(target, 
                      choices = c("Cumulative Deaths", "Incident Deaths"), 
                      several.ok = FALSE)
  
  # load the most up to date weeky truth data from JHU CSSE
  df <- covidData::load_jhu_data(issue_date = issue_date, 
                                 spatial_resolution = c('national', 'state'),
                                 temporal_resolution = 'weekly', 
                                 measure = 'deaths',
                                 replace_negatives = FALSE, 
                                 adjustment_cases = 'none',
                                 adjustment_method = 'none')
  
  # select columns needed for target
  if (target == "Cumulative Deaths"){
    target_var <- " wk ahead cum death"
    df <- df %>%
      dplyr::select(-inc) %>%
      dplyr::rename(value = cum, unit = location)
  } else {
    target_var <- " wk ahead inc death"
    df <- df %>%
      dplyr::select(-cum) %>%
      dplyr::rename(value = inc, unit = location)
  }
  
  # expand the data frame where for each observed value at time t, 
  # that observation is the observed value at each horizon 1 through 20 
  # relative to the corresponding past forecast timezeros from times t-1 through t-20
  df <- tidyr::expand_grid(df, horizon = seq_len(20)) %>%
    dplyr::mutate(target = paste0(horizon,target_var))
    
  # set up Zoltar connection
  zoltar_connection <- zoltr::new_connection()
  if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
    zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
  } else {
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
  }
  
  # construct Zoltar project url
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # get all valid timezeros from zoltar
  zoltar_timezeros<- zoltr::timezeros(zoltar_connection, project_url)$timezero_date
  
  # generate target end date with horzion 1 to 20 for all zoltar_timezeros
  df_zoltar <- tidyr::expand_grid(
    zoltar_timezeros = zoltar_timezeros, horizon = seq_len(20)) %>%
    dplyr::mutate(target_end_date = 
                    covidHubUtils::calc_target_week_end_date(
                      zoltar_timezeros, horizon))
  
  # merge zoltar timezeros with truth values and targets
  df_final <- merge(x = df, 
                    y = df_zoltar, 
                    by.x = c('date','horizon'),
                    by.y = c('target_end_date','horizon'),
                    all.y = TRUE)
  
  # select columns and drop empty rows
  df_final <- df_final %>%
    dplyr::select(zoltar_timezeros, unit, target, value) %>%
    dplyr::rename(timezero = zoltar_timezeros) %>%
    tidyr::drop_na()
  
  return (df_final)
  
}


#' Generate the most up to date truth data for zoltar
#' It only includes national and state-level truth data.
#' 
#' @param save_location character specifying the location of to save zoltar truth data.
#' Default to "./data-truth"
#'
#' @export
#'
save_truth_for_zoltar <- function(save_location = "./data-truth"){
  
  df_cum_death <- covidData::preprocess_truth_for_zoltar("Cumulative Deaths")
  df_inc_death <- covidData::preprocess_truth_for_zoltar("Incident Deaths")
  
  zoltar_truth <- rbind(df_cum_death, df_inc_death)
  
  file_path <- file.path(save_location,"zoltar-truth.csv")
  
  readr::write.csv(zoltar_truth, file_path)
  
}