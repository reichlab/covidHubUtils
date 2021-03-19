#' Create data/hub_targets_us.rda and data/hub_targets_ecdc.rda
#'
get_valid_target_list <- function(){
  zoltar_connection <- setup_zoltar_connection()
  us_project_url <- get_zoltar_project_url(hub = c("US", "ECDC"), zoltar_connection)
  hub_targets_us <- zoltr::targets(zoltar_connection, project_url)$name
  save(hub_targets_us, file = "data/hub_targets_us.rda")
  
  ecdc_project_url <- get_zoltar_project_url(hub = c("ECDC"), zoltar_connection)
  hub_targets_ecdc <- zoltr::targets(zoltar_connection, project_url)$name
  save(hub_targets_ecdc, file = "data/hub_targets_ecdc.rda")
}

