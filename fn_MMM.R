


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
safe_min <- function(x) {
  m <- suppressWarnings(min(x, na.rm = TRUE))
  if (is.infinite(m)) NA_real_ else m
}

safe_max <- function(x) {
  M <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.infinite(M)) NA_real_ else M
}


#' Title
#'
#' @param df 
#' @param group_by_var1 
#' @param trt 
#' @param file_path 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
fn_age <- function(df, 
                   group_by_var1 = NULL,
                   trt = NULL,
                      file_path = "data_derived/mmm/", 
                   file_name){
df_age <- df |>
      group_by(study_area, species, year, {{group_by_var1}}, trt, age_new) |>
      summarise(n = n(),
                #min_den = min(density_new, na.rm = T), 
                min_den = safe_min(density_new), 
                max_den = safe_max(density_new),
                mean_den = mean(density_new, na.rm = T),
                sd_den = sd(density_new, na.rm = T),
                min_bio = safe_min(biomass_new),
                max_bio = safe_max(biomass_new),
                mean_bio = mean(biomass_new, na.rm = T),
                sd_bio = sd(biomass_new, na.rm = T)
      )
   write.csv(df_age, paste0(file_path, file_name, ".csv"), row.names = F)
   return(df_age)
   }



#' Title
#'
#' @param df 
#' @param trt 
#' @param file_path 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
fn_site <- function(df, 
                    trt = NULL, 
                    file_path = "data_derived/mmm/", 
                    file_name){
   df_site <- df |>
      group_by(study_area, species, year, trt, age_new) |>
      summarise(n = n(),
                min_den = safe_min(density_new), 
                max_den = safe_max(density_new),
                mean_den = mean(density_new, na.rm = T),
                sd_den = sd(density_new, na.rm = T),
                min_bio = safe_min(biomass_new),
                max_bio = safe_max(biomass_new),
                mean_bio = mean(biomass_new, na.rm = T),
                sd_bio = sd(biomass_new, na.rm = T)
      )
   write.csv(df_site, paste0(file_path, file_name, ".csv"), row.names = F)
   return(df_site)
}



#' Title
#'
#' @param df 
#' @param trt 
#' @param file_path 
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
fn_yr <- function(df, 
                  trt = NULL, 
                  file_path = "data_derived/mmm/", 
                  file_name){
   df_yr <- df |>
      group_by(study_area, species, trt, age_new) |>
      summarise(n = n(),
                min_den = safe_min(density_new), 
                max_den = safe_max(density_new),
                mean_den = mean(density_new, na.rm = T),
                sd_den = sd(density_new, na.rm = T),
                min_bio = safe_min(biomass_new),
                max_bio = safe_max(biomass_new),
                mean_bio = mean(biomass_new, na.rm = T),
                sd_bio = sd(biomass_new, na.rm = T)
      )
   write.csv(df_yr, paste0(file_path, file_name, ".csv"), row.names = F)
   return(df_yr)
}
