
fn_age <- function(df, 
                   group_by_var1 = NULL,
                   trt = NULL,
                      file_path = "data_derived/mmm/", 
                   file_name){
df_age <- df |>
      group_by(study_area, species, year, {{group_by_var1}}, trt, age_new) |>
      summarise(n = n(),
                min_den = min(density_new, na.rm = T), 
                max_den = max(density_new, na.rm = T),
                mean_den = mean(density_new, na.rm = T),
                sd_den = sd(density_new, na.rm = T),
                min_bio = min(biomass_new, na.rm = T),
                max_bio = max(biomass_new, na.rm = T),
                mean_bio = mean(biomass_new, na.rm = T),
                sd_bio = sd(biomass_new, na.rm = T)
      )
   ewrite.csv(df_age, paste0(file_path, file_name, ".csv"), row.names = F)
   return(df_age)
   }



fn_site <- function(df, trt = NULL, file_path = "data_derived/mmm/", file_name){
   df_site <- df |>
      group_by(study_area, species, year, trt, age_new) |>
      summarise(n = n(),
                min_den = min(density_new, na.rm = T), 
                max_den = max(density_new, na.rm = T),
                mean_den = mean(density_new, na.rm = T),
                sd_den = sd(density_new, na.rm = T),
                min_bio = min(biomass_new, na.rm = T),
                max_bio = max(biomass_new, na.rm = T),
                mean_bio = mean(biomass_new, na.rm = T),
                sd_bio = sd(biomass_new, na.rm = T)
      )
   write.csv(df_site, paste0(file_path, file_name, ".csv"), row.names = F)
   return(df_site)
}



fn_yr <- function(df, trt = NULL, file_path = "data_derived/mmm/", file_name){
   df_yr <- df |>
      group_by(study_area, species, trt, age_new) |>
      summarise(n = n(),
                min_den = min(density_new, na.rm = T), 
                max_den = max(density_new, na.rm = T),
                mean_den = mean(density_new, na.rm = T),
                sd_den = sd(density_new, na.rm = T),
                min_bio = min(biomass_new, na.rm = T),
                max_bio = max(biomass_new, na.rm = T),
                mean_bio = mean(biomass_new, na.rm = T),
                sd_bio = sd(biomass_new, na.rm = T)
      )
   write.csv(df_yr, paste0(file_path, file_name, ".csv"), row.names = F)
   return(df_yr)
}
