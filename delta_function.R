
# fn_delta_Age <- function(df, data_type, min_age){
#    df_filter <- df |>
#       filter(data_den == data_type & age > min_age)
#    return(df_filter)
# }
# 
# temp <- fn_delta_Age(df_med2, "2b", 0)
# unique(temp$age)
# unique(temp$study_area)

# filter dataset by data variable (data_den or bio_den)
## by data variable level or type (e.g. "2b")
## and condition: age == 0 or age > 0.....may need to add other arguements bc ages have been entered in different ways
fn_filterAge <- function(df, var1, data_type, condition){
   # browser()
   df_filter <- df |>
      filter({{var1}} == data_type & {{condition}})
   return(df_filter)
}

# temp <- fn_delta_Age(df_med2, "2b", age > 0)
# unique(temp$age)
# unique(temp$study_area)

# use delta method - sum age and variance; this works because its summing - if averaging, then need to take partial derivative like below
## var1 == a grouping variable like 'site', var2 is density_new or biomass_new, var3 is variance, e.g., d_se_new or b_se_new
fn_delta_Age <- function(df, var1 = NULL, var2, var3) {
   df_dAge <- df |>
      select(study_area, year, species, age, trt, {{var1}}, {{var2}}, {{var3}})|>
      group_by(study_area, year, species, {{var1}}, trt) |>
      summarise(dsum = sum({{var2}}, na.rm = T),
                dsum_var = sum({{var3}}, na.rm = T)^2,
                ll_site = dsum - sqrt(dsum_var)*1.96,
                ul_site = dsum + sqrt(dsum_var)*1.96
                ) 
   return(df_dAge)
}

# fn_delta_Age <- function(df, var1, var2, var_trt = NULL) {
#    if(is.null(var_trt)) {
#       df_dAge <- df |>
#          select(year, species, age, {{var1}}, {{var2}})|>
#          group_by(year, species) |>
#          summarise(dsum = sum({{var1}}, na.rm = T),
#                    dsum_var = sum({{var2}}, na.rm = T)^2) 
#       return(df_dAge)  
#    } else {
#       df_dAge <- df |>
#          select(year, species, age, var_trt, {{var1}}, {{var2}})|>
#          group_by(year, species, var_trt) |>
#          summarise(dsum = sum({{var1}}),
#                    dsum_var = sum({{var2}})^2) 
#       return(df_dAge)      
#    }
# }


# derviatives for site
## need to do partial derivatives for sites because averaging
# fn_delta_derivative_site <- function(df, var1, age){
#    # browser()
#    if(age == "YOY"){
#       df_pd <- df |>
#          group_by(year, species, trt) |>
#          mutate(dsite_var = {{var1}}^2*1/n()^2)      
#    } else if(age == "a1") {
#       df_pd <- df |>
#          group_by(year, species, trt) |>
#          mutate(dsite_var = {{var1}}*1/n()^2)  
#    }
# }   

fn_delta_derivative_site <- function(df, var1, var = "var", trt = NULL){
    # browser()
   if(var == "se"){
      df_pd <- df |>
         group_by(study_area, year, species, {{trt}}) |>
         mutate(dsite_var = {{var1}}^2*1/n()^2)      
   } else if(var == "var") {
      df_pd <- df |>
         group_by(study_area, year, species, {{trt}}) |>
         mutate(dsite_var = {{var1}}*1/n()^2)  
   }
   return(df_pd)
}   

# take means over sites    
fn_delta_site <- function(df, var1, trt = NULL){
   #browser()
   df_delta_site <- df |> 
      group_by(study_area, year, species, {{trt}}) |>
      summarize(
         mean_site = mean({{var1}}, na.rm = T),
         se_site = sqrt(mean(dsite_var, na.rm = T)),
         ll_site = mean_site - se_site*1.96,
         ul_site = mean_site + se_site*1.96
      )
   return(df_delta_site)   
   }     
# temp2 <- fn_delta_Age(temp)


# Year
# this converts se to var and does partial derivative
## this matches EXCEL

# fn_delta_derivative_site <- function(df){
#    df_pd <- df |>
#       group_by(species, trt) |>
#       mutate(dsite_var = dsum_var*1/n())
#    return(df_pd)
# }

# year: partial derivaties
# take the parial deriviatives; var1 is the variance (d_se_new or b_se_new)
## age is just YOY as need to make variance from an SE but this was done in the previous step above
fn_delta_derivative_year <- function(df, var1, var = "var", trt = NULL){
   # browser()
   if(var == "se"){
      df_pd <- df |>
         group_by(study_area, species, {{trt}}) |>
         mutate(dyear_var = {{var1}}^2*1/n()^2)      
         } else if(var == "var") {
      df_pd <- df |>
         group_by(study_area, species, {{trt}}) |>
         mutate(dyear_var = {{var1}}*1/n()^2)  
      }
   return(df_pd)
   }

#temp3 <- fn_delta_derivative_site(temp2, dsum_var)

# take the mean of the estimate and the variance and get CIs
## var1 is biomassnew
fn_delta_year <- function(df, var1, trt = NULL){
   #browser()
   df_delta_year <- df |> 
      group_by(study_area, species, {{trt}}) |>
      summarize(
         mean_year = mean({{var1}}, na.rm = T),
         var_year = sqrt(mean(dyear_var, na.rm = T)),
         ll_site = mean_year - var_year*1.96,
         ul_site = mean_year + var_year*1.96
      )
   return(df_delta_year)   
}
# fn_delta_site(temp3)



