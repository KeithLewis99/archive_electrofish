# this file is to produce DELTA METHOD estimates of all the electrofishing Sstudy areas for which we don't have raw data to do bootstrapping. We will use the delta method to calculate variances for density and biomass at the age and site levels, and then for year when available. We will then use these estimates to create a table of all the electrofishing data.
## see Table X in the tech report.


# export_med is from the excel file df_summary_v1.xlsx where I compiled all of the data from the various reports and papers. This file focused on data in tables while Fig_data_juicr_extracted-v1.xlsx extrated data from tables and I copied this to export_med or export low depending on the quality of the data.
## I standardized the area for some of the studies (e.g., multiplied by 100 for those that were per 1 m2) and standardized the ages and species names. I also added a column for data type (density or biomass) and data quality (1-5). I will use this file to calculate the delta method estimates for all the studies that are ranked 2-5 in terms of data quality.

library(dplyr)
source("delta_function.R")



# Data rank 2 ----
## import data ----
df_med <- read.csv("data/export_med.csv")
str(df_med)
unique(df_med$species)
unique(df_med$study_area)
unique(df_med$age)

# check list of mediums and data types
df_med |>
   group_by(study_area) |>
   summarise(
      unique_den = paste(sort(unique(data_den)), collapse = ", "),
      unique_bio = paste(sort(unique(data_bio)), collapse = ", "),
      .groups = "drop"
   )
      
## stdze data ----
# standardize area
df_med1 <- df_med |>
   mutate(density_new = ifelse(area == 1, density*100, density)) |>
   mutate(dll_new = ifelse(area == 1, dll*100, dll)) |>
   mutate(dul_new = ifelse(area == 1, dul*100, dul)) |> 
   mutate(d_se_new = ifelse(area == 1, d_se*100, d_se)) |> 
   mutate(biomass_new = ifelse(area == 1, biomass*100, biomass)) |>
   mutate(bll_new = ifelse(area == 1, bll*100, bll)) |>
   mutate(bul_new = ifelse(area == 1, bll*100, bll)) |>
   mutate(b_se_new = ifelse(area == 1, b_se*100, b_se)) 


# standardize ages
# filter by data type and remove ages and species
df_med1a <- df_med1 |>
   filter(!(age == "All") & 
             !(age == "1 to 6")
          & !(species == "SAL")) 
unique(df_med1a$age)
unique(df_med1a$area)
unique(df_med1a$species)


# standarize ages and species
library(forcats)
df_med2 <- df_med1a |>
   mutate(age_new = fct_recode(age, 
                               "YOY" = "0", 
                               "YOY" = "0+",
                               ">=1+" = ">0+",
                               ">=1+" = "1", 
                               ">=1+" = "1+",
                               ">=1+" = "2", 
                               ">=1+" = "2+", 
                               ">=1+" = "3", 
                               ">=1+" = "4", 
                               ">=1+" = "5", 
                               ">=1+" = "6")) |>
   mutate(species = fct_recode(species,
                               #"AS" = "SAL", # For WestSalmonRiver, this is actually salmonids, not AS; the values for BT and OU add up to SAL
                               "AS" = "OU"))

unique(df_med2$study_area)
unique(df_med2$age_new)
unique(df_med2$species)


### 2b density ----
# SUM DENSITY, BIOMASS, AND VARIANCES
str(df_med2)
# filter YOY with test
# test <- df_med2 |>
#    filter(data_den == "2b" & age_new == "YOY")
# test |> select(year, species, age, density, density_new, area)

df_2b_den_yoy <- fn_filterAge(df_med2, 
                              data_den, 
                              "2b", 
                              age == 0)
df_2b_den_a1 <- fn_filterAge(df_med2, 
                             data_den, 
                             "2b", 
                             age > 0)
str(df_2b_den_a1)

# delta Age1 - density_new, d_se_new
df_2b_den_a1_d <- fn_delta_Age(df_2b_den_a1, 
                               pt_est2 = density_new, 
                               var_est3 = d_se_new, 
                               age="1+")
str(df_2b_den_a1_d, give.attr = F)

# combine to get yearly CIs for species and ages
df_2b_den_yoy1 <- df_2b_den_yoy |> 
   select(study_area, year, species, trt,  density_new, d_se_new, dll_new, dul_new, age) |>
   rename(dsum = density_new, dsum_var = d_se_new, ll_site = dll_new, ul_site = dul_new)

#bind and write
df_2b_den_site <- rbind(df_2b_den_yoy1, df_2b_den_a1_d)
write.csv(df_2b_den_site, "output/tp_den_bootstrap_1984_1996.csv", row.names = F)


# year: partial derivaties
df_2b_den_yoy_pdy <- 
   fn_delta_derivative_year(df_2b_den_yoy,
                             d_se_new, 
                             "se")
str(df_2b_den_yoy_pdy, give.attr = F)
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
df_2b_den_a1_pdy <- 
   fn_delta_derivative_year(df_2b_den_a1_d, 
                            dsum_var, 
                            "var")
str(df_2b_den_a1_pdy, give.attr = F)

## overall species/age averages
# delta
df_2b_yoy_den_yr <- fn_delta_year(df_2b_den_yoy_pdy, 
                                  density_new, 
                                  dyear_var,
                                  age = "YOY")
df_2b_a1_den_yr <- fn_delta_year(df_2b_den_a1_pdy, 
                                 dsum, 
                                 dyear_var, 
                                 age = "1+")


#bind and write
df_2b_den <- rbind(df_2b_yoy_den_yr, df_2b_a1_den_yr)
write.csv(df_2b_den, "output/tp_den_year_bootstrap_1984_1996.csv", row.names = F)

# this matches EXCEL -          


### 2b biomass ----
# filter by age and data type
df_2b_bio_yoy <- fn_filterAge(df_med2, 
                              data_bio, 
                              "2b", 
                              age == 0)
df_2b_bio_a1 <- fn_filterAge(df_med2, 
                             data_bio, 
                             "2b", 
                             age > 0)
str(df_2b_bio_a1)

# delta Age1
df_2b_bio_da1 <- fn_delta_Age(df_2b_bio_a1, 
                              pt_est2 = biomass_new, 
                              var_est3 = b_se_new,
                              age = "1+")
str(df_2b_bio_da1, give.attr = F)

# rename, bind, write
df_2b_bio_yoy <- df_2b_bio_yoy |> 
   select(study_area, year, species, trt,  density_new, d_se_new, dll_new, dul_new, age) |>
   rename(dsum = density_new, dsum_var = d_se_new, ll_site = dll_new, ul_site = dul_new)

df_2b_bio_age <- rbind(df_2b_bio_yoy, df_2b_bio_da1)
write.csv(df_2b_bio_age, "output/tp_bio_bootstrap_1984_1996.csv", row.names = F)


# year: partial derivaties
# this converts se to var and does partial derivative
df_2b_bio_yoy_pdy <- 
   fn_delta_derivative_year(df_2b_bio_yoy, 
                            dsum_var, 
                            "var")
df_2b_bio_a1_pdy <- 
   fn_delta_derivative_year(df_2b_bio_da1, 
                            dsum_var, 
                            "var")
str(df_2b_bio_a1_pdy, give.attr = F)


# delta
# this takes mean of biomass and variance
df_2b_bio_yoy_yr <- fn_delta_year(df_2b_bio_yoy_pdy, 
                                  dsum, 
                                  dsum_var,
                                  age = "YOY")

df_2b_bio_a1_yr <- fn_delta_year(df_2b_bio_a1_pdy, 
                                 dsum,
                                 dsum_var,
                                 age = "1+")

# biomass
df_2b_bio <- rbind(df_2b_bio_yoy_yr, df_2b_bio_a1_yr)

write.csv(df_2b_bio, "output/tp_bio_year_bootstrap_1984_1996.csv", row.names = F)



## 2a den ----
# filter ages
### age-CL ----
unique(df_med2$study_area)
df_2a_CL <- df_med2 |> filter(
   study_area == "Copper Lake")

df_2a_den_yoy <- fn_filterAge(df_2a_CL, 
                              data_den, 
                              "2a", 
                              age_new == "YOY")
df_2a_den_a1 <- fn_filterAge(df_2a_CL, 
                             data_den, 
                             "2a", 
                             age > 0 & age != "YOY") #& age != "YOY"
str(df_2a_den_a1)
unique(df_2a_den_a1$age)
unique(df_2a_den_a1$study_area)


# delta Age1 - density_new, d_se_new
df_2a_den_a1d <- fn_delta_Age(df_2a_den_a1, 
                              stations, 
                              density_new, 
                              d_se_new,
                              age = "1+")
str(df_2a_den_a1d, give.attr = F)

#bind and write
df_2a_den_yoy1 <- df_2a_den_yoy |> select(study_area, year, species, stations, density_new, d_se_new, dll_new, dul_new, age) |>
   rename(dsum = density_new, dsum_var = d_se_new, ll_site = dll_new, ul_site = dul_new)


df_2a_den_site <- rbind(df_2a_den_yoy1, df_2a_den_a1d[,-5])
write.csv(df_2a_den_site, "output/cl_den_bootstrap_1993_1995.csv", row.names = F)

#### sites ----
df_2a_den_yoy_pds <- 
   fn_delta_derivative_site(df_2a_den_yoy,
                            d_se_new, 
                            "se")
str(df_2a_den_yoy_pds, give.attr = F)
df_2a_den_yoy_pds |> select(year, site, age_new, density_new, d_se_new, dsite_var)
df_2a_den_yoy_site <- fn_delta_site(df_2a_den_yoy_pds, 
                                    density_new,
                                    d_se_new,
                                    age = "YOY")

# +1
df_2a_den_a1_pds <- fn_delta_derivative_site(df_2a_den_a1d, 
                                             dsum_var, 
                                             "var")
df_2a_den_a1_site <- fn_delta_site(df_2a_den_a1_pds, 
                                   dsum, 
                                   dsum_var, 
                                   age = "1+")

# bind and write
df_2a_den_site <- rbind(df_2a_den_yoy_site, df_2a_den_a1_site)
write.csv(df_2a_den_site, "output/cl_den_year_bootstrap_193_1995.csv", row.names = F)


#### years ----
# this converts se to var and does partial derivative
## this matches EXCEL
df_2a_den_yoy_pdy <- 
   fn_delta_derivative_year(df_2a_den_yoy_site, 
                            se_site, 
                            "se")
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
df_2a_den_a1_pdy <- 
   fn_delta_derivative_year(df_2a_den_a1_site, 
                            se_site, 
                            "se")
str(df_2a_den_a1_pdy, give.attr = F)

# delta
# this takes mean of density and variance
df_2a_den_yoy <- fn_delta_year(df_2a_den_yoy_pdy, 
                               mean_site,
                               se_site, 
                               age = "YOY")

df_2a_den_a1 <- fn_delta_year(df_2a_den_a1_pdy, 
                              mean_site,
                              se_site, 
                              age = "1+")

# den
df_2a_den <- rbind(df_2a_den_yoy, df_2a_den_a1) |>
   arrange(study_area)
# biomass for CL is 3a
# no biomass for Joe Farrell's
write.csv(df_2a_den, "output/cl_den_tot_bootstrap_1993_1995.csv", row.names = F)



### age-JF ----
df_2a_JF <- df_med2 |> filter(
   study_area == "Joe Farrell's ")

df_2a_den_yoy <- fn_filterAge(df_2a_JF, 
                              data_den, 
                              "2a", 
                              age_new == "YOY")
df_2a_den_a1 <- fn_filterAge(df_2a_JF, 
                             data_den, 
                             "2a", 
                             age > 0 & age != "YOY") #& age != "YOY"
str(df_2a_den_a1)
unique(df_2a_den_a1$age)
unique(df_2a_den_a1$study_area)


# delta Age1 - density_new, d_se_new
df_2a_den_a1d <- fn_delta_Age(df_2a_den_a1, 
                              site, 
                              density_new, 
                              d_se_new,
                              age = "1+")
str(df_2a_den_a1d, give.attr = F)

#bind and write
df_2a_den_yoy1 <- df_2a_den_yoy |> select(study_area, year, species, site, trt, density_new, d_se_new, dll_new, dul_new, age) |>
   rename(dsum = density_new, dsum_var = d_se_new, ll_site = dll_new, ul_site = dul_new)


df_2a_den_site <- rbind(df_2a_den_yoy1, df_2a_den_a1d)
write.csv(df_2a_den_site, "output/jf_den_bootstrap_1993_1995.csv", row.names = F)


#### sites ----
# get an SE for YOY
df_3a_bio_yoy_year <- df_3a_bio_yoy |> 
   group_by(study_area, year, species) |>
   summarise(biomass_new = sum(biomass, na.rm = T),
             biomass_se = sd(biomass, na.rm = T))

df_3a_bio_yoy_year <- df_3a_bio_yoy_year |> select(year, species, biomass_new, biomass_se)
df_3a_bio_yoy_year$age <- "YOY"

df_3a_bio_yoy_pds <- fn_delta_derivative_site(df_3a_bio_yoy_year,
                                              biomass_se, 
                                              "se")
str(df_3a_bio_yoy_pds, give.attr = F)
df_3a_bio_yoy_pds |> select(year, biomass_new, biomass_se, dsite_var)

df_3a_bio_yoy_site <- fn_delta_site(df_3a_bio_yoy_pds, 
                                    biomass_new,
                                    biomass_se,
                                    age = "YOY"
                                    )
# +1
df_2a_den_a1_pds <- 
   fn_delta_derivative_site(df_2a_den_a1d, 
                            dsum_var, 
                            "var",
                            trt = trt)
df_2a_den_a1_site <- fn_delta_site(df_2a_den_a1_pds, 
                                   dsum,
                                   dsum_var,
                                   trt = trt,
                                   age = "1+")

# bind and write
df_2a_den_site <- rbind(df_2a_den_yoy_site, df_2a_den_a1_site)
write.csv(df_2a_den_site, "output/jf_den_year_bootstrap_193_1995.csv", row.names = F)


#### years ----
# this converts se to var and does partial derivative
## this matches EXCEL
df_2a_den_yoy_pdy <- 
   fn_delta_derivative_year(df_2a_den_yoy_site, 
                            se_site, 
                            "se",
                            trt = trt)
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
df_2a_den_a1_pdy <- 
   fn_delta_derivative_year(df_2a_den_a1_site, 
                            se_site, 
                            "se",
                            trt = trt)
str(df_2a_den_a1_pdy, give.attr = F)

# delta
# this takes mean of density and variance
df_2a_den_yoy <- fn_delta_year(df_2a_den_yoy_pdy, 
                               mean_site,
                               se_site,
                               trt = trt,
                               age = "YOY")
df_2a_den_a1 <- fn_delta_year(df_2a_den_a1_pdy, 
                              mean_site,
                              se_site,
                              trt = trt,
                              age = "1+")

# den
df_2a_den <- rbind(df_2a_den_yoy, df_2a_den_a1) |>
   arrange(study_area)
# biomass for CL is 3a
# no biomass for Joe Farrell's
write.csv(df_2a_den, "output/jf_den_tot_bootstrap_1993_1995.csv", row.names = F)


### age - WS ----
unique(df_med2$study_area)
df_2a_WS <- df_med2 |> filter(
   study_area == "West Salmon River")

df_2a_den_yoy <- fn_filterAge(df_2a_WS, 
                              data_den, 
                              "2a", 
                              age_new == "YOY")
df_2a_den_a1 <- fn_filterAge(df_2a_WS, 
                             data_den, 
                             "2a", 
                             age > 0 & age != "YOY") #& age != "YOY"
str(df_2a_den_a1)
unique(df_2a_den_a1$age)
unique(df_2a_den_a1$study_area)


# delta Age1 - density_new, d_se_new
df_2a_den_a1d <- fn_delta_Age(df_2a_den_a1, 
                              site, 
                              density_new, 
                              d_se_new,
                              age = "1+")
str(df_2a_den_a1d, give.attr = F)

#bind and write
df_2a_den_yoy1 <- df_2a_den_yoy |> select(study_area, year, species, site, density_new, d_se_new, dll_new, dul_new, age) |>
   rename(dsum = density_new, dsum_var = d_se_new, ll_site = dll_new, ul_site = dul_new)


df_2a_den_site <- rbind(df_2a_den_yoy1, df_2a_den_a1d[,-5])
write.csv(df_2a_den_site, "output/ws_den_bootstrap_1992.csv", row.names = F)

#### sites ----
df_2a_den_yoy_pds <- 
   fn_delta_derivative_site(df_2a_den_yoy, 
                            d_se_new, 
                            "se"
                            )
str(df_2a_den_yoy_pds, give.attr = F)
df_2a_den_yoy_pds |> select(year, site, age_new, density_new, d_se_new, dsite_var)
df_2a_den_yoy_site <- fn_delta_site(df_2a_den_yoy_pds, 
                                    density,
                                    d_se, 
                                    age = "YOY")

# +1
df_2a_den_a1_pds <- 
   fn_delta_derivative_site(df_2a_den_a1d, 
                            dsum_var, 
                            "var")
df_2a_den_a1_site <- fn_delta_site(df_2a_den_a1_pds,
                                   dsum,
                                   dsum_var, 
                                   age = "1+")


# bind and write
df_2a_den_site <- rbind(df_2a_den_yoy_site, df_2a_den_a1_site)
write.csv(df_2a_den_site, "output/ws_den_year_bootstrap_1993.csv", row.names = F)


## 3a ----
### age -  ----
### YOY
df_3a_bio_yoy <- fn_filterAge(df_2a_CL, 
                              data_bio, 
                              "3a", 
                              age_new == "YOY")
df_3a_bio_yoy <- df_3a_bio_yoy |> 
   select(study_area, stations, year, species, biomass_new, b_se_new, age) |>
   rename(biomass = biomass_new, biomass_se = b_se_new)


### +1
df_3a_bio_a1 <- fn_filterAge(df_2a_CL, 
                             data_bio, 
                             "3a", 
                             age > 0 & age != "YOY")
df_3a_bio_a1 <- df_3a_bio_a1 |> select(study_area, stations, year, species, biomass_new)

# average age
df_3a_bio_a1_d <- df_3a_bio_a1 |> 
   group_by(study_area, year, stations, species) |>
   summarise(biomass = sum(biomass_new, na.rm = T),
             biomass_se = sd(biomass_new, na.rm = T))
df_3a_bio_a1_d <- df_3a_bio_a1_d |> select(year, species, biomass, biomass_se)
df_3a_bio_a1_d$age <- "1+"


# bind and write
df_3a_bio_site <- rbind(df_3a_bio_yoy, df_3a_bio_a1_d)
write.csv(df_3a_bio_site, "output/cl_bio_age_bootstrap_1993_1995.csv", row.names = F)


### site ----
## yoy - dplyr

df_3a_bio_yoy_site <- df_3a_bio_yoy |>
   group_by(study_area, year, species) |>
   summarise(biomass_new = mean(biomass, na.rm = T), 
             biomass_se = sd(biomass, na.rm = T))
df_3a_bio_yoy_site


# a1
df_3a_bio_a1_pdy <- 
   fn_delta_derivative_site(df_3a_bio_a1_d,
                            biomass_se, 
                            "se")
df_3a_bio_a1 <- fn_delta_site(df_3a_bio_a1_pdy, 
                              biomass,
                              biomass_se,
                              age = "1+")

# bind and write
df_3a_bio_site <- rbind(df_3a_bio_yoy_site, df_3a_bio_a1)
write.csv(df_3a_bio_site, "output/cl_bio_site_bootstrap_1993_1995.csv", row.names = F)

### year ----
## this matches EXCEL
df_3a_bio_yoy_pdy <- 
   fn_delta_derivative_year(df_3a_bio_yoy_site, 
                            biomass_se, 
                            "se")

df_3a_bio_yoy <- fn_delta_year(df_3a_bio_yoy_pdy, 
                               biomass_new,
                               biomass_se,
                               age = "YOY")

df_3a_bio_a1_pdy <- 
   fn_delta_derivative_year(df_3a_bio_a1, 
                            se_site, 
                            "se")

df_3a_bio_a1 <- fn_delta_year(df_3a_bio_a1_pdy, 
                               mean_site,
                               se_site,
                               age = "1+")

df_3a_bio <- rbind(df_3a_bio_yoy, df_3a_bio_a1)
df_3a_bio

write.csv(df_2a_den_site, "output/ws_bio_bootstrap_1993.csv", row.names = F)




## 3b ----
### age - Trepassey ----
## there is only 3b for Trepassy so the below works.
df_3b_den_yoy <- fn_filterAge(df_med2, 
                              data_den, 
                              "3b", 
                              age == 0)
df_3b_den_yoy <- df_3b_den_yoy |> 
   select(study_area, year, species, density_new, d_se_new, biomass_new, b_se_new, age_new) |>
   rename(density = density_new, density_se = d_se_new, biomass = biomass_new, biomass_se = b_se_new, age = age_new)

df_3b_den_a1 <- fn_filterAge(df_med2, 
                             data_den, 
                             "3b", 
                             age > 0)

# get variance for age for a1
df_3b_den_a1_d <- df_3b_den_a1 |> filter(
   study_area == "Northeast Brook, Trepassey") |>
   group_by(study_area, year, species) |>
   summarise(density = sum(density_new),
             density_se = sd(density_new),
             biomass = sum(biomass_new),
             biomass_se = sd(biomass_new))
df_3b_den_a1_d$age <- "1+"
df_3b_den_a1_d$age <- as.factor(df_3b_den_a1_d$age)

# write and bind
df_3b_year <- rbind(df_3b_den_yoy, df_3b_den_a1_d)
write.csv(df_3b_year, "output/treap__bootstrap_1994_1996.csv", row.names = F)


# year for YOY
df_3b_yr_yoy <- df_3b_den_yoy |>
   group_by(study_area, species) |>
   summarise(density_new = mean(density, na.rm = T),
             density_se = sd(density, na.rm = T),
             biomass_new = mean(biomass, na.rm = T),
             biomass_se = sd(biomass, na.rm = T)
   )
df_3b_yr_yoy$age <- "YOY"

# year for age 1+
df_3b_yr_den_a1_pdy <- fn_delta_derivative_year(df_3b_den_a1_d,
                         density_se,
                         "se")
df_3b_year_den_a1 <- fn_delta_year(df_3b_yr_den_a1_pdy, 
              density,
              density_se,
              age = "1+")

df_3b_yr_bio_a1_pdy <- fn_delta_derivative_year(df_3b_den_a1_d,
                                                biomass_se,
                                                "se")
df_3b_year_bio_a1 <- fn_delta_year(df_3b_yr_bio_a1_pdy, 
                                   biomass,
                                   biomass_se,
                                   age = "1+")

# write and bind
df_3b_yr_yoy_d <- df_3b_yr_yoy |> 
   select(study_area, species, density_new, density_se, age) |>
   rename(mean_year = density_new, var_year = density_se)
df_3b_yr_yoy_b <- df_3b_yr_yoy |> 
   select(study_area, species, biomass_new, biomass_se, age) |>
   rename(mean_year = biomass_new, var_year = biomass_se)


df_3b_den_a1_d
df_3b_year_bio_a1
df_3b_year_den <- rbind(df_3b_yr_yoy_d, df_3b_den_a1_d)
write.csv(df_3b_year_den, "output/trep__bootstrap__den_1994_1996.csv", row.names = F)
df_3b_year_bio <- rbind(df_3b_yr_yoy_b, df_3b_year_bio_a1)
write.csv(df_3b_year_bio, "output/trep__bootstrap_bio_1994_1996.csv", row.names = F)


# Low ----
## import data ----
## stdz data ----
df_low <- read.csv("data/export_low.csv")
str(df_low)
unique(df_low$species)
unique(df_low$study_area)

df_low |>
   group_by(study_area) |>
   summarise(
      unique_den = paste(sort(unique(data.quality)), collapse = ", "),
      .groups = "drop"
   )

# standardize area
df_low1 <- df_low |>
   mutate(density_new = ifelse(modifier == 1, density*100, density)) |>
   mutate(dll_new = ifelse(modifier == 1, dll*100, dll)) |>
   mutate(dul_new = ifelse(modifier == 1, dul*100, dul)) |> 
   mutate(d_se_new = ifelse(modifier == 1, d_se*100, d_se)) |> 
   mutate(biomass_new = ifelse(modifier == 1, biomass*100, biomass)) |>
   mutate(bll_new = ifelse(modifier == 1, bll*100, bll)) |>
   mutate(bul_new = ifelse(modifier == 1, bll*100, bll)) |>
   mutate(b_se_new = ifelse(modifier == 1, b_se*100, b_se)) 


## 2c ----
### age - GG ----
df_2c_GG <- df_low1 |> filter(
   study_area == "Great Gull Brook")


### sites ----
df_2c_den_pds <- fn_delta_derivative_site(df_2c_GG, d_se_new, "se")

df_2c_den_site <- fn_delta_site(df_2c_den_pds, density_new)


### years ----
# this converts se to var and does partial derivative
## this matches EXCEL
df_2c_den_pdy <- fn_delta_derivative_year(df_2c_den_site, se_site, "se")
df_2a_den <- fn_delta_year(df_2c_den_pdy, mean_site)

str(df_2a_den_a1_pdy, give.attr = F)



## 4 -----
### Corner Brook (behind copper lake), Indian Bay, Gander River
df_CB_IB_GR <- df_low1 |>
   filter(data.quality == "4") |>
   group_by(study_area, species) |>
   summarise(density = mean(density_new), 
             biomass = mean(biomass_new))

## 5 -----
df_TN <- df_low1 |>
   filter(data.quality == "5a") |>
   group_by(species) |>
   summarise(density = mean(density_new), 
             biomass = mean(biomass_new))


df_TN_TI <- df_low1 |>
   filter(data.quality == "5b") |>
   group_by(species) |>
   summarise(density = mean(density_new))

# #####

# df_med3 <- df_med2 |>
#    group_by(study_area, site, year, species, trt, age, data_den, data_bio) |>
#    summarise(density_sum = sum(density, na.rm = T), 
#              biomass_sum = sum(biomass, na.rm = T), 
#    )
# 
# df_med3 |> print(n = Inf)
# str(df_med3)
# unique(df_med3$study_area)
# 
# dat3_4.bio.boot <- df_med3 |> 
#    filter(data_bio == "3") |>
#    group_by(study_area, year, species, age, trt) |>
#    do(data.frame(rbind(Hmisc::smean.cl.boot(.$biomass_sum)))) |>
#    rename(bio = Mean, bll = Lower, bul = Upper)
# unique(dat3_4.bio.boot$study_area)
# 
# dat3_4.den.boot <- df_med3 |> 
#    filter(data_den == "3") |>
#    group_by(study_area, year, species, age, trt) |>
#    do(data.frame(rbind(Hmisc::smean.cl.boot(.$density_sum)))) |>
#    rename(density = Mean, dll = Lower, dul = Upper)
# unique(dat3_4.den.boot$study_area)
# 
# dat_3_4.boot <- full_join(dat3_4.bio.boot, dat3_4.den.boot, by = c( "study_area", "year", "species", "age", "trt")) 
# dat_3_4.boot |> print(n = Inf)
# 
# unique(dat_3_4.boot$study_area)
# write.csv(dat_3_4.boot, "data_derived/dat_3_4.boot.csv", row.names = F)

# END ----
