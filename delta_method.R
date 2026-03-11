# this file is to produce DELTA METHOD estimates of all the electrofishing Sstudy areas for which we don't have raw data to do bootstrapping. We will use the delta method to calculate variances for density and biomass at the age and site levels, and then for year when available. We will then use these estimates to create a table of all the electrofishing data.
## see Table X in the tech report.


# export_med is from the excel file df_summary_v1.xlsx where I compiled all of the data from the various reports and papers. This file focused on data in tables while Fig_data_juicr_extracted-v1.xlsx extrated data from tables and I copied this to export_med or export low depending on the quality of the data.
## I standardized the area for some of the studies (e.g., multiplied by 100 for those that were per 1 m2) and standardized the ages and species names. I also added a column for data type (density or biomass) and data quality (1-5). I will use this file to calculate the delta method estimates for all the studies that are ranked 2-5 in terms of data quality.

# need to make sure that I am using the right density and biomass and assocaited variance/se throughout - what about dyear_var
# check that the proper age is being filtered
# make talbes
# check against excel
# check output tables
# chnage names of tables

library(dplyr)
library(forcats)
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
# standardize area - recalculate density  biomass
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
#### Trepassey ----
# SUM DENSITY, BIOMASS, AND VARIANCES
# no site information
str(df_med2)
# filter YOY with test
# test <- df_med2 |>
#    filter(data_den == "2b" & age_new == "YOY")
# test |> select(year, species, age, density, density_new, area)

df_2b_den_yoy <- fn_filterAge(df_med2, 
                              data_den, 
                              "2b", 
                              age == 0)
str(df_2b_den_yoy)
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
df_2b_den_age <- rbind(df_2b_den_yoy1, df_2b_den_a1_d)
write.csv(df_2b_den_age, "data_derived/tp_den_delta_age_1984_1996.csv", row.names = F)


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
df_2b_den_yr <- rbind(df_2b_yoy_den_yr, df_2b_a1_den_yr)
write.csv(df_2b_den_yr, "data_derived/tp_den_delta_yr_1984_1996.csv", row.names = F)

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
write.csv(df_2b_bio_age, "data_derived/tp_bio_delta_age_1984_1996.csv", row.names = F)


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
df_2b_bio_yr <- rbind(df_2b_bio_yoy_yr, df_2b_bio_a1_yr)

write.csv(df_2b_bio_yr, "data_derived/tp_bio_delta_yr_1984_1996.csv", row.names = F)



## 2a den ----
# filter ages
### CL-age ----
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


df_2a_den_age <- rbind(df_2a_den_yoy1, df_2a_den_a1d[,-5])
write.csv(df_2a_den_age, "data_derived/cl_den_delta_age_1993_1995.csv", row.names = F)

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
write.csv(df_2a_den_site, "data_derived/cl_den_delta_site_1993_1995.csv", row.names = F)


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
df_2a_den_yr <- rbind(df_2a_den_yoy, df_2a_den_a1) |>
   arrange(study_area)
# biomass for CL is 3a
# no biomass for Joe Farrell's
write.csv(df_2a_den_yr, "data_derived/cl_den_delta_yr_1993_1995.csv", row.names = F)



### JF-age ----
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


df_2a_den_age <- rbind(df_2a_den_yoy1, df_2a_den_a1d)
write.csv(df_2a_den_age, "data_derived/jf_den_delta_age_1993_1995.csv", row.names = F)


#### sites ----
# get an SE for YOY

df_2a_den_yoy_year <- df_2a_den_yoy1 |> 
   group_by(study_area, year, species, trt) |>
   summarise(density_new = sum(dsum, na.rm = T),
             density_se = sd(dsum, na.rm = T))

df_2a_den_yoy_year <- df_2a_den_yoy_year |> select(year, species, density_new, density_se, trt)
df_2a_den_yoy_year$age <- "YOY"

df_2a_den_yoy_pds <- fn_delta_derivative_site(df_2a_den_yoy_year,
                                              density_se, 
                                              "se", 
                                              trt = trt)
str(df_2a_den_yoy_pds, give.attr = F)
df_2a_den_yoy_pds |> select(year, density_new, density_se, dsite_var)

df_2a_den_yoy_site <- fn_delta_site(df_2a_den_yoy_pds, 
                                    density_new,
                                    density_se,
                                    trt = trt,
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
write.csv(df_2a_den_site, "data_derived/jf_den_delta_site_1993_1995.csv", row.names = F)


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
df_2a_den_yr <- rbind(df_2a_den_yoy, df_2a_den_a1) |>
   arrange(study_area)
# biomass for CL is 3a
# no biomass for Joe Farrell's
write.csv(df_2a_den_yr, "data_derived/jf_den_delta_yr_1993_1995.csv", row.names = F)


### WS-age ----
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


df_2a_den_age <- rbind(df_2a_den_yoy1, df_2a_den_a1d[,-5])
write.csv(df_2a_den_age, "data_derived/ws_den_delta_age_1992.csv", row.names = F)

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
write.csv(df_2a_den_site, "data_derived/ws_den_delta_site_1992.csv", row.names = F)



### SJ-age ----
unique(df_med2$study_area)
df_2a_SJ <- df_med2 |> filter(
   study_area == "St. John's" & data_den == "2a")
unique(df_2a_SJ$age_new)

sj_2a_den_yoy <- fn_filterAge(df_2a_SJ, 
                              data_den, 
                              "2a", 
                              age_new == "YOY")
sj_2a_den_a1 <- fn_filterAge(df_2a_SJ, 
                             data_den, 
                             "2a", 
                             age > 0 & age != "YOY") #& age != "YOY"


# delta Age1 - biomass_new, d_se_new
sj_2a_den_a1d <- fn_delta_Age(sj_2a_den_a1, 
                              site, 
                              biomass_new, 
                              d_se_new,
                              age = "1+")
str(sj_2a_den_a1d, give.attr = F)

#bind and write
sj_2a_den_yoy1 <- sj_2a_den_yoy |> select(study_area, year, species, site, biomass_new, d_se_new, dll_new, dul_new, age) |>
   rename(dsum = biomass_new, dsum_var = d_se_new, ll_site = dll_new, ul_site = dul_new)


df_2a_den_age <- rbind(sj_2a_den_yoy1, sj_2a_den_a1d[-5])
write.csv(df_2a_den_age, "data_derived/sj_den_delta_age_1982_1985.csv", row.names = F)

### biomass
sj_2a_bio_yoy <- fn_filterAge(df_2a_SJ, 
                              data_bio, 
                              "2a", 
                              age_new == "YOY")
sj_2a_bio_a1 <- fn_filterAge(df_2a_SJ, 
                             data_bio, 
                             "2a", 
                             age > 0 & age != "YOY") #& age != "YOY"


# delta Age1 - biomass_new, b_se_new
sj_2a_bio_a1d <- fn_delta_Age(sj_2a_bio_a1, 
                              site, 
                              biomass_new, 
                              b_se_new,
                              age = "1+")
str(sj_2a_bio_a1d, give.attr = F)

#bind and write
sj_2a_bio_yoy1 <- sj_2a_bio_yoy |> select(study_area, year, species, site, biomass_new, b_se_new, bll_new, bul_new, age) |>
   rename(dsum = biomass_new, dsum_var = b_se_new, ll_site = bll_new, ul_site = bul_new)


df_2a_bio_age <- rbind(sj_2a_bio_yoy1, sj_2a_bio_a1d[-5])
write.csv(df_2a_bio_age, "data_derived/sj_bio_delta_age_1982_1985.csv", row.names = F)




#### sites ----
## density
sj_2a_den_yoy_pds <- 
   fn_delta_derivative_site(sj_2a_den_yoy1,
                            dsum_var, 
                            "var")
str(sj_2a_den_yoy_pds, give.attr = F)
sj_2a_den_yoy_pds |> select(year, site, age, dsum, dsum_var, dsite_var)
sj_2a_den_yoy_site <- fn_delta_site(sj_2a_den_yoy_pds, 
                                    dsum,
                                    dsum_var,
                                    age = "YOY")

# +1
sj_2a_den_a1_pds <- fn_delta_derivative_site(sj_2a_den_a1d, 
                                             dsum_var, 
                                             "var")
sj_2a_den_a1_site <- fn_delta_site(sj_2a_den_a1_pds, 
                                   dsum, 
                                   dsum_var, 
                                   age = "1+")

# bind and write
sj_2a_den_site <- rbind(sj_2a_den_yoy_site, sj_2a_den_a1_site)
write.csv(sj_2a_den_site, "data_derived/sj_den_delta_site_1983_1985.csv", row.names = F)


# biomass
sj_2a_bio_yoy_pds <- 
   fn_delta_derivative_site(sj_2a_bio_yoy1,
                            dsum_var, 
                            "var")
str(sj_2a_bio_yoy_pds, give.attr = F)
sj_2a_bio_yoy_pds |> select(year, site, age, dsum, dsum_var, dsite_var)
sj_2a_bio_yoy_site <- fn_delta_site(sj_2a_bio_yoy_pds, 
                                    dsum,
                                    dsum_var,
                                    age = "YOY")

# +1
sj_2a_bio_a1_pds <- fn_delta_derivative_site(sj_2a_bio_a1d, 
                                             dsum_var, 
                                             "var")
sj_2a_bio_a1_site <- fn_delta_site(sj_2a_bio_a1_pds, 
                                   dsum, 
                                   dsum_var, 
                                   age = "1+")

# bind and write
sj_2a_bio_site <- rbind(sj_2a_bio_yoy_site, sj_2a_bio_a1_site)
write.csv(sj_2a_bio_site, "data_derived/sj_bio_delta_site_1983_1985.csv", row.names = F)


#### years80 ----
# this converts se to var and does partial derivative
## density
## split out 1980s bc can't compare densities from 1980s and 1990s to biomass from each seperate period
sj_2a_den_yoy_site80 <- sj_2a_den_yoy_site |> filter(year < 1995)
sj_2a_den_yoy_pdy80 <- 
   fn_delta_derivative_year(sj_2a_den_yoy_site80, 
                            se_site, 
                            "se")
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
sj_2a_den_a1_site80 <- sj_2a_den_a1_site |> filter(year < 1995)
sj_2a_den_a1_pdy80 <- 
   fn_delta_derivative_year(sj_2a_den_a1_site80, 
                            se_site, 
                            "se")
str(sj_2a_den_a1_pdy80, give.attr = F)

# delta
# this takes mean of density and variance
sj_2a_den_yoy80 <- fn_delta_year(sj_2a_den_yoy_pdy80, 
                               mean_site,
                               se_site, 
                               age = "YOY")

sj_2a_den_a180 <- fn_delta_year(sj_2a_den_a1_pdy80, 
                              mean_site,
                              se_site, 
                              age = "1+")

# den
sj_2a_den_yr <- rbind(sj_2a_den_yoy80, sj_2a_den_a180) |>
   arrange(study_area)
# biomass for CL is 3a
# no biomass for Joe Farrell's
write.csv(sj_2a_den_yr, "data_derived/sj_den_delta_yr_1982_1985.csv", row.names = F)

## biomass
sj_2a_bio_yoy_site80 <- sj_2a_bio_yoy_site |> filter(year < 1995)
sj_2a_bio_yoy_pdy80 <- 
   fn_delta_derivative_year(sj_2a_bio_yoy_site80, 
                            se_site, 
                            "se")
#df_2b_d0pd |> select(year, species, trt, biomass_new, dyear_var)
sj_2a_bio_a1_site80 <- sj_2a_bio_a1_site |> filter(year < 1995)
sj_2a_bio_a1_pdy80 <- 
   fn_delta_derivative_year(sj_2a_bio_a1_site80, 
                            se_site, 
                            "se")
str(sj_2a_bio_a1_pdy80, give.attr = F)

# delta
# this takes mean of density and variance
sj_2a_bio_yoy80 <- fn_delta_year(sj_2a_bio_yoy_site80, 
                               mean_site,
                               se_site, 
                               age = "YOY")

sj_2a_bio_a180 <- fn_delta_year(sj_2a_bio_a1_pdy80, 
                              mean_site,
                              se_site, 
                              age = "1+")

# bio
sj_2a_bio_yr80 <- rbind(sj_2a_bio_yoy80, sj_2a_bio_a180) |>
   arrange(study_area)
write.csv(sj_2a_bio_yr80, "data_derived/sj_bio_delta_yr_1983_1985.csv", row.names = F)


#### years90 ----
# this converts se to var and does partial derivative
## density
## split out 1980s bc can't compare densities from 1980s and 1990s to biomass from each seperate period
sj_2a_den_yoy_site95 <- sj_2a_den_yoy_site |> filter(year >= 1995)
sj_2a_den_yoy_pdy95 <- 
   fn_delta_derivative_year(sj_2a_den_yoy_site95, 
                            se_site, 
                            "se")
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
sj_2a_den_a1_site95 <- sj_2a_den_a1_site |> filter(year < 1995)
sj_2a_den_a1_pdy95 <- 
   fn_delta_derivative_year(sj_2a_den_a1_site95, 
                            se_site, 
                            "se")
str(sj_2a_den_a1_pdy95, give.attr = F)

# delta
# this takes mean of density and variance
sj_2a_den_yoy95 <- fn_delta_year(sj_2a_den_yoy_pdy95, 
                                 mean_site,
                                 se_site, 
                                 age = "YOY")

sj_2a_den_a195 <- fn_delta_year(sj_2a_den_a1_pdy95, 
                                mean_site,
                                se_site, 
                                age = "1+")

# den
sj_2a_den_yr95 <- rbind(sj_2a_den_yoy95, sj_2a_den_a195) |>
   arrange(study_area)
# biomass for CL is 3a
# no biomass for Joe Farrell's
write.csv(sj_2a_den_yr95, "data_derived/sj_den_delta_yr_1995_1996.csv", row.names = F)



## 3a-CL ----
### age -  ----
### YOY
df_3a_bio_yoy <- fn_filterAge(df_2a_CL, 
                              data_bio, 
                              "3a", 
                              age_new == "YOY")
df_3a_bio_yoy <- df_3a_bio_yoy |> 
   select(study_area, year, species, stations, biomass_new, b_se_new, age) |>
   rename(biomass = biomass_new, biomass_se = b_se_new)


### +1
df_3a_bio_a1 <- fn_filterAge(df_2a_CL, 
                             data_bio, 
                             "3a", 
                             age > 0 & age != "YOY")
df_3a_bio_a1 <- df_3a_bio_a1 |> select(study_area, year, species, stations, biomass_new)

# average age
df_3a_bio_a1_d <- df_3a_bio_a1 |> 
   group_by(study_area, year, stations, species) |>
   summarise(biomass = sum(biomass_new, na.rm = T),
             biomass_se = sd(biomass_new, na.rm = T))
df_3a_bio_a1_d <- df_3a_bio_a1_d |> select(year, species, biomass, biomass_se)
df_3a_bio_a1_d$age <- "1+"


# bind and write
df_3a_bio_age <- rbind(df_3a_bio_yoy, df_3a_bio_a1_d)
write.csv(df_3a_bio_age, "data_derived/cl_bio_delta_age_1993_1995.csv", row.names = F)


### site ----
## yoy - dplyr

df_3a_bio_yoy_site <- df_3a_bio_yoy |>
   group_by(study_area, year, species) |>
   summarise(mean_site = mean(biomass, na.rm = T), 
             se_site = sd(biomass, na.rm = T),
             ll_site = mean_site - se_site*1.96,
             ul_site = mean_site + se_site*.196)
df_3a_bio_yoy_site$age <- "0"


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
write.csv(df_3a_bio_site, "data_derived/cl_bio_site_1993_1995.csv", row.names = F)

### year ----
## this matches EXCEL
df_3a_bio_yoy_pdy <- 
   fn_delta_derivative_year(df_3a_bio_yoy_site, 
                            se_site, 
                            "se")

df_3a_bio_yoy <- fn_delta_year(df_3a_bio_yoy_pdy, 
                               mean_site,
                               se_site,
                               age = "YOY")

df_3a_bio_a1_pdy <- 
   fn_delta_derivative_year(df_3a_bio_a1, 
                            se_site, 
                            "se")

df_3a_bio_a1 <- fn_delta_year(df_3a_bio_a1_pdy, 
                               mean_site,
                               se_site,
                               age = "1+")

df_3a_bio_yr <- rbind(df_3a_bio_yoy, df_3a_bio_a1)
df_3a_bio_yr

write.csv(df_3a_bio_yr, "data_derived/cl_bio_delta_yr_1993_1995.csv", row.names = F)


## 3a-Highlands ----

df_3a_HL <- df_med2 |> filter(
   study_area == "Highlands")

### age -  ----
### YOY
df_HL3a_den_yoy <- fn_filterAge(df_3a_HL, 
                              data_den, 
                              "3a", 
                              age_new == "YOY")
df_HL3a_den_yoy <- df_HL3a_den_yoy |>
   select(study_area, site, year, species, trt, density_new, d_se_new, age) |>
   rename(density = density_new, density_se = d_se_new)


### +1
df_HL3a_den_a1 <- fn_filterAge(df_3a_HL, 
                             data_den, 
                             "3a", 
                             age_new == ">=1+")
df_HL3a_den_a1 <- df_HL3a_den_a1 |> select(study_area, site, year, species, trt, density_new)

# average age
df_HL3a_den_a1_d <- df_HL3a_den_a1 |> 
   group_by(study_area, year, site, species, trt) |>
   summarise(density = sum(density_new, na.rm = T),
             density_se = sd(density_new, na.rm = T))
df_HL3a_den_a1_d <- df_HL3a_den_a1_d |> select(year, species, trt, density, density_se)
df_HL3a_den_a1_d$age <- "1+"


# bind and write
df_HL3a_den_age <- rbind(df_HL3a_den_yoy, df_HL3a_den_a1_d)
write.csv(df_HL3a_den_age, "data_derived/HL_den_delta_age_2002.csv", row.names = F)


### site ----
## yoy - dplyr

df_HL3a_den_yoy_site <- df_HL3a_den_yoy |>
   group_by(study_area, year, species, trt) |>
   summarise(density_new = mean(density, na.rm = T), 
             density_se = sd(density, na.rm = T))
df_HL3a_den_yoy_site


# a1
df_HL3a_den_a1_pdy <- 
   fn_delta_derivative_site(df_HL3a_den_a1_d,
                            density_se, 
                            "se",
                            trt = trt)
df_HL3a_den_a1 <- fn_delta_site(df_HL3a_den_a1_pdy, 
                              density,
                              density_se,
                              age = "1+")

# bind and write
df_HL3a_den_site <- rbind(df_HL3a_den_yoy_site, df_HL3a_den_a1)
write.csv(df_HL3a_den_site, "data_derived/HL_den_delta_site_2002.csv", row.names = F)

### year ----
## this matches EXCEL
df_HL3a_den_yoy_pdy <- 
   fn_delta_derivative_year(df_HL3a_den_yoy_site, 
                            density_se, 
                            "se")

df_HL3a_den_yoy <- fn_delta_year(df_HL3a_den_yoy_pdy, 
                               density_new,
                               density_se,
                               age = "YOY")

df_HL3a_den_a1_pdy <- 
   fn_delta_derivative_year(df_HL3a_den_a1, 
                            se_site, 
                            "se")

df_HL3a_den_a1 <- fn_delta_year(df_HL3a_den_a1_pdy, 
                              mean_site,
                              se_site,
                              age = "1+")

df_HL3a_den_yr <- rbind(df_HL3a_den_yoy, df_HL3a_den_a1)

write.csv(df_HL3a_den_yr, "data_derived/HL_den_delta_yr_2002.csv", row.names = F)


## WS biomass -----

### age -  ----
### YOY
df_WS3a_bio_yoy <- fn_filterAge(df_2a_WS, 
                                data_bio, 
                                "3a", 
                                age_new == "YOY")
df_WS3a_bio_yoy <- df_WS3a_bio_yoy |>
   select(study_area, site, year, species, trt, biomass_new, b_se_new, age) |>
   rename(biomass = biomass_new, biomass_se = b_se_new)


### +1
df_WS3a_bio_a1 <- fn_filterAge(df_2a_WS, 
                               data_bio, 
                               "3a", 
                               age_new == ">=1+")
df_WS3a_bio_a1 <- df_WS3a_bio_a1 |> select(study_area, site, year, species, trt, biomass_new)

# average age
df_WS3a_bio_a1_d <- df_WS3a_bio_a1 |> 
   group_by(study_area, year, site, species, trt) |>
   summarise(biomass = sum(biomass_new, na.rm = T),
             biomass_se = sd(biomass_new, na.rm = T))
df_WS3a_bio_a1_d <- df_WS3a_bio_a1_d |> select(year, species, trt, biomass, biomass_se)
df_WS3a_bio_a1_d$age <- "1+"


# bind and write
df_WS3a_bio_age <- rbind(df_WS3a_bio_yoy, df_WS3a_bio_a1_d)
write.csv(df_WS3a_bio_age, "data_derived/ws_bio_delta_age_1992.csv", row.names = F)


### site ----
## yoy - dplyr

df_WS3a_bio_yoy_site <- df_WS3a_bio_yoy |>
   group_by(study_area, year, species, trt) |>
   summarise(mean_site = mean(biomass, na.rm = T), 
             se_site = sd(biomass, na.rm = T))
df_WS3a_bio_yoy_site


# a1
df_WS3a_bio_a1_pdy <- 
   fn_delta_derivative_site(df_WS3a_bio_a1_d,
                            biomass_se, 
                            "se",
                            trt = trt)
df_WS3a_bio_a1 <- fn_delta_site(df_WS3a_bio_a1_pdy, 
                                biomass,
                                biomass_se,
                                age = "1+")

# bind and write
df_WS3a_bio_site <- rbind(df_WS3a_bio_yoy_site, df_WS3a_bio_a1)
write.csv(df_WS3a_bio_site, "data_derived/ws_bio_delta_site_1992.csv", row.names = F)

## SJ - bio ----
# Riffles only
df_2a_SJ <- df_med2 |> filter(
   study_area == "St. John's" & data_bio == "3a")

df_2a_SJ_age <- df_2a_SJ |>
   select(study_area, year, species, site, age, density_new, biomass_new, )

write.csv(df_2a_SJ_age, "data_derived/sj_bio_delta_age_1995_1996.csv", row.names = F)

# site
df_2a_SJ_den_site <- df_2a_SJ_age |>
   group_by(study_area, year, species, age) |>
   summarise(density = mean(density_new, na.rm = T),
             density_se = sd(density_new, na.rm = T)
             )
write.csv(df_2a_SJ_den_site, "data_derived/sj_den_delta_site_1995_1996.csv", row.names = F)   

# site
df_2a_SJ_bio_year <- df_2a_SJ_age |>
   group_by(study_area, species, age) |>
   summarise(biomass = mean(biomass_new, na.rm = T),
             biomass_se = sd(biomass_new, na.rm = T)
   )
write.csv(df_2a_SJ_bio_year, "data_derived/sj_bio_delta_bio_year_1995_1996.csv", row.names = F) 

## 3b ----
### age - Trepassey - pools ----
### pools
## there is only 3b for Trepassy so the below works.
df_3b_den_yoy <- fn_filterAge(df_med2, 
                              data_den, 
                              "3b", 
                              age == 0)
df_3b_den_yoy <- df_3b_den_yoy |> 
   select(study_area, year, species, density_new, d_se_new, biomass_new, b_se_new, age_new) |>
   rename(density = density_new, density_se = d_se_new, biomass = biomass_new, biomass_se = b_se_new, age = age_new)
df_3b_den_yoy$age <- "0"

df_3b_den_a1 <- fn_filterAge(df_med2, 
                             data_den, 
                             "3b", 
                             age > 0)
df_3b_den_a1$age <- "1+"

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
df_3b_age <- rbind(df_3b_den_yoy, df_3b_den_a1_d)
write.csv(df_3b_age, "data_derived/tp_pool_delta_age_1984_1996.csv", row.names = F)


# year for YOY
df_3b_yr_yoy <- df_3b_den_yoy |>
   group_by(study_area, species) |>
   summarise(density_new = mean(density, na.rm = T),
             density_se = sd(density, na.rm = T),
             biomass_new = mean(biomass, na.rm = T),
             biomass_se = sd(biomass, na.rm = T)
   )
df_3b_yr_yoy$age <- "0"

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
df_3b_year_den <- rbind(df_3b_yr_yoy_d, df_3b_year_den_a1)
write.csv(df_3b_year_den, "data_derived/tp_pool_den_delta_yr_1984_1996.csv", row.names = F)

df_3b_year_bio <- rbind(df_3b_yr_yoy_b, df_3b_year_bio_a1)
write.csv(df_3b_year_bio, "data_derived/tp_pool_bio_delta_yr_1984_1996.csv", row.names = F)


### SJ pool ----
## there is only 3b for Trepassy so the below works.
## density
df_3b_SJ <- df_med2 |> filter(
   study_area == "St. John's" & trt == "Pool")

df_3b_SJ <- df_3b_SJ |> select(study_area, year, species, site, density_new, dll_new, dul_new, biomass_new, bll_new, bul_new, age_new)
write.csv(df_3b_SJ, "data_derived/sj_pool_delta_age_1995_1996.csv", row.names = F)
# don't go any further - there are zeros for YOY in 1996 and not adults in 1995 - this makes 



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
# density
df_2c_GG <- df_low1 |> filter(
   study_area == "Great Gull Brook")

write.csv(df_2c_GG, "data_derived/GG_age_1995_1996.csv", row.names = F)

### sites ----
df_2c_den_pds <- fn_delta_derivative_site(df_2c_GG, d_se_new, "se")
df_2c_den_pds$age <- "all"

df_2c_den_site <- fn_delta_site(df_2c_den_pds, density_new, d_se, age = "all")
write.csv(df_2c_den_site, "data_derived/GG_den_site_1995_1996.csv", row.names = F)

### years ----
# this converts se to var and does partial derivative
## this matches EXCEL
df_2c_den_pdy <- fn_delta_derivative_year(df_2c_den_site, se_site, "se")

df_2a_den <- fn_delta_year(df_2c_den_pdy, mean_site, se_site, age = "all")

str(df_2a_den_a1_pdy, give.attr = F)

## write and bind
write.csv(df_2a_den, "data_derived/GG_den_yr_1995_1996.csv", row.names = F)


# biomass

### sites ----
df_2c_bio_pds <- fn_delta_derivative_site(df_2c_GG, b_se_new, "se")
df_2c_bio_pds$age <- "all"

df_2c_bio_site <- fn_delta_site(df_2c_bio_pds, biomass_new, b_se, age = "all")
write.csv(df_2c_bio_site, "data_derived/GG_bio_site_1995_1996.csv", row.names = F)

### years ----
# this converts se to var and does partial derivative
## this matches EXCEL
df_2c_bio_pdy <- fn_delta_derivative_year(df_2c_bio_site, se_site, "se")

df_2a_bio <- fn_delta_year(df_2c_bio_pdy, mean_site, se_site, age = "all")


## write and bind
write.csv(df_2a_bio, "data_derived/GG_bio_yr_1995_1996.csv", row.names = F)



### age - JumpB ----
df_2c_JumpB <- df_med2 |> filter(
   study_area == "Jumpers Brook") |>
   mutate(den_ll = density_new - d_se_new*1.96,
          den_ul = density_new + d_se_new*1.96) |>
   select(study_area, year, species, site, age, density_new, d_se_new, den_ll, den_ul)

write.csv(df_2c_JumpB, "data_derived/JB_age_2017_2018.csv", row.names = F)

### sites ----
df_JB2c_den_pds <- 
   fn_delta_derivative_site(df_2c_JumpB, 
                            d_se_new, 
                            "se")

df_JB2c_den_site <- fn_delta_site(df_JB2c_den_pds, 
                                  density_new,
                                  d_se_new,
                                  age = "all")

write.csv(df_JB2c_den_site, "data_derived/JB_site_2017_2018.csv", row.names = F)

### years ----
# this converts se to var and does partial derivative
df_JB2c_den_pdy <- 
   fn_delta_derivative_year(df_JB2c_den_site, 
                                          se_site, 
                                          "se")
df_JB2c_den <- fn_delta_year(df_JB2c_den_pdy, 
                           mean_site,
                           se_site,
                           age = "all")

str(df_JB2c_den, give.attr = F)

## write and bind
write.csv(df_JB2c_den, "data_derived/JB2c_yr_2017_2018.csv", row.names = F)


## 4 -----
### Corner Brook (behind copper lake), Indian Bay, Gander River
df_CB_IB_GR_age <- df_low1 |>
   filter(data.quality == "4")
write.csv(df_CB_IB_GR_age, "data_derived/CB_IB_GB_age_2000.csv", row.names = F)

df_CB_IB_GR <- df_low1 |>
   filter(data.quality == "4") |>
   group_by(study_area, species) |>
   summarise(density = mean(density_new), 
             biomass = mean(biomass_new))
write.csv(df_CB_IB_GR, "data_derived/CB_IB_GB_site_2000.csv", row.names = F)
          
## 5 -----
df_TN_site <- df_low1 |>
   filter(data.quality == "5a") 
# this is not aggregated by site but rather the data from Cote 2007
write.csv(df_TN_site, "data_derived/TN_site_2002.csv", row.names = F)

df_TN_year <- df_low1 |>
   filter(data.quality == "5a") |>
   group_by(study_area, year, species) |>
   summarise(density = mean(density_new), 
             biomass = mean(biomass_new))

write.csv(df_TN_year, "data_derived/TN_yr_2002.csv", row.names = F)



df_TI_site <- df_low1 |>
   filter(data.quality == "5b")
write.csv(df_TI_site, "data_derived/TI_site_2006_2010.csv", row.names = F)


df_TI_year <- df_low1 |>
   filter(data.quality == "5b") |>
   group_by(study_area, species) |>
   summarise(density = mean(density_new))

write.csv(df_TI_year, "data_derived/TI_yr_2006_2010.csv", row.names = F)

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
