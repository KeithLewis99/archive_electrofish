# After a great deal of reflection (mid-March, 2026), I have decided that all the work on bootstrapping and the delta method is just not appropriate.  See the tech report.  

# Briefly, the density and biomass estimates for each time series often contain many zeros and a wide range of values (e.g., high variance) values, e.g., GC.  As such, the lower confidence levels for density and biomass estimates are often less than zero when calculated by assuming a normal distribution but the uppper limit is very, very large when using the log-normal.  Bootstrap is biased bc of low sample sizes.  The same issues apply with the Delta Method.  

# Therefore, simply present the minimum and maximum along with a mean if the sample size warranted (MMM).  

## this file is for the study areas with the extracted data

# library ----
library(dplyr)
library(forcats)
source("fn_MMM.R")


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
str(df_med2)
df_2b_TP <- df_med2 |> filter(
   study_area == "Northeast Brook, Trepassey")


TP_age <- fn_age(df_2b_TP, file_name = "TP_age_1984_1996")
TP_yr <- fn_yr(df_2b_TP, file_name = "TP_yr__1984_1996")



## 2a den ----
# filter ages
### CL-age ----
unique(df_med2$study_area)
df_2a_CL <- df_med2 |> filter(
   study_area == "Copper Lake")

CL_age <- fn_age(df_2a_CL, group_by_var1 = stations, file_name = "CL_age_1993_1995")
CL_site <- fn_site(df_2a_CL, file_name = "CL_site_1993_1995")
CL_yr <- fn_yr(df_2a_CL, file_name = "CL_yr_1993_1995")




### JF-age ----
df_2a_JF <- df_med2 |> filter(
   study_area == "Joe Farrell's ")
unique(df_2a_JF$year)

JF_age <- fn_age(df_2a_JF, 
                 group_by_var1 = site, 
                 trt = trt,
                 file_name = "JF_age_1993_1995")
JF_site <- fn_site(df_2a_JF, trt = trt, file_name = "JF_site_1993_1995")
JF_yr <- fn_yr(df_2a_JF, trt = trt, file_name = "JF_yr_1993_1995")


### WS-age ----
unique(df_med2$study_area)
df_2a_WS <- df_med2 |> filter(
   study_area == "West Salmon River")
unique(df_2a_WS$year)

WS_age <- fn_age(df_2a_WS, 
                 group_by_var1 = site, 
                 trt = trt,
                 file_name = "WS_age_1992")
WS_site <- fn_site(df_2a_WS, trt = trt, file_name = "WS_site_1992")


### SJ-age ----
df_2a_SJ80 <- df_med2 |> filter(
   study_area == "St. John's" & year <= 1985)
unique(df_2a_SJ80$year)


SJ_age80 <- fn_age(df_2a_SJ80, 
                 group_by_var1 = site, 
                 file_name = "SJ_age_1982_1985")
SJ_site80 <- fn_site(df_2a_SJ80, trt = trt, file_name = "SJ_site_1982_1985")
SJ_yr80 <- fn_yr(df_2a_SJ80, trt = trt, file_name = "SJ_yr_1982_1985")





## 3a-Highlands ----

df_3a_HL <- df_med2 |> filter(
   study_area == "Highlands")
unique(df_3a_HL$year)

HL_age <- fn_age(df_3a_HL, 
                 group_by_var1 = site,
                 trt = trt,
                 file_name = "HL_age_2002")
HL_site <- fn_site(df_3a_HL, trt = trt, file_name = "HL_site_2002")
HL_yr <- fn_yr(df_3a_HL, trt = trt, file_name = "HL_yr_2002")



### SJ90s-----

df_2a_SJ90 <- df_med2 |> filter(
   study_area == "St. John's" & year >= 1995)
unique(df_2a_SJ90$year)


SJ_age90 <- fn_age(df_2a_SJ90, 
                   group_by_var1 = site, 
                   file_name = "SJ_age_1995_1996")
SJ_site90 <- fn_site(df_2a_SJ90, trt = trt, file_name = "SJ_site_1995_1996")
SJ_yr90 <- fn_yr(df_2a_SJ90, trt = trt, file_name = "SJ_yr_1995_1996")



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
df_GG <- df_low1 |> filter(
   study_area == "Great Gull Brook")
unique(df_GG$year)


df_GG_site <- df_GG |>
   group_by(study_area, species, year) |>
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

write(df_GG_site, "data_derived/mmm/GG_site_1997_1998.csv", row.names = F)

GG_yr <- df_GG |>
   group_by(study_area, species) |>
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
write(df_GG_yr, "data_derived/mmm/GG_yr_1997_1998.csv", row.names = F)

### age - JumpB ----
df_2c_JumpB <- df_med2 |> filter(
   study_area == "Jumpers Brook")

JB_yr <- fn_yr(df_2c_JumpB, trt = trt, file_name = "JumpB_yr_2017_2018")


## 4 -----
### Corner Brook (behind copper lake), Indian Bay, Gander River
df_CB_IB_GR_age <- df_low1 |>
   filter(data.quality == "4")

df_CB_IB_GR <- df_low1 |>
   filter(data.quality == "4") |>
   group_by(study_area, species) |>
   summarise(density = mean(density_new), 
             biomass = mean(biomass_new))
write.csv(df_CB_IB_GR, "data_derived/mmm/CB_IB_GB_site_2000.csv", row.names = F)


## 5 -----
df_TN_site <- df_low1 |>
   filter(data.quality == "5a") 

df_TN_year <- df_low1 |>
   filter(data.quality == "5a") |>
   group_by(study_area, year, species) |>
   summarise(density = mean(density_new), 
             biomass = mean(biomass_new))

write.csv(df_TN_year, "data_derived/mmm/TN_yr_2002.csv", row.names = F)



df_TI_site <- df_low1 |>
   filter(data.quality == "5b")

df_TI_year <- df_low1 |>
   filter(data.quality == "5b") |>
   group_by(study_area, species) |>
   summarise(density = mean(density_new))
write.csv(df_TI_year, "data_derived/mmm/TI_yr_2006_2010.csv", row.names = F)
# END ----