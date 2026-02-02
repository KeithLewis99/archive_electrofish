# this file is to produce bootstrap estimates of the stoney brook data to get density and biomass estimates and ultimately, a table of all the electrofishing data.
## see table on slide 6 of m9-2026-01-09.ppt in synthesis folder.

# Data rank 1 ----
# Hmisc - ben bolker approach
#https://stackoverflow.com/questions/38554383/bootstrapped-confidence-intervals-with-dplyr
library(Hmisc)
library(dplyr)
source("delta_function.R")

df_sb <- read.csv("../data/stoneybrook/stoney2001_2003.csv")
str(df_sb)

unique(df_sb$Species) # "AS"      "BT"      "BN"      "SMELT"   "ASYOY"   "BNYOY"   "EEL"    "BTYOY"   "EELS"    "STICKLE"
unique(df_sb$Site) # all "Stoney"
unique(df_sb$Station) # all OK - "R1"   "R2"   "R3"   "R4"   "F1"   "F2"   "POOL"

# filter out unneeded species
species_filter <- c("BN", "SMELT", "BNYOY", "EEL", "EELS", "STICKLE")
df_sb <- df_sb |> filter(!Species %in% species_filter)

df_sum <- df_sb |>
   filter(Sweep <= 3) |>
   group_by(Year, Species, Station, Area) |>
   summarise(bio.sum = sum(Weight.g, na.rm = T), abun = n()) |>
   mutate(abun.stand = abun/Area*100, bio.stand = bio.sum/Area*100)


# boostrap
spp.den.boot.ci <- df_sum |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)


spp.bio.boot.ci <- df_sum |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)

sb.boot.ci <- full_join(spp.den.boot.ci, spp.bio.boot.ci, by = c("Year", "Species")) |>
   rename(density = mean.x, den.ll = ll.x, den.ul = ul.x,
          biomass = mean.y, bio.ll = ll.y, bio.ul = ul.y)
sb.boot.ci
write.csv(sb.boot.ci, "../data/stoneybrook/bootstrap_ci_2001_2003.csv", row.names = F)


# Data rank 2 ----
## stdze data ----
## bootstrap bc no variance at the site level so at least have it within year, species, and age
df_med <- read.csv("data/export_med.csv")
str(df_med)
unique(df_med$species)
unique(df_med$age)

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
             !(age == "1 to 6")) 
unique(df_med1a$age)
unique(df_med1a$area)



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
                           "AS" = "SAL", 
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

df_2b_yoy <- fn_filterAge(df_med2, data_den, "2b", age == 0)
df_2b_1 <- fn_filterAge(df_med2, data_den, "2b", age > 0)
str(df_2b_1)

# delta Age1 - density_new, d_se_new
df_2b_d1 <- fn_delta_Age(df_2b_1, density_new, d_se_new)
str(df_2b_d1, give.attr = F)

# this converts se to var and does partial derivative
## this matches EXCEL
# test1 <- test |>
#    group_by(species, age, trt) |>
#    mutate(dvar = (d_se_new)^2*1/n()) 
# 
# test1 |> 
#    select(year, species, age, density, density_new, d_se_new, dvar) 

# year: partial derivaties
# temp3 <- fn_delta_derivative_site(df_2b_yoy)
#df_2b_yoy |> select(year, species, trt, density_new, d_se_new)
df_2b_d0pd <- fn_delta_derivative_year(df_2b_yoy, d_se_new, "YOY")
str(df_2b_d0pd, give.attr = F)
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
df_2b_d1pd <- fn_delta_derivative_year(df_2b_d1, dsum_var, "a1")
str(df_2b_d1pd, give.attr = F)

# this takes mean of density and variance
# test2 <- test1 |>
#    group_by(species, age, trt) |>
#    summarise(
#       mean_site = mean(density_new, na.rm = T),
#       var_site = sqrt(mean(dvar, na.rm = T)),
#       ll_site = mean_site - var_site*1.96,
#       ul_site = mean_site + var_site*1.96
#       )
# test2

# delta
df_2b_yoy_den <- fn_delta_year(df_2b_d0pd, density_new)
df_2b_yoy_den$age <- "YOY"
df_2b_a1_den <- fn_delta_year(df_2b_d1pd, dsum)
df_2b_a1_den$age <- "1+"

df_2b_den <- rbind(df_2b_yoy_den, df_2b_a1_den)

# this matches EXCEL -          

### 2b biomass ----
# filter by age and data type
df_2b_yoy_bio <- fn_filterAge(df_med2, data_bio, "2b", age == 0)
df_2b_a1_bio <- fn_filterAge(df_med2, data_bio, "2b", age > 0)
str(df_2b_1)

# delta Age1
df_2b_d1 <- fn_delta_Age(df_2b_1, biomass_new, b_se_new)
str(df_2b_d1, give.attr = F)

## this matches EXCEL

# year: partial derivaties
# this converts se to var and does partial derivative
df_2b_d0pd_bio <- fn_delta_derivative_year(df_2b_yoy, d_se_new, "YOY")
df_2b_d1pd_bio <- fn_delta_derivative_year(df_2b_d1, dsum_var, "a1")
str(df_2b_d1pd_bio, give.attr = F)


# delta
# this takes mean of biomass and variance
df_2b_yoy_bio <- fn_delta_year(df_2b_d0pd_bio, biomass_new)
df_2b_yoy_bio$age <- "YOY"
df_2b_a1_bio <- fn_delta_year(df_2b_d1pd_bio, dsum)
df_2b_a1_bio$age <- "1+"

# biomass
df_2b_bio <- rbind(df_2b_yoy_bio, df_2b_a1_bio)



## 2a ----
# filter ages
### age ----
temp2a <- df_med2 |> filter(study_area == "Copper Lake")
df_2a_yoy <- fn_filterAge(temp2a, data_den, "2a", age_new == "YOY")
df_2a_1 <- fn_filterAge(temp2a, data_den, "2a", age > 0 & age != "YOY") #& age != "YOY"
str(df_2a_1)
unique(df_2a_1$age)
unique(df_2a_1$study_area)

# delta Age1 - density_new, d_se_new
df_2a_1d <- fn_delta_Age(df_2a_1, stations, density_new, d_se_new)
str(df_2a_1d, give.attr = F)


### sites ----
df_2a_yoy |> select(study_area, year, species, site, stations, age_new, density_new)
df_2a_yoy_pd <- fn_delta_derivative_site(df_2a_yoy, d_se_new, "YOY")
df_2a_yoy_pd <- fn_delta_derivative_site(df_2a_yoy, d_se_new, "se")

str(test, give.attr = F)
df_2a_yoy_pd |> select(year,site, age_new, density_new, d_se_new, dsite_var)

df_2a_yoy_site <- fn_delta_site(df_2a_yoy_pd, dsite_var)

df_2a_a1_pd <- fn_delta_derivative_site(df_2a_yoy, d_se_new, "a1")
df_2a_a1_site <- fn_delta_site(df_2a_a1_pd, dsite_var)

### years ----
# this converts se to var and does partial derivative
## this matches EXCEL
df_2a_d0pd <- fn_delta_derivative_year(df_2a_yoy_site, var_site, "a1")
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
df_2a_d1pd <- fn_delta_derivative_year(df_2a_a1_site, var_site, "a1")
str(df_2a_d1pd)

# delta
# this takes mean of density and variance
df_2a_yoy_den <- fn_delta_year(df_2a_d0pd, mean_site)
df_2a_yoy_den$age <- "YOY"
df_2a_a1_den <- fn_delta_year(df_2a_d1pd, mean_site)
df_2a_a1_den$age <- "1+"

# den
df_2a_den <- rbind(df_2a_yoy_den, df_2a_a1_den)


############ START HERE###### 

df_med3 <- df_med2 |>
   group_by(study_area, site, year, species, trt, age, data_den, data_bio) |>
   summarise(density_sum = sum(density, na.rm = T), 
             biomass_sum = sum(biomass, na.rm = T), 
             )

df_med3 |> print(n = Inf)
str(df_med3)
unique(df_med3$study_area)

dat3_4.bio.boot <- df_med3 |> 
   filter(data_bio == "3") |>
   group_by(study_area, year, species, age, trt) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$biomass_sum)))) |>
   rename(bio = Mean, bll = Lower, bul = Upper)
unique(dat3_4.bio.boot$study_area)

dat3_4.den.boot <- df_med3 |> 
   filter(data_den == "3") |>
   group_by(study_area, year, species, age, trt) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$density_sum)))) |>
   rename(density = Mean, dll = Lower, dul = Upper)
unique(dat3_4.den.boot$study_area)

dat_3_4.boot <- full_join(dat3_4.bio.boot, dat3_4.den.boot, by = c( "study_area", "year", "species", "age", "trt")) 
dat_3_4.boot |> print(n = Inf)

unique(dat_3_4.boot$study_area)
write.csv(dat_3_4.boot, "data_derived/dat_3_4.boot.csv", row.names = F)


# Data rank 2-5 ----
## delta method bc variance at the site level plus for year, age and species when available

# Data rank 6-7 ----
# just raw data to be fed into table

# Create table ----