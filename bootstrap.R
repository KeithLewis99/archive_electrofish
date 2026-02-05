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
## import data ----
df_med <- read.csv("data/export_med.csv")
str(df_med)
unique(df_med$species)
unique(df_med$study_area)
unique(df_med$age)

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

df_2b_den_yoy <- fn_filterAge(df_med2, data_den, "2b", age == 0)
df_2b_den_a1 <- fn_filterAge(df_med2, data_den, "2b", age > 0)
str(df_2b_den_a1)

# delta Age1 - density_new, d_se_new
df_2b_den_a1_d <- fn_delta_Age(df_2b_den_a1, var2 = density_new, var3 = d_se_new)
str(df_2b_den_a1_d, give.attr = F)

df_2b_den_yoy |> select(study_area, year, species, trt, )
df_2b_den_site <- rbind(df_2b_den_yoy, df_2b_den_a1_d)

# year: partial derivaties
df_2b_den_yoy_pdy <- fn_delta_derivative_year(df_2b_den_yoy, d_se_new, "se")
str(df_2b_den_yoy_pdy, give.attr = F)
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
df_2b_den_a1_pdy <- fn_delta_derivative_year(df_2b_den_a1_d, dsum_var, "var")
str(df_2b_den_a1_pdy, give.attr = F)

# START HERE ----
## COMBINE YOY AND A1
# delta
df_2b_yoy_den_yr <- fn_delta_year(df_2b_den_yoy_pdy, density_new)
df_2b_yoy_den_yr$age <- "YOY"
df_2b_a1_den_yr <- fn_delta_year(df_2b_den_a1_pdy, dsum)
df_2b_a1_den_yr$age <- "1+"

df_2b_den <- rbind(df_2b_yoy_den_yr, df_2b_a1_den_yr)

# this matches EXCEL -          

### 2b biomass ----
# filter by age and data type
df_2b_bio_yoy <- fn_filterAge(df_med2, data_bio, "2b", age == 0)
df_2b_bio_a1 <- fn_filterAge(df_med2, data_bio, "2b", age > 0)
str(df_2b_bio_a1)

# delta Age1
df_2b_bio_da1 <- fn_delta_Age(df_2b_bio_a1, var2 = biomass_new, var3 = b_se_new)
str(df_2b_bio_da1, give.attr = F)

## this matches EXCEL

# year: partial derivaties
# this converts se to var and does partial derivative
df_2b_bio_yoy_pdy <- fn_delta_derivative_year(df_2b_bio_yoy, d_se_new, "se")
df_2b_bio_a1_pdy <- fn_delta_derivative_year(df_2b_bio_da1, dsum_var, "var")
str(df_2b_bio_a1_pdy, give.attr = F)


# delta
# this takes mean of biomass and variance
df_2b_bio_yoy_yr <- fn_delta_year(df_2b_bio_yoy_pdy, biomass_new)
df_2b_bio_yoy_yr$age <- "YOY"
df_2b_bio_a1_yr <- fn_delta_year(df_2b_bio_a1_pdy, dsum)
df_2b_bio_a1_yr$age <- "1+"

# biomass
df_2b_bio <- rbind(df_2b_bio_yoy_yr, df_2b_bio_a1_yr)



## 2a den ----
# filter ages
### age-CL-JF ----
unique(df_med2$study_area)
df_2a_CL_JF <- df_med2 |> filter(
   study_area == "Copper Lake" | 
      study_area == "Joe Farrell's ")

df_2a_den_yoy <- fn_filterAge(df_2a_CL_JF, data_den, "2a", age_new == "YOY")
df_2a_den_a1 <- fn_filterAge(df_2a_CL_JF, data_den, "2a", age > 0 & age != "YOY") #& age != "YOY"
str(df_2a_den_a1)
unique(df_2a_den_a1$age)
unique(df_2a_den_a1$study_area)

# delta Age1 - density_new, d_se_new
df_2a_den_a1d <- fn_delta_Age(df_2a_den_a1, stations, density_new, d_se_new)
str(df_2a_den_a1d, give.attr = F)


### sites ----
# df_2a_yoy |> select(study_area, year, species, site, stations, age_new, density_new)
# df_2a_yoy_pd <- fn_delta_derivative_site(df_2a_yoy, d_se_new, "YOY")
df_2a_den_yoy_pds <- fn_delta_derivative_site(df_2a_den_yoy, d_se_new, "se")
str(test, give.attr = F)
df_2a_den_yoy_pds |> select(year, site, age_new, density_new, d_se_new, dsite_var)
df_2a_den_yoy_site <- fn_delta_site(df_2a_den_yoy_pds, density_new)

df_2a_den_a1_pds <- fn_delta_derivative_site(df_2a_den_a1d, dsum_var, "var")
df_2a_den_a1_site <- fn_delta_site(df_2a_den_a1_pds, dsum)

### years ----
# this converts se to var and does partial derivative
## this matches EXCEL
df_2a_den_yoy_pdy <- fn_delta_derivative_year(df_2a_den_yoy_site, se_site, "se")
#df_2b_d0pd |> select(year, species, trt, density_new, dyear_var)
df_2a_den_a1_pdy <- fn_delta_derivative_year(
   df_2a_den_a1_site, se_site, "se")
str(df_2a_den_a1_pdy, give.attr = F)

# delta
# this takes mean of density and variance
df_2a_den_yoy <- fn_delta_year(df_2a_den_yoy_pdy, mean_site)
df_2a_den_yoy$age <- "YOY"
df_2a_den_a1 <- fn_delta_year(df_2a_den_a1_pdy, mean_site)
df_2a_den_a1$age <- "1+"

# den
df_2a_den <- rbind(df_2a_den_yoy, df_2a_den_a1) |>
   arrange(study_area)
# biomass for CL is 3a
# no biomass for Joe Farrell's

### age - WS ----
unique(df_med2$study_area)
df_2a_WS <- df_med2 |> filter(
   study_area == "West Salmon River")

unique(df_2a_WS$age_new)
df_2a_den_yoy <- fn_filterAge(df_2a_WS, data_den, "2a", age == "0")
df_2a_den_a1 <- fn_filterAge(df_2a_WS, data_den, "2a", age > 0) #& age != "YOY"
str(df_2a_den_a1)
unique(df_2a_den_a1$age)
unique(df_2a_den_a1$study_area)

# delta Age1 - density_new, d_se_new
df_2a_den_a1d <- fn_delta_Age(df_2a_den_a1, site, density_new, d_se_new)
str(df_2a_den_a1d, give.attr = F)


### sites ----
# df_2a_yoy |> select(study_area, year, species, site, stations, age_new, density_new)
# df_2a_yoy_pd <- fn_delta_derivative_site(df_2a_yoy, d_se_new, "YOY")
df_2a_den_yoy_pds <- fn_delta_derivative_site(df_2a_den_yoy, d_se_new, "se")
df_2a_den_yoy_site <- fn_delta_site(df_2a_den_yoy_pds, density_new)
df_2a_den_yoy_site$age <- "YOY"


df_2a_den_a1_pds <- fn_delta_derivative_site(df_2a_den_a1d, dsum_var, "var")
df_2a_den_a1_site <- fn_delta_site(df_2a_den_a1_pds, dsum)
df_2a_den_a1_site$age <- "1+"

### years ----
# no Years for West Salmon River - just 1992

# den
df_2a_den <- rbind(df_2a_den_yoy_site, df_2a_den_a1_site) |>
   arrange(study_area)

# Low ----
## import data ----
## stdz data ----
df_low <- read.csv("data/export_low.csv")
str(df_low)
unique(df_low$species)
unique(df_low$study_area)

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


## 3a ----
### age -  ----
df_3a_den_yoy <- fn_filterAge(df_2a_CL_JF, data_bio, "3a", age_new == "YOY")
df_3a_den_a1 <- fn_filterAge(df_2a_CL_JF, data_bio, "3a", age > 0 & age != "YOY")

df_3a_den_a1_d <- df_3a_den_a1 |> 
   group_by(study_area, year, species) |>
   summarise(density = sum(density_new, na.rm = T),
             density_se = sd(density_new, na.rm = T),
             biomass = sum(biomass_new, na.rm = T),
             biomass_se = sd(biomass_new, na.rm = T))
df_3a_den_a1_d |> select(year, species, density, density_se, biomass, biomass_se)

# could use the delta method for years
df_3a_den_yoy_site <- df_3a_den_yoy |>
   group_by(study_area, year, species) |>
   summarise(density = mean(density, na.rm = T), 
             biomass = mean(biomass, na.rm = T))
df_3a_den_yoy_site

df_3a_den_a1_site <- df_3a_den_a1 |>
   group_by(study_area, year, species) |>
   summarise(density = mean(density, na.rm = T), 
             biomass = mean(biomass, na.rm = T))
df_3a_den_a1_site


df_3a_yoy_den <- df_3a_den_yoy_site |>
   group_by(study_area, species) |>
   summarise(density = mean(density, na.rm = T), 
             biomass = mean(biomass, na.rm = T))
df_3a_yoy_den

df_3a_a1_den <- df_3a_den_a1_site |>
   group_by(study_area, species) |>
   summarise(density = mean(density, na.rm = T), 
             biomass = mean(biomass, na.rm = T))
df_3a_a1_den

df_3a_den <- rbind(df_3a_yoy_den, df_3a_a1_den)
df_3a_den



## 3b ----
### age - Trepassey ----
df_3b_den_yoy <- fn_filterAge(df_med2, data_den, "3b", age == 0)
df_3b_den_a1 <- fn_filterAge(df_med2, data_den, "3b", age > 0)

df_3b_den_a1_d <- df_3b_den_a1 |> filter(
   study_area == "Northeast Brook, Trepassey") |>
   group_by(study_area, year, species) |>
   summarise(density = sum(density_new),
             density_se = sd(density_new),
             biomass = sum(biomass_new),
             biomass_se = sd(biomass_new))
df_3b_den_a1_d |> select(year, species, density, density_se, biomass, biomass_se)

# could use the delta method for years
df_3b_yr <- df_3b_den_a1_d |>
   group_by(study_area, species) |>
   summarise(density = mean(density), 
             biomass = mean(biomass, na.rm = T))
df_3b_yr

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

# START HERE #####

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