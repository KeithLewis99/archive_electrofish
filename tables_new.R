# the purpose of this file is to create tables of density and biomass estimates including the CIs for bootstrap, deltamethod, and simpler files.

# library ----
library(dplyr)
library(kableExtra)
library(purrr)
library(tidyr)

# function ----
# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/MMM/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}


## age ---- 
### load files ----
# create pattern
temp1 = list.files(path = "data_derived/MMM", pattern=".+_age.+.csv$", full.names = T)

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_age = (lapply(temp1, read.csv))
names(ls_age) <- name_files(temp1)
list2env(ls_age, envir = .GlobalEnv)
str(ls_age, 1)

# fix a few dfs
ls_age$CL_age_1993_1995 <- ls_age$CL_age_1993_1995 |>
   rename(site = stations)

ls_age$TP_age_1984_1996$site <- NA

# convert variable type
ls_age <- map(ls_age, ~ {
   if ("site" %in% names(.x)) {
      mutate(.x, site = as.character(site))
   } else {
      .x
   }
})


df_age <- bind_rows(ls_age)

### import ----
# import SB
df_SB <- read.csv("data_derived/mmm/SB_dage_2001_2003.csv")
df_SB1 <- df_SB[, c(1:3, 7, 8)] |>
   rename(year = Year, species = Species, site = Station, mean_den = abun.stand, mean_bio = bio.stand)
df_SB1$study_area <- "Stoney Brook"

df_age <- bind_rows(df_age, df_SB1)

# import HLTP
df_HLTP <- read.csv("data_derived/mmm/HLTP_dage_2012_2018.csv")
df_HLTP1 <- df_HLTP[, c(1:4, 8, 9)] |>
   rename(study_area = Study_area, year = Year, species = Species, site = Stn_no, mean_den = abun.stand, mean_bio = bio.stand)
df_HLTP1$site <- as.character(df_HLTP1$site)

df_age <- bind_rows(df_age, df_HLTP1)
write.csv(df_age, "output/df_age.csv", row.names = F)

# make table
tab_age <- kbl(df_age, 
                col.names = c('Study_area', 'Species', 'Year', 'Site', 'trt', 'Age',
                              'n', 'min', 'max', 'mean', 'sd',
                              'min', 'max', 'mean', 'sd'),
                align = 'c', 
               caption = "Density and Biomass by Station", 
               digits = 3, booktabs = TRUE, longtable = TRUE) |>
   collapse_rows(valign = "top", latex_hline = "major") |>
   add_header_above(header = c(" " = 6, "Density" = 5, "Biomass" = 4)) |>
   kable_paper()

save_kable(tab_age, file = "output/tab_age.html")




## site ---- 
### load files ----
# create pattern
temp2 = list.files(path = "data_derived/MMM", pattern=".+_site.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_site = (lapply(temp2, read.csv))
names(ls_site) <- name_files(temp2)
list2env(ls_site, envir = .GlobalEnv)
str(ls_site, 1)
ls_site[1]

# fix
ls_site$CB_IB_GB_site_2000$year <- 2002
ls_site$HLTP_site_2012_2018 <- ls_site$HLTP_site_2012_2018 |>
   rename(study_area = Study_area, year = Year, species = Species)

ls_site$SB_site_2001_2003$study_area <- "Stoney Brook"
ls_site$SB_site_2001_2003 <- ls_site$SB_site_2001_2003 |>
   rename(year = Year, species = Species)


# bind
df_site <- bind_rows(ls_site)
df_site <- df_site[, c(1:2, 5:10, 3, 11:13, 4, 14)]
write.csv(df_site, "output/df_site.csv", row.names = F)

# table
tab_site <- kbl(df_site, 
               col.names = c('Study_area', 'Species', 'Year', 'trt', 'Age',
                             'n', 'min', 'max', 'mean', 'sd',
                             'min', 'max', 'mean', 'sd'),
               align = 'c', 
               caption = "Density and Biomass by Site", 
               digits = 3, booktabs = TRUE, longtable = TRUE) |>
   collapse_rows(valign = "top", latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 5, "Biomass" = 4)) |>
   kable_paper()

save_kable(tab_site, file = "output/tab_site.html")



## year ---- 
### load files ----
# create pattern
temp3 = list.files(path = "data_derived/MMM", pattern=".+_yr.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_year = (lapply(temp3, read.csv))
names(ls_year) <- name_files(temp3)
list2env(ls_year, envir = .GlobalEnv)
str(ls_year, 1)


df_year <- bind_rows(ls_year)
df_year <- df_year[, c(1:2, 5:9, 3, 10:12, 4, 13)]

# import SB
df_SB_yr <- read.csv("data_derived/mmm/SB_year_2001_2003.csv")
df_SB_yr$study_area <- "Stoney Brook"
df_SB_yr <- df_SB_yr |>
   rename(species = Species)

df_year <- bind_rows(df_year, df_SB_yr)

# import HLTP
df_HLTP_yr <- read.csv("data_derived/mmm/HLTP_year_2012_2018.csv")
df_HLTP_yr <- df_HLTP_yr |>
   rename(study_area = Study_area, species = Species)
df_year <- bind_rows(df_year, df_HLTP_yr)

write.csv(df_year, "output/df_year.csv", row.names = F)

# table
tab_year <- kbl(df_year, 
                col.names = c('Study_area', 'Species', 'trt', 'Age',
                              'n', 'min', 'max', 'mean', 'sd',
                              'min', 'max', 'mean', 'sd'),
                align = 'c', 
                caption = "Density and Biomass by Year", 
                digits = 3, booktabs = TRUE, longtable = TRUE) |>
   collapse_rows(valign = "top", latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 5, "Biomass" = 4)) |>
   kable_paper()

save_kable(tab_year, file = "output/tab_year.html")


### density ----

df_year_den <- df_year |>
   filter(!is.na(age_new)) |>
   group_by(study_area, species, age_new, trt) |>
   summarise(min = min(mean_den),
             max = max(mean_den)) |>
   #print(n = Inf)
   pivot_wider(id_cols = c(study_area, trt),
               names_from = c(species,age_new),
               values_from = c(min, max)
   )

df_year_den_min <- df_year |>
    filter(!is.na(age_new)) |>
   #filter(!is.na(age_new) & trt != "pool") |>
   group_by(species, age_new) |>
   summarise(min = min(mean_den, na.rm = T)) 


df_year_den_max <- df_year |>
    filter(!is.na(age_new)) |>
   #filter(!is.na(age_new) & trt != "pool") |>
   group_by(species, age_new) |>
   summarise(max = max(mean_den, na.rm = T)) 

df_year_den_min <- left_join(df_year_den_min, df_year[, c(1:4, 8)], by = c('species', 'age_new', 'min' = 'mean_den'))
df_year_den_max <- left_join(df_year_den_max, df_year[, c(1:4, 8)], by = c('species', 'age_new', 'max' = 'mean_den'))


### biomass ----
df_year_bio <- df_year |>
   filter(!is.na(age_new)) |>
   group_by(study_area, species, age_new, trt) |>
   summarise(min = min(mean_bio),
             max = max(mean_bio)) |>
   #print(n = Inf)
   pivot_wider(id_cols = c(study_area, trt),
               names_from = c(species,age_new),
               values_from = c(min, max)
   )


df_year_bio_min <- df_year |>
   filter(!is.na(age_new)) |>
   #filter(!is.na(age_new) & trt != "pool") |>
   group_by(species, age_new) |>
   summarise(min = min(mean_bio, na.rm = T)) 


df_year_bio_max <- df_year |>
   filter(!is.na(age_new)) |>
   #filter(!is.na(age_new) & trt != "pool") |>
   group_by(species, age_new) |>
   summarise(max = max(mean_bio, na.rm = T)) 

df_year_bio_min <- left_join(df_year_bio_min, df_year[, c(1:4, 12)], by = c('species', 'age_new', 'min' = 'mean_bio'))
df_year_bio_max <- left_join(df_year_bio_max, df_year[, c(1:4, 12)], by = c('species', 'age_new', 'max' = 'mean_bio'))


# End ----