# the purpose of this file is to create tables of density and biomass estimates including the CIs for bootstrap, deltamethod, and simpler files.

# load packages
library(dplyr)
library(kableExtra)
library(purrr)

# boot ----
## age ---- 
### load files ----
# create pattern
temp1 = list.files(path = "data_derived/MMM", pattern=".+_age.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/MMM/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

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
write.csv(df_age, "data_derived/MMM/df_age.csv", row.names = F)


# make table
tab_test <- kbl(temp[, c(10, 1:4, 6, 7, 5, 8)], 
                col.names = c('Study_area', 'Year', 'Species', 'Station', 'Area',
                              'abundance', 'density', 'biomass', 'biomass/area'),
                align = 'c', caption = "Density and Biomass (bootstrap) by Station", digits = 3, booktabs = TRUE, longtable = TRUE) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 2, "Biomass" = 2)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab_test, file = "output/tab_boot_age_HL_SB_TP.html")




## site ---- 
### load files ----
# create pattern
temp2 = list.files(path = "data_derived/MMM", pattern=".+_site.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/MMM/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_site = (lapply(temp2, read.csv))
names(ls_site) <- name_files(temp2)
list2env(ls_site, envir = .GlobalEnv)
str(ls_site, 1)


df_site <- bind_rows(ls_site)
write.csv(df_site, "data_derived/MMM/df_site.csv", row.names = F)


## year ---- 
### load files ----
# create pattern
temp3 = list.files(path = "data_derived/MMM", pattern=".+_yr.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/MMM/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_year = (lapply(temp3, read.csv))
names(ls_year) <- name_files(temp3)
list2env(ls_year, envir = .GlobalEnv)
str(ls_year, 1)


df_year <- bind_rows(ls_year)
write.csv(df_year, "data_derived/MMM/df_year.csv", row.names = F)

