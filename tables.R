# the purpose of this file is to create tables of density and biomass estimates including the CIs for bootstrap, deltamethod, and simpler files.

# load packages
library(dplyr)

# disagg ---- 
## load files ----
# create pattern
temp1 = list.files(path = "output", pattern=".+disagg.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*output/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_disagg = (lapply(temp1, read.csv))
names(ls_disagg) <- name_files(temp1)
list2env(ls_disagg, envir = .GlobalEnv)


# agg ----
HL_disagg_2013_2019$type <- "dis_agg"
SB_disagg_2001_2003$type <- "dis_agg"
TP_disagg_2012_2018$type <- "dis_agg"

HL_disagg_2013_2019$study_area <- "Highlands"
SB_disagg_2001_2003$study_area <- "Stoney_Brook"
TP_disagg_2012_2018$study_area <- "Trepassey"

SB_disagg_2001_2003 <- SB_disagg_2001_2003 |> rename(Stn_no = Station)

temp <- rbind(HL_disagg_2013_2019,
              SB_disagg_2001_2003,
              TP_disagg_2012_2018)


library(kableExtra)
kbl(temp[, c(10, 1:4, 6, 7, 5, 8)], 
    col.names = c('Study_area', 'Year', 'Species', 'Station', 'Area',
                  'biomass', 'abundance', 'density', 'biomass/area'),
    align = 'c', caption = "Density and Biomass by Station", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 2, "Biomass" = 2)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()


# load files ----
# create pattern
temp1 = list.files(path = "output", pattern=".+boot.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*output/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_sc_ci1 = (lapply(temp1, read.csv))
names(ls_sc_ci1) <- name_files(temp1)
list2env(ls_sc_ci1, envir = .GlobalEnv)


# agg ----
hl_boot_ci_agg_2012_2018$type <- "agg"
SB_boot_ci_agg_2001_2003$type <- "agg"
TP_boot_ci_agg_2012_2018$type <- "agg"

hl_boot_ci_agg_2012_2018$study_area <- "Highlands"
SB_boot_ci_agg_2001_2003$study_area <- "Stoney_Brook"
TP_boot_ci_agg_2012_2018$study_area <- "Trepassey"


temp <- rbind(hl_boot_ci_agg_2012_2018,
              SB_boot_ci_agg_2001_2003,
              TP_boot_ci_agg_2012_2018)


library(kableExtra)
kbl(temp[, c(10, 1:8)], 
    col.names = c('Study_area', 'Year', 'Species', 
                  'density', '2.5%', '97.5%',
                  'biomass', '2.5%', '97.5%'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 3, "Density" = 3, "Biomass" = 3)) |>
#   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()



# yr-agg ----
hl_boot_ci_yr_agg_2012_2018$type <- "yr_agg"
SB_boot_ci_yr_agg_2001_2003$type <- "yr_agg"
TP_boot_ci_yr_agg_2012_2018$type <- "yr_agg"

hl_boot_ci_yr_agg_2012_2018$study_area <- "Highlands"
SB_boot_ci_yr_agg_2001_2003$study_area <- "Stoney_Brook"
TP_boot_ci_yr_agg_2012_2018$study_area <- "Trepassey"


temp1 <- rbind(hl_boot_ci_yr_agg_2012_2018,
              SB_boot_ci_yr_agg_2001_2003,
              TP_boot_ci_yr_agg_2012_2018)


library(kableExtra)
kbl(temp1[, c(9, 1:7)], 
    col.names = c('Study_area', 'Species', 
                  'mean', '2.5%', '97.5%',
                  'mean', '2.5%', '97.5%'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 2, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()


# WTF is this????
SJ__bootstrap_1995_1996


# END ----