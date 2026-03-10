# this is a follow up to tables but will be only for tables aggregated over sites, i.e., they years will be displayed

# library ----
## delta method only
library(dplyr)
library(kableExtra)


# site ----
## 2b load files ----
# create pattern
temp1 = list.files(path = "data_derived", pattern=".+site.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_delta_siteagg = (lapply(temp1, read.csv))
names(ls_delta_siteagg) <- name_files(temp1)
list2env(ls_delta_siteagg, envir = .GlobalEnv)
str(ls_delta_siteagg, 1)

### TP ----
# no trepassey because no site level info

## 2a load files ----
# create pattern
str(ls_delta_siteagg, 1)

### CL ----
cl_den_delta_site_1993_1995 <- cl_den_delta_site_1993_1995 |>
   rename(density = mean_site,
          den_se = se_site,
          den_ll = ll_site,
          den_ul = ul_site)


cl_bio_site_1993_1995 <- cl_bio_site_1993_1995 |>
   rename(biomass = mean_site,
          bio_se = se_site,
          bio_ll = ll_site,
          bio_ul = ul_site)

common <- intersect(names(cl_den_delta_site_1993_1995), 
                    names(cl_bio_site_1993_1995)
)
out <- bind_cols(cl_den_delta_site_1993_1995,
                 cl_bio_site_1993_1995 %>% 
                    select(-all_of(common)))

tab2a_siteagg <- kbl(out[, c(1:3, 8, 4, 6:7, 9, 11:12)], 
                    col.names = c('Study_area', 'Year', 'Species', 'age',
                                  'mean', '2.5%', '97.5%',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "Copper Lake (1993-1995): Density and Biomass CIs aggregated over sites", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_siteagg, file = "output/tab_2aCL_site_agg.html")


### WS 1992 ----
ws_den_delta_site_1992 <- ws_den_delta_site_1992 |>
   rename(density = mean_site,
          den_ll = ll_site,
          den_ul = ul_site
   )

ws_bio_delta_site_1992 <- ws_bio_delta_site_1992 |>
   mutate(den_ll = mean_site - 1.96*se_site,
          den_ul = mean_site + 1.96*se_site) |>
   rename(biomass = mean_site)

common <- intersect(names(ws_den_delta_site_1992), 
                    names(ws_bio_delta_site_1992)
)
out <- bind_cols(ws_den_delta_site_1992,
                 ws_bio_delta_site_1992 %>% 
                    select(-all_of(common)))

tab2a_WSsite <- kbl(out[, c(1:3, 8, 4, 6:7, 10:12)], 
                   col.names = c('Study_area', 'Year', 'Species', 'age',
                                 'mean', '2.5%', '97.5%',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "West Salmon (1992): Density and Biomass CIs", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_WSsite, file = "output/tab_2aWS_site.html")

### HL & JF ----
jf_den_delta_site_1993_1995 <- jf_den_delta_site_1993_1995 |>
   rename(density = dsum,
          den_var = dsum_var,
          den_ll = ll_site,
          den_ul = ul_site)

HL_den_delta_site_2002 <- HL_den_delta_site_2002 |>
   mutate(den_ll = density - 1.96*density_se,
          den_ul = density + 1.96*density_se)

keys <- c("study_area", "year", "species", "trt", "age")

res <- jf_den_delta_site_1993_1995 %>%
   # out already has density/biomass + CI/var; keep it as the primary table
   full_join(
      HL_den_delta_site_2002 %>%
         select(all_of(keys)), 
      by = keys
   )

tab2b_HL_JF <- kbl(res[, c(1:5, 10, 6, 8:9)], 
                   col.names = c('Study_area', 'Year', 'Species', 'site' , 'trt', 'age',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "Joe Farrells (1993-95) and Highlands (2002): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 6, "Density" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_HL_JF, file = "tab_HL_JF_site.html")


# year ----
## load files ----
# create pattern
temp2 = list.files(path = "data_derived", pattern=".+_yr_.+.csv$", full.names = T)

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_delta_yearagg = (lapply(temp2, read.csv))
names(ls_delta_yearagg) <- name_files(temp2)
list2env(ls_delta_yearagg, envir = .GlobalEnv)
str(ls_delta_yearagg, 1)


### TP ----
tp_bio_delta_yr_1984_1996 <- tp_bio_delta_yr_1984_1996 |>
   rename(biomass = mean_year,
          bio_var = var_year,
          bio_ll = ll_site,
          bio_ul = ul_site)


tp_den_delta_yr_1984_1996 <- tp_den_delta_yr_1984_1996 |>
   rename(density = mean_year,
          den_var = var_year,
          den_ll = ll_site,
          den_ul = ul_site)



common <- intersect(names(tp_bio_delta_yr_1984_1996), 
                    names(tp_den_delta_yr_1984_1996)
)
out <- bind_cols(tp_den_delta_yr_1984_1996,
                 tp_bio_delta_yr_1984_1996 %>% 
                    select(-all_of(common)))

# add pool
tp_pool_den_delta_yr_1984_1996$trt <- "pool"
tp_pool_den_delta_yr_1984_1996 <- tp_pool_den_delta_yr_1984_1996 |>
   rename(density = mean_year,
          den_var = var_year,
          den_ll = ll_site,
          den_ul = ul_site)

tp_pool_bio_delta_yr_1984_1996$trt <- "pool"
tp_pool_bio_delta_yr_1984_1996 <- tp_pool_bio_delta_yr_1984_1996 |>
   rename(biomass = mean_year,
          bio_var = var_year,
          bio_ll = ll_site,
          bio_ul = ul_site)

common <- intersect(names(tp_pool_den_delta_yr_1984_1996), 
                    names(tp_pool_bio_delta_yr_1984_1996)
)
out_pool <- bind_cols(tp_pool_den_delta_yr_1984_1996,
                 tp_pool_bio_delta_yr_1984_1996 %>% 
                    select(-all_of(common)))

keys <- c("study_area", "species", "trt", "age")

res_all <- out %>%
   # out already has density/biomass + CI/var; keep it as the primary table
   full_join(
      out_pool %>%
         select(all_of(keys)), 
      by = keys
   )

tab2b_yragg <- kbl(res[, c(1:3, 5, 8, 3, 6:7, 9, 11:12)], 
                    col.names = c('Study_area', 'Species', 'trt', 'age',
                                  'mean', '2.5%', '97.5%',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "Trepassey (1984-1996): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_yragg, file = "output/tab_tp_yr.html")




### CL ----
str(ls_delta_yearagg, 1)
cl_den_delta_yr_1993_1995 <- cl_den_delta_yr_1993_1995 |>
   rename(density = mean_year,
          den_se = var_year,
          den_ll = ll_site,
          den_ul = ul_site)


cl_bio_delta_yr_1993_1995 <- cl_bio_delta_yr_1993_1995 |>
   rename(biomass = mean_year,
          bio_se = var_year,
          bio_ll = ll_site,
          bio_ul = ul_site)

common <- intersect(names(cl_den_delta_yr_1993_1995), 
                    names(cl_bio_delta_yr_1993_1995)
)
out <- bind_cols(cl_den_delta_yr_1993_1995,
                 cl_bio_delta_yr_1993_1995 %>% 
                    select(-all_of(common)))

tab2a_yragg <- kbl(out[, c(1:2, 7, 3, 5:6, 8, 10:11)], 
                     col.names = c('Study_area', 'Species', 'age',
                                   'mean', '2.5%', '97.5%',
                                   'mean', '2.5%', '97.5%'),
                     align = 'c', caption = "Copper Lake (1993-1995): Density and Biomass CIs aggregated over years", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 3, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_yragg, file = "output/tab_2aCL_yr_agg.html")


### WS 1992 ----
ws_den_delta_site_1992 <- ws_den_delta_site_1992 |>
   rename(density = mean_site,
          den_ll = ll_site,
          den_ul = ul_site
   )

ws_bio_delta_site_1992 <- ws_bio_delta_site_1992 |>
   mutate(den_ll = mean_site - 1.96*se_site,
          den_ul = mean_site + 1.96*se_site) |>
   rename(biomass = mean_site)

common <- intersect(names(ws_den_delta_site_1992), 
                    names(ws_bio_delta_site_1992)
)
out <- bind_cols(ws_den_delta_site_1992,
                 ws_bio_delta_site_1992 %>% 
                    select(-all_of(common)))

tab2a_WSsite <- kbl(out[, c(1:3, 8, 4, 6:7, 10:12)], 
                    col.names = c('Study_area', 'Year', 'Species', 'age',
                                  'mean', '2.5%', '97.5%',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "West Salmon (1992): Density and Biomass CIs", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_WSsite, file = "output/tab_2aWS_site.html")

### HL & JF ----
jf_den_delta_site_1993_1995 <- jf_den_delta_site_1993_1995 |>
   rename(density = dsum,
          den_var = dsum_var,
          den_ll = ll_site,
          den_ul = ul_site)

HL_den_delta_site_2002 <- HL_den_delta_site_2002 |>
   mutate(den_ll = density - 1.96*density_se,
          den_ul = density + 1.96*density_se)

keys <- c("study_area", "year", "species", "trt", "age")

res <- jf_den_delta_site_1993_1995 %>%
   # out already has density/biomass + CI/var; keep it as the primary table
   full_join(
      HL_den_delta_site_2002 %>%
         select(all_of(keys)), 
      by = keys
   )

tab2b_HL_JF <- kbl(res[, c(1:5, 10, 6, 8:9)], 
                   col.names = c('Study_area', 'Year', 'Species', 'site' , 'trt', 'age',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "Joe Farrells (1993-95) and Highlands (2002): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 6, "Density" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_HL_JF, file = "tab_HL_JF_site.html")

### GG ----

# END ----