# age first, then site
# filter data type and year YOY with test
test <- df_med2 |>
   filter(data_den == "2b" & age > 0)
test |> select(year, species, age, density, density_new, area)

str(test)
# this sums the mean and variance by year for each age
## this matches EXCEL
test1 <- test |>
   select(year, species, age, trt, density_new, d_se_new)|>
   group_by(year, species, trt) |>
   summarise(dsum = sum(density_new),
          dsum_var = sum(d_se_new)^2) 

test1 |> 
   filter(species == "AS" & trt == "riffle") |>
   #select(year, species, density_new, d_se_new, dsum, dsum_var)|> 
   print(n = Inf)

# SITE
# this converts se to var and does partial derivative
## this matches EXCEL
test2 <- test1 |>
   group_by(species, trt) |>
   mutate(dsite_var = dsum_var*1/n())

# add them up
test3 <- test2 |> 
   group_by(species, trt) |>
   summarize(
      mean_site = mean(dsum, na.rm = T),
      var_site = sqrt(mean(dsite_var, na.rm = T)),
      ll_site = mean_site - var_site*1.96,
      ul_site = mean_site + var_site*1.96
   )

filter(species == "AS" & trt == "riffle") |>
   select(year, species, age, density_new, dsum_var, dsite_var)

