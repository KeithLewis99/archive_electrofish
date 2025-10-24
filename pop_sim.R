km <- 100 # length of river (km)
L <- km*1000 # length of river (m)
W <- 10 # width of river (m)

per <- 1.2 # percent of river improved
per_mult <- 1.2*0.01
units <- L*(1-per_mult)*W/100 # unit in 100m2 for unimproved

D <- 30 # density per unit

units_hab <- L*(per_mult)*W/100 # unit in 100m2 for unimproved


d_pop <- -3 # percent change along river due to other factors
I <- 100 # percent increase in improved section

pop <- units*D # population along unimproved section of the river with units in 100m2; multiplier is for 1000 km in a meter and then divided by 100 m2 to get units

pop_new <- pop*(1 + (d_pop*0.01)) # change in population on unimproved section
pop_change <- pop_new - pop
pop_new/pop

hab_pop <- units_hab*D # population in the habitat improvement section
hab_pop_new <- hab_pop + (hab_pop*I*0.01) # change in the populatoin due to habitat improvement
hab_pop_new <- hab_pop*(1 + I*0.01)
pop_change_hab <- hab_pop_new - hab_pop

pop_change_hab - abs(pop_change) # net change in population
# negative value means overall population decline

pop_change/pop_change_hab


## would like to do the calculus so that we can create an isocline below which population is decreaseing and above which it is increasing.  
dr <- (1-p)/p
p <- seq(0.1, 10, 0.1)

plot(p, dr)
