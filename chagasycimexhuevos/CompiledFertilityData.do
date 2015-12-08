*Bring in data from R
import delimited C:\Users\student\Laboratory\chagasycimexhuevos\CompiledFertilityData.csv

*format the data frame for our analysis
*Destring

 *convert string variables to numeric
replace avtemphigh="." if avtemphigh=="NA"
destring(avtemphigh), gen(avtemphigh_n)

replace hummax="." if hummax=="NA"
destring(hummax), gen(hummax_n)

*create indicator of first day of each bug
sort idnum week
gen firstweekind=1 if week==1
replace firstweekind=0 if week!=1

*check to make sure number of indicators = number of bugs
tab firstweekind

*summary statistics for discrete variables
tab infected
tab infected if firstweekind==1

*how many bugs feed on each mouse
tab mouseidnum if firstweekind==1

*summarize continuous variables
summ avlowtemp_total, detail
summ lowhum_total, detail

*model building
*start with all the variables that might be of interest
zinb egg_total infected lifespan i.mouseidnum lowhum_total avlowtemp_total if firstweekind==1, inflate(lifespan) vuong 

*save the log likelihood as a variable
scalar m1 = e(ll)

*take out one variable (avtemphigh has lowest p-value from model w/o mouse)
*remove lifespan
zinb egg_total infected i.mouseidnum lowhum_total avlowtemp_total if firstweekind==1, inflate(lifespan) vuong 
scalar m2a = e(ll)
*remove temperature
zinb egg_total infected lifespan i.mouseidnum lowhum_total if firstweekind==1, inflate(lifespan) vuong 
scalar m2b = e(ll)
*remove humidity
zinb egg_total infected lifespan i.mouseidnum avlowtemp_total if firstweekind==1, inflate(lifespan) vuong 
scalar m2c = e(ll)
*remove mouse
zinb egg_total infected lifespan lowhum_total avlowtemp_total if firstweekind==1, inflate(lifespan) vuong 
scalar m2d = e(ll)

*do a chi square test to see if that variable was significant in the model
*di "chi2(2) = " 2*(m2-m1)
*di "Prob > chi2 = "chi2tail(2, 2*(m1-m2))
*save that p-value
*scalar p_mode2c = chi2tail(2, 2*(m1-m2))

*for lifespan
di "chi2(2) = " 2*(m2a-m1)
di "Prob > chi2 = "chi2tail(2, 2*(m1-m2a))
scalar p_mode2a = chi2tail(2, 2*(m1-m2a))
*for temperature
di "chi2(2) = " 2*(m2b-m1)
di "Prob > chi2 = "chi2tail(2, 2*(m1-m2b))
scalar p_mode2b = chi2tail(2, 2*(m1-m2b))
*for humidity
di "chi2(2) = " 2*(m2c-m1)
di "Prob > chi2 = "chi2tail(2, 2*(m1-m2c))
scalar p_mode2c = chi2tail(2, 2*(m1-m2c))
*for mouse
di "chi2(2) = " 2*(m2d-m1)
di "Prob > chi2 = "chi2tail(2, 2*(m1-m2d))
scalar p_mode2d = chi2tail(2, 2*(m1-m2d))

*from this humidity is the weekest link and should be removed
*thus model two 
zinb egg_total infected lifespan i.mouseidnum avlowtemp_total if firstweekind==1, inflate(lifespan) vuong 
scalar m2 = e(ll)

*Repeat the above process for humidity and mouse
*remove lifespan
zinb egg_total infected i.mouseidnum avlowtemp_total if firstweekind==1, inflate(lifespan) vuong 
scalar m3a = e(ll)
*remove temperature
zinb egg_total infected i.mouseidnum lifespan if firstweekind==1, inflate(lifespan) vuong 
scalar m3b = e(ll)
*remove mouse
zinb egg_total infected lifespan avlowtemp_total   if firstweekind==1, inflate(lifespan) vuong 
scalar m3c = e(ll)

*for longevity
di "chi2(2) = " 2*(m3a-m2)
di "Prob > chi2 = "chi2tail(2, 2*(m2-m3a))
scalar p_mode3a = chi2tail(2, 2*(m2-m3a))
*for temperature
di "chi2(2) = " 2*(m3b-m2)
di "Prob > chi2 = "chi2tail(2, 2*(m2-m3b))
scalar p_mode3b = chi2tail(2, 2*(m2-m3b))
*for mouse
di "chi2(2) = " 2*(m3c-m2)
di "Prob > chi2 = "chi2tail(2, 2*(m2-m3c))
scalar p_mode3c = chi2tail(2, 2*(m2-m3c))

*we should remove temperature
*the following should be the model
zinb egg_total infected i.mouseidnum lifespan if firstweekind==1, inflate(lifespan) vuong 
scalar m3 = e(ll)

*remove mouse
zinb egg_total infected lifespan if firstweekind==1, inflate(lifespan) vuong 
scalar m4a = e(ll)
*remove lifespan

scalar m4b = e(ll)

*for longevity
di "chi2(2) = " 2*(m4a-m3)
di "Prob > chi2 = "chi2tail(2, 2*(m3-m4a))
scalar p_mode4a = chi2tail(2, 2*(m3-m4a))

*for mouse
di "chi2(2) = " 2*(m4b-m3)
di "Prob > chi2 = "chi2tail(2, 2*(m3-m4b))
scalar p_mode4b = chi2tail(2, 2*(m3-m4b))

*both variables survive

*do a chi square test to see if that variable was significant in the model
*di "chi2(2) = " 2*(m2-m1)
*di "Prob > chi2 = "chi2tail(2, 2*(m1-m2))
*save that p-value
*scalar p_mode2c = chi2tail(2, 2*(m1-m2))

*this will give you the difference between the two models
*if this p-value is >.05, the variable you removed is NOT significant in the model
*this means that you SHOULD remove it

*****create the models recommended by Mike and Ricardo
***Simple model without covariates***

zinb egg_total infected if firstweekind==1, inflate(week) vuong 

*save dataset as .dta
save "C:\Users\student\Laboratory\chagasycimexhuevos\CompiledFertilityData.dta"


nbreg egg_total infected if firstweekind==1, exposure(lifespan)


nbreg egg_total infected avlowtemp_total if firstweekind==1, exposure(lifespan)

*model with temperature and humidity in offset(exposure) model
nbreg egg_total infected avlowtemp_total lowhum_total  if firstweekind==1, exposure(lifespan) irr

*model with infection and lifespan interaction
zinb egg_total i.infected##c.lifespan if firstweekind==1, inflate(lifespan) vuong 
*model without inflation
zinb egg_total i.infected##c.lifespan if firstweekind==1
*model with other covarriates.
zinb egg_total i.infected##c.lifespan avlowtemp_total lowhum_total if firstweekind==1, inflate(lifespan) vuong 

