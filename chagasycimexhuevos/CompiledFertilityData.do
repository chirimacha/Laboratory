**install fitstat package
*findit fitstat
**do "C:\Users\student\AppData\Local\Temp\STD02000000.tmp"

**install lars package
*findit lasso

*Bring in data from R
import delimited C:\Users\student\Laboratory\chagasycimexhuevos\singleweekdata.csv, clear

***format the data frame for our analysis
*convert string variables to numeric
*replace avtemphigh="." if avtemphigh=="NA"
*destring(avtemphigh), gen(avtemphigh_n)


*create indicator of first day of each bug (to get one observation per insect)
*sort idnum week
*gen firstweekind=1 if week==1
*replace firstweekind=0 if week!=1

*check to make sure number of indicators = number of bugs (should be 177)
*tab firstweekind

*summary statistics for discrete variables
*tab infected
*tab infected if firstweekind==1

*how many bugs feed on each mouse
*tab mouseidnum if firstweekind==1

*summarize continuous variables
*summ avlowtemp_total, detail
*summ lowhum_total, detail

*==============================================================================
*Skit to line 139.  This section uses a formal reduction method to choose model.
*******************************************************************************
*model building
*start with all the variables that might be of interest
zinb egg_total infected lifespan i.mouseidnum lowhum_total avlowtemp_total, inflate(lifespan) vuong 

*save the log likelihood as a variable
scalar m1 = e(ll)

*take out one variable (avtemphigh has lowest p-value from model w/o mouse)
*remove lifespan
zinb egg_total infected i.mouseidnum lowhum_total avlowtemp_total, inflate(lifespan) vuong 
scalar m2a = e(ll)
*remove temperature
zinb egg_total infected lifespan i.mouseidnum lowhum_total, inflate(lifespan) vuong 
scalar m2b = e(ll)
*remove humidity
zinb egg_total infected lifespan i.mouseidnum avlowtemp_total, inflate(lifespan) vuong 
scalar m2c = e(ll)
*remove mouse
zinb egg_total infected lifespan lowhum_total avlowtemp_total, inflate(lifespan) vuong 
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
zinb egg_total infected lifespan i.mouseidnum avlowtemp_total, inflate(lifespan) vuong 
scalar m2 = e(ll)

*Repeat the above process for humidity and mouse
*remove lifespan
zinb egg_total infected i.mouseidnum avlowtemp_total, inflate(lifespan) vuong 
scalar m3a = e(ll)
*remove temperature
zinb egg_total infected i.mouseidnum lifespan, inflate(lifespan) vuong 
scalar m3b = e(ll)
*remove mouse
zinb egg_total infected lifespan avlowtemp_total, inflate(lifespan) vuong 
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
zinb egg_total infected i.mouseidnum lifespan, inflate(lifespan) vuong 
scalar m3 = e(ll)

*remove mouse
zinb egg_total infected lifespan, inflate(lifespan) vuong 
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
*zero-inflated negative binomial (no interaction)
zinb egg_total infected, inflate(week) vuong 
*negative binomial offset/exposure model
nbreg egg_total infected, exposure(lifespan)
*same model with temperature
nbreg egg_total infected avlowtemp_total, exposure(lifespan)
*model with temperature and humidity in offset(exposure) model

*m1: Offset/exposure model with temperature and humidity covariates
nbreg egg_total infected avlowtemp_total lowhum_total, exposure(lifespan) irr
fitstat 
*AIC = 9.114 
predict p_egg_m1
twoway scatter egg_total p_egg_m1

*m2: negative binomial model with hum, temp, and lifespan
nbreg egg_total infected avlowtemp_total lowhum_total lifespan, irr
fitstat
*AIC =  9.245
predict p_egg_m2
twoway scatter egg_total p_egg_m2

*m3: same as m2, but with zero-inflation on lifespan
zinb egg_total infected avlowtemp_total lowhum_total lifespan, inflate(lifespan) vuong irr
fitstat
*AIC =  9.490
predict p_egg_m3
twoway scatter egg_total p_egg_m3

*m4a: same as m3 but removing lifespan as a parameter.
*zinb egg_total infected avlowtemp_total lowhum_total, inflate(lifespan) vuong irr
*predict p_egg_m4a
*twoway scatter egg_total p_egg_m4a

*m4: same as m1, but with average low humidity instead of absolute low humidity
nbreg egg_total infected avlowtemp_total avlowhum_total, exposure(lifespan) irr
predict p_egg_m4
twoway scatter egg_total p_egg_m4
fitstat
*AIC=
*******************************************************************************
***Let's run the same models but use the priniple components
******************************************************************************* 
*m1p9a: Offset/exposure model with all principle components
nbreg egg_total infected comp1 comp2 comp3 comp4 comp5 comp6 comp7 comp8 comp9,exposure(lifespan) irr
*estimates store fuller 
scalar m1p9= e(ll)

fitstat
*AIC =  9.142
predict p_egg_m1p5
twoway scatter egg_total p_egg_m1p5

nbreg egg_total infected comp1 comp2 comp3 comp4, exposure(lifespan) irr
estimates store full 
fitstat
*AIC =  9.140
predict p_egg_m1p4
twoway scatter egg_total p_egg_m1p4

nbreg egg_total infected comp1 comp2 comp3, exposure(lifespan) irr
estimates store full 
fitstat
*AIC =  9.133
predict p_egg_m1p3
twoway scatter egg_total p_egg_m1p3


*m1p2: Offset/exposure model with 1 principle component
nbreg egg_total infected comp1 comp2, exposure(lifespan) irr
scalar m1p2a = e(ll)
*estimates store full 
fitstat
*AIC =  9.122
predict p_egg_m1p2a
twoway scatter egg_total p_egg_m1p2a

*m1p1: Offset/exposure model with 1 principle component
nbreg egg_total infected comp1, exposure(lifespan) irr
scalar m1p1 = e(ll)
fitstat 
*AIC = 9.112
predict p_egg_m1p1
twoway scatter egg_total p_egg_m1p1

*m1p0
nbreg egg_total infected, exposure(lifespan) irr
scalar m1p0a = e(ll)
predict p_egg_m1p0a
twoway scatter egg_total p_egg_m1p0a

*use chi square to test if Comp1 is beneficial compared to null
di "chi2(2) = " 2*(m1p1-m1p0a)
di "Prob > chi2 = "chi2tail(2, 2*(m1p1-m1p0a))
scalar p_modelm1p1a = chi2tail(2, 2*(m1p1-m1p0a))

*perform chi squared to test if Comp2 is benefician compared to Comp1
di "chi2(2) = " 2*(m1p2a-m1p1)
di "Prob > chi2 = "chi2tail(2, 2*(m1p2a-m1p1))
scalar p_modelm1p2aa = chi2tail(2, 2*(m1p2a-m1p1))

**perform chi squared to test if Comp2 is benefician compared to Null
di "chi2(2) = " 2*(m1p2a-m1p0a)
di "Prob > chi2 = "chi2tail(2, 2*(m1p2a-m1p0a))
scalar p_modelm1p2ba = chi2tail(2, 2*(m1p2a-m1p0a))

*lets compare comp 9 with the naive model
di "chi2(2) = " 2*(m1p9-m1p0a)
di "Prob > chi2 = "chi2tail(2, 2*(m1p9-m1p0a))
scalar p_modelm1p9 = chi2tail(2, 2*(m1p9-m1p0a))

*We get high p-values, thus we shouldn't add the principle componenets.



*******************************************************************************
***Lasso Analysis***
*******************************************************************************
*model 1
lars egg_total lifespan infected avtemp_total avlowtemp_total avhightemp_total hightemp_total lowtemp_total avhum_total avlowhum_total avhighhum_total highhum_total lowhum_total, a(lasso) g

*model 2
lars egg_total lifespan avtemp_total avlowtemp_total avhightemp_total hightemp_total lowtemp_total avhum_total avlowhum_total avhighhum_total highhum_total lowhum_total infected, a(lasso) g

*switching order has no effect
*******************************************************************************
*model with infection and lifespan interaction with zero inflation
zinb egg_total i.infected##c.lifespan, inflate(lifespan) vuong 
*model without inflation without zero inflation
nbreg egg_total i.infected##c.lifespan
*same models with other covarriates temperature and humidity
zinb egg_total i.infected##c.lifespan avlowtemp_total lowhum_total, inflate(lifespan) vuong 
nbreg egg_total i.infected##c.lifespan avlowtemp_total lowhum_total

*save dataset as .dta
*save "C:\Users\student\Laboratory\chagasycimexhuevos\singleweekdata.dta"
