#temp file for oil work

library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv)
library(lubridate) 
library(grid)
library(boot)



#run fresh data import and clean:
source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_clean.r") 

#prep data for modeling
source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_modeling_prep.r") 


#gam modeling*************************************************************************

#benchmark model - uses non-parametric function on both analysis time and year
prod_gam<-gam(year_prod~s(time_to_peak) + s(peak_to_end) + year + recoverable_oil, 
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p)

fields_p$smooth_bench<-prod_gam$fitted.values

prod_gam2<-gam(year_prod~s(time_to_peak) + s(peak_to_end) + (recoverable_oil) + year, 
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p)

fields_p$smooth_bench_dummies<-prod_gam2$fitted.values

#benchmark model split into under and over a certain limit
split<-8

prod_gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +  s(year), 
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])
prod_gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) + s(year), 
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])


fields_p$smooth_bench_split[fields_p$max_prod<=split]<-prod_gam_under$fitted.values
fields_p$smooth_bench_split[fields_p$max_prod>split]<-prod_gam_over$fitted.values


#residuals appear to be 
#plot(prod_gam_under$residuals)
#plot(prod_gam_over$residuals)

#can we get standard errors for the prediction
#use bootstram



#benchmark with ekofisk in two
#prod_eko_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(year), data=fields_eko[fields_eko$max_prod<=split,])
#prod_eko_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(year), weights=recoverable_oil, data=fields_eko[fields_eko$max_prod>split,])

#fields_p$smooth_eko[fields_p$max_prod<=split]<-prod_eko_under$fitted.values
#fields_p$smooth_eko[fields_p$max_prod>split]<-prod_eko_over$fitted.values


#Now see if oil prices have an effect


#Have to be careful about price and effect of year dummies - remove affect of year
gam_price_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

gam_price_under2<-gam(year_prod~s(time_to_peak, oil_price_real_l5)+ s(peak_to_end, oil_price_real_l5),
	family=Gamma(link=log), weights=recoverable_oil,data=fields_p[fields_p$max_prod<=split,])

gam_price_over2<-gam(year_prod~s(time_to_peak, oil_price_real_l5) + s(peak_to_end, oil_price_real_l5),
	family=Gamma(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

fields_p$smooth_price[fields_p$max_prod<=split]<-gam_price_under$fitted.values
fields_p$smooth_price[fields_p$max_prod>split]<-gam_price_over$fitted.values

fields_p$smooth_price_2[fields_p$max_prod<=split]<-gam_price_under2$fitted.values
fields_p$smooth_price_2[fields_p$max_prod>split]<-gam_price_over2$fitted.values



#model as a logit with 1 being the peak?  0 being min.  


#compare direct price/non price comparision
gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil),
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil),
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

fields_p$smooth_split[fields_p$max_prod<=split]<-gam_price_under$fitted.values
fields_p$smooth_split[fields_p$max_prod>split]<-gam_price_over$fitted.values


#test of coefficients on price
coef_under<-gam_price_under$coefficients
coef_over<-gam_price_over$coefficients

sd_under<-sqrt(diag(gam_price_under$Ve))
sd_over<-sqrt(diag(gam_price_over$Ve)) #variance from frequentist point of view, Vp for bayesian - better for CI

coef_under[2:7]
sd_under[2:7]

coef_over[2:7]
sd_over[2:7]

#now use ridge regression option
gam_under<-gam(year_prod~s(time_to_peak),
	data=fields_p[fields_p$max_prod<=split,], select=TRUE)

gam_over<-gam(year_prod~s(time_to_peak),
	weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,], select=TRUE)

#test on statfjord
gam_test<-ggplot(fields_p[fields_p$name=="STATFJORD",]) +
geom_point(aes(x=year, y=year_prod)) +
geom_line(aes(x=year, y=smooth_bench_split), color="blue") +
geom_line(aes(x=year, y=smooth_price), color="red") +
labs(title="Statfjord")
gam_test 

gam_test<-ggplot(fields_p[fields_p$name=="EKOFISK",]) +
geom_point(aes(x=year, y=year_prod)) +
geom_line(aes(x=year, y=smooth_bench_split), color="blue") +
geom_line(aes(x=year, y=smooth_price), color="red") +
labs(title="Ekofisk")
gam_test 



#Show results Tom - Down



#better way to show
include_fields<-c("ALBUSKJELL","ALVE", "COD", "EKOFISK", "TROLL","GULLFAKS", "KRISTIN", "STATFJORD", "ENOCH")

#function to to show field-level of data

fields_lim<-subset(fields_p, name %in% include_fields)

multi_gam_plot <- ggplot(fields_lim) +
geom_point(aes(x=year, y=year_prod)) +
facet_wrap(~name, scales="free")

#compare whole benchmark with non-param year and non-year
#multi_gam_plot +
#geom_line(aes(x=year, y=smooth_bench,sep=""), color="blue") +
#geom_line(aes(x=year, y=smooth_bench_dummies), color="purple")

#compare split benchmark with non-split
multi_gam_plot +
geom_line(aes(x=year, y=smooth_bench,sep=""), color="blue") +
geom_line(aes(x=year, y=smooth_bench_split), color="red")

#compare split benchmark with split price
multi_gam_plot %+% fields_lim + 
geom_line(aes(x=year, y=smooth_bench_split,sep=""), color="red") +
geom_line(aes(x=year, y=smooth_price), color="orange")

#compare split with split price
multi_gam_plot %+% fields_lim + 
geom_point(aes(x=year, y=smooth_split,sep=""), color="green") +
geom_line(aes(x=year, y=smooth_price), color="orange", alpha=.5)


#compare split eko with and without
#multi_gam_plot %+% fields_lim + 
#geom_line(aes(x=year, y=smooth_prod4, sep=""), color="blue") +
#geom_line(aes(x=year, y=smooth_eko), color="orange")










