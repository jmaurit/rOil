#temp file for oil work

library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv)
library(lubridate) 
library(grid)
library(boot)



#run fresh data import and clean:
#source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_clean.r") 

#prep data for modeling
source("/Users/johannesmauritzen/Google Drive/github/rOil/oil_modeling_prep.r") 


#gam modeling*************************************************************************

#benchmark model - uses non-parametric function on both analysis time and year
prod_gam<-gam(year_prod~s(time_to_peak) + s(peak_to_end) + s(year) + s(recoverable_oil), 
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p)

fields_p$smooth_bench<-prod_gam$fitted.values

#prod_gam2<-gam(year_prod~s(time_to_peak) + s(peak_to_end) + (recoverable_oil) + year, 
#	family=Gamma(link=log), weights=recoverable_oil, data=fields_p)

#fields_p$smooth_bench_dummies<-prod_gam2$fitted.values

#benchmark model split into under and over a certain limit
split<-8

prod_gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +  s(year), 
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])
prod_gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) + s(year), 
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])


fields_p$smooth_bench_split[fields_p$max_prod<=split]<-prod_gam_under$fitted.values
fields_p$smooth_bench_split[fields_p$max_prod>split]<-prod_gam_over$fitted.values


#residuals appear to be q
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

#gam_price_under2<-gam(year_prod~s(time_to_peak, oil_price_real_l5)+ s(peak_to_end, oil_price_real_l5),
#	family=Gamma(link=log), weights=recoverable_oil,data=fields_p[fields_p$max_prod<=split,])

#gam_price_over2<-gam(year_prod~s(time_to_peak, oil_price_real_l5) + s(peak_to_end, oil_price_real_l5),
#	family=Gamma(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

fields_p$smooth_price[fields_p$max_prod<=split]<-gam_price_under$fitted.values
fields_p$smooth_price[fields_p$max_prod>split]<-gam_price_over$fitted.values

#fields_p$smooth_price_2[fields_p$max_prod<=split]<-gam_price_under2$fitted.values
#fields_p$smooth_price_2[fields_p$max_prod>split]<-gam_price_over2$fitted.values



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

fields_lim<-fields_lim[,c("name", "year", "year_prod", "smooth_bench", 
	"smooth_bench_split", "smooth_price", "smooth_split")]

fields_long<-melt(fields_lim, id.vars=c("name", "year", "year_prod"))
names(fields_long)[4]<-"smoothing_type"
names(fields_long)[5]<-"smoothed"
fields_long$smoothing_type<-factor(fields_long$smoothing_type, 
	labels=c("Benchmark", "Split Benchmark", "With Price", "Non-Price"))

#compare split benchmark with non-split
bench_vs_split <- ggplot(fields_long[fields_long$smoothing_type %in% c("Benchmark", "Split Benchmark"),]) +
geom_point(aes(x=year, y=year_prod)) +
geom_line(aes(x=year, y=smoothed, color=smoothing_type)) +
facet_wrap(~name, scales="free")

png("/Users/johannesmauritzen/Google Drive/oil/figures/bench_vs_split.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(bench_vs_split)
dev.off()

#compare split benchmark with split price
bench_vs_price<-bench_vs_split %+%
fields_long[fields_long$smoothing_type %in% c("Split Benchmark", "With Price"),] +
scale_color_manual(values=c("red", "orange"))

png("/Users/johannesmauritzen/Google Drive/oil/figures/bench_vs_price.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(bench_vs_price)
dev.off()

#compare split with split price
price_vs_non_price<-bench_vs_split %+%  
fields_long[fields_long$smoothing_type %in% c("With Price", "Non-Price"),] +
geom_line(aes(x=year, y=smoothed, color=smoothing_type),position="jitter") +
scale_color_manual(values=c("orange", "green"))


png("/Users/johannesmauritzen/Google Drive/oil/figures/price_vs_non_price.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(price_vs_non_price)
dev.off()
#compare split eko with and without
#multi_gam_plot %+% fields_lim + 
#geom_line(aes(x=year, y=smooth_prod4, sep=""), color="blue") +
#geom_line(aes(x=year, y=smooth_eko), color="orange")










