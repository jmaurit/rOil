library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv) #for generalized additive models
library(lubridate) 
library(grid)
library(boot)
library(arm)
library(gamm4) # for mixed models
library(texreg)
#library(lmer) not available for 3.02

#clear workspace
rm(list = ls())

#Price part of modeling
#price_modeling.R
# under oil_modeling.R
source("/Users/johannesmauritzen/Google Drive/github/rOil/oil_modeling_prep.r") 

split<-8
fields_p$after_peak<-ifelse(fields_p$time_to_peak==0,1,0)
fields_p$large_field<-as.factor(ifelse(fields_p$max_prod>split, "large","small"))

#Two - stage model look at residuals.

gam_1stage_under<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_1stage_over<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

resid_under<-as.numeric(residuals(gam_1stage_under))
resid_over<-as.numeric(residuals(gam_1stage_over))

fields_p$residuals<-ifelse(fields_p$max_prod<=split,resid_under,resid_over)

fields_p$

plot(resid_under)
plot(resid_over)
qqplot(resid_under)

hist(resid_under)
hist(resid_over)

ggplot(fields_p[c("year", "residuals")]) +
geom_jitter(aes(x=year, y=residuals))

ggplot(fields_p[c("peak_to_end", "residuals")]) +
geom_jitter(aes(x=peak_to_end, y=residuals))

ggplot(fields_p[c("time_to_peak", "residuals")]) +
geom_jitter(aes(x=time_to_peak, y=residuals))




#preferred models for effect on price
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

summary_under_2d<-summary(gam_price_under_2d)
summary_over_2d<-summary(gam_price_over_2d)
summary_under_2d
summary_over_2d

plot(gam_price_under_2d, select=1)
plot(gam_price_under_2d, select=2)

plot(gam_price_over_2d, select=1)
plot(gam_price_over_2d, select=2)

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_under_2d_plot_1.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
plot(gam_price_under_2d, select=1)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_under_2d_plot_2.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
plot(gam_price_under_2d, select=2)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_over_2d_plot_1.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
plot(gam_price_over_2d, select=1)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_over_2d_plot_2.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
plot(gam_price_over_2d, select=2)
dev.off()

#with fixed effect for after 1987 - introduction of gas injection.  

fields_p$pre_87<-as.factor(ifelse(fields_p$year<1988, "pre-1987","post-1987"))


gam_price_under_fe<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8 + pre_87,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_fe<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8 + pre_87,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

summary(gam_price_under_fe)
summary(gam_price_over_fe)


#qq-plot
png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/qq_gam_price_under_2d.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
qq.gam(gam_price_under_2d) 
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/qq_gam_price_over_2d.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
qq.gam(gam_price_over_2d) 
dev.off()

#fitted vs. predicted
png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/over_fitted.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
plot(fitted(gam_price_over_2d), napredict(gam_price_over_2d$na.action, gam_price_over_2d$y), xlab = "Fitted Values", 
        ylab = "Response", main = "Response vs. Fitted Values")  #  fitted vs. reside
dev.off()


png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/under_fitted.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
plot(fitted(gam_price_under_2d), napredict(gam_price_under_2d$na.action, gam_price_under_2d$y), xlab = "Fitted Values", 
        ylab = "Response", main = "Response vs. Fitted Values") 
dev.off()



#use 8 lags
gam_price_under_2d_8<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_2d_8<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

summary(gam_price_under_2d_8)
summary(gam_price_over_2d_8)

# Short: get rid of concurrent price and first 3 lags

gam_price_under_2d_short<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_2d_short<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

summary(gam_price_under_2d_short)
summary(gam_price_over_2d_short)


#Table of results
texreg(list(gam_price_under_2d, gam_price_over_2d, gam_price_under_2d_8, gam_price_over_2d_8,
	gam_price_under_2d_short, gam_price_under_2d_short))

#Charts of resutls with simulated uncertainty

## simulate replicate beta vectors from posterior...
coef(gam_price_under_2d_8)
#with 8 lags
sim_gam<-function(model, start=1, stop=length(coef(model))){
	#test
	#model<-gam_price_under_2d_8
	#start<-2
	#stop<-10
	#

	beta<-coef(model)
	Vb<-vcov(model)
	## simulate replicate beta vectors from posterior...
	Cv <- chol(Vb)  #cholesky decomposition the equivalent of taking square root in a single variance to get SD?
	n.rep=1000
	nb <- length(beta)
	br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta  

	#chart replicate beta vectors
	out_data<-t(br[c(start:stop),])
	out_data_long<-data.frame(melt(out_data))
	names(out_data_long)<-c("id", "Variable", "Coef_Estimate")
	return(out_data_long)
	}


#combined over and under on one plot


#chart 6 lags
chart_under_6<-sim_gam(model=gam_price_under_2d,start=2,stop=8)
chart_over_6<-sim_gam(gam_price_over_2d,2,8)


chart_under_6$type<-"under"
chart_over_6$type<-"over"
chart_6_long<-rbind(chart_under_6, chart_over_6)

gam_price_6<-ggplot(chart_6_long, aes(x=Variable, y=Coef_Estimate, color=factor(type))) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variables", color="Field Size Threshhold") 

gam_price_6

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_6_pres.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_6)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_6_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_6)
dev.off()


#chart 8 lags
chart_under_8<-sim_gam(model=gam_price_under_2d_8,start=2,stop=10)
chart_over_8<-sim_gam(gam_price_over_2d_8,2,10)


chart_under_8$type<-"under"
chart_over_8$type<-"over"
chart_8_long<-rbind(chart_under_8, chart_over_8)

gam_price_8<-ggplot(chart_8_long, aes(x=Variable, y=Coef_Estimate, color=factor(type))) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variables", color="Field Size Threshhold") 

gam_price_8

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_8_pres.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_8)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_8_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_8)
dev.off()

#chart short lags
#coef(gam_price_under_2d_short)

chart_under_short<-sim_gam(model=gam_price_under_2d_short,start=2,stop=6)
chart_over_short<-sim_gam(gam_price_over_2d_short,2,6)


chart_under_short$type<-"under"
chart_over_short$type<-"over"
chart_short_long<-rbind(chart_under_short, chart_over_short)

gam_price_short<-ggplot(chart_short_long, aes(x=Variable, y=Coef_Estimate, color=factor(type))) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variables", color="Field Size Threshhold") 

gam_price_short

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_short_pres.png", 
	width = 27.8, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_short)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_short_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_short)
dev.off()

#***********



#Pooled data: interaction with small/large fields
#only interaction on the 6th lag was significant
gam_price_2d<-gam(year_prod~ + s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real*large_field +
	oil_price_real_l1*large_field + 
	oil_price_real_l2*large_field + 
	oil_price_real_l3*large_field + 
	oil_price_real_l4*large_field + 
	oil_price_real_l5*large_field + 
	oil_price_real_l6*large_field +
	oil_price_real_l7*large_field +
	oil_price_real_l8*large_field,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p)

summary(gam_price_2d)
coef(gam_price_2d)



#only interaction on the 6th lag was significant, remove first lags
#when we remove interactions, then significance of l1 disappears

gam_price_2d_short<-gam(year_prod~ + s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	large_field + 
	oil_price_real_l4+ 
	oil_price_real_l5+ 
	oil_price_real_l6+
	oil_price_real_l7+
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p)

summary(gam_price_2d_short)




#create table
texreg(list(gam_price_2d, gam_price_2d_short))

#create charts
chart_pooled<-sim_gam(model=gam_price_2d,start=2,stop=11)
chart_pooled$Variable<-as.character(chart_pooled$Variable)
chart_pooled<-chart_pooled[chart_pooled$Variable!="large_fieldsmall",]

gam_price_pooled<-ggplot(chart_pooled, aes(x=Variable, y=Coef_Estimate)) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variables", color="Field Size Threshhold") 

gam_price_pooled

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_pooled.png", 
	width = 27.8, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_pooled)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_pooled_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_pooled)
dev.off()


#Look at pre and post data

#Post Peak
#split with just after peak: excluded data where less than 5 years old
fields_old<-fields_p[fields_p$init_year<2008,]
#from 1131 to 1062 points
#small
gam_postpeak_small<-gam(year_prod~ s(peak_to_end, max_prod) +
	oil_price_real +
	oil_price_real_l1 +
	oil_price_real_l2 +
	oil_price_real_l3 +
	oil_price_real_l4 +
	oil_price_real_l5 +
	oil_price_real_l6 + 
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1 & fields_old$large_field=="small",])

#large
gam_postpeak_large<-gam(year_prod~ s(peak_to_end, max_prod) +
	oil_price_real +
	oil_price_real_l1 +
	oil_price_real_l2 +
	oil_price_real_l3 +
	oil_price_real_l4 +
	oil_price_real_l5 +
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1 & fields_old$large_field=="large",])

#pooled

gam_postpeak_pooled<-gam(year_prod~ s(peak_to_end, max_prod) +
	large_field +
	oil_price_real  +
	oil_price_real_l1 +
	oil_price_real_l2 + 
	oil_price_real_l3 + 
	oil_price_real_l4+ 
	oil_price_real_l5+ 
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1,])

summary(gam_postpeak_pooled)
summary(gam_postpeak_small)
summary(gam_postpeak_large)


#with recoverable_oil
gam_postpeak_small_tro<-gam(year_prod~ s(peak_to_end, recoverable_oil) +
	oil_price_real +
	oil_price_real_l1 +
	oil_price_real_l2 +
	oil_price_real_l3 +
	oil_price_real_l4 +
	oil_price_real_l5 +
	oil_price_real_l6 + 
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1 & fields_old$large_field=="small",])

#large
gam_postpeak_large_tro<-gam(year_prod~ s(peak_to_end, recoverable_oil) +
	oil_price_real +
	oil_price_real_l1 +
	oil_price_real_l2 +
	oil_price_real_l3 +
	oil_price_real_l4 +
	oil_price_real_l5 +
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1 & fields_old$large_field=="large",])

#pooled

gam_postpeak_pooled_tro<-gam(year_prod~ s(peak_to_end, recoverable_oil) +
	oil_price_real  + 
	oil_price_real_l1  +
	oil_price_real_l2  + 
	oil_price_real_l3 + 
	oil_price_real_l4 + 
	oil_price_real_l5 + 
	oil_price_real_l6  +
	oil_price_real_l7  +
	oil_price_real_l8 ,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1,])

summary(gam_postpeak_pooled_tro)
summary(gam_postpeak_small_tro)
summary(gam_postpeak_large_tro)

#Show fitted values
fields_old$smooth_max_prod[fields_old$after_peak==1 & fields_old$large_field=="large"]<-
	gam_postpeak_large$fitted.values
fields_old$smooth_max_prod[fields_old$after_peak==1 & fields_old$large_field=="small"]<-
	gam_postpeak_small$fitted.values

fields_old$smooth_tro_prod[fields_old$after_peak==1 & fields_old$large_field=="large"]<-
	gam_postpeak_large_tro$fitted.values
fields_old$smooth_tro_prod[fields_old$after_peak==1 & fields_old$large_field=="small"]<-
	gam_postpeak_small_tro$fitted.values


include_fields<-c("ALBUSKJELL","ALVHEIM", "COD", "EKOFISK", "TROLL","GULLFAKS", "KRISTIN", "STATFJORD", "TUNE")

#function to to show field-level of data
fields_lim<-subset(fields_old, name %in% include_fields)

fields_lim<-fields_lim[,c("name", "year", "year_prod", "smooth_max_prod", 
	"smooth_tro_prod")]

fields_long<-melt(fields_lim, id.vars=c("name", "year", "year_prod"))
names(fields_long)[4]<-"smoothing_type"
names(fields_long)[5]<-"smoothed"
fields_long$smoothing_type<-factor(fields_long$smoothing_type, 
	labels=c("Max Prod. GAM", "Tot. Recov. Oil GAM"))

Max_prod_vs_TRO <- ggplot(fields_long[fields_long$smoothing_type %in% c("Max Prod. GAM", "Tot. Recov. Oil GAM"),]) +
geom_point(aes(x=year, y=year_prod)) +
geom_line(aes(x=year, y=smoothed, color=smoothing_type)) +
facet_wrap(~name, scales="free")

Max_prod_vs_TRO

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/Max_prod_vs_TRO_pres.png", 
	width = 27.8, height = 21, units = "cm", res=300, pointsize=10)
print(Max_prod_vs_TRO)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/Max_prod_vs_TRO_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(Max_prod_vs_TRO)
dev.off()

#table
texreg(list(gam_postpeak_large_tro, gam_postpeak_small_tro, gam_postpeak_large, gam_postpeak_small))

#charts
coef(gam_postpeak_small_tro)
chart_postpeak_small<-sim_gam(model=gam_postpeak_small_tro,start=2,stop=10)
chart_postpeak_large<-sim_gam(gam_postpeak_large_tro, 2, 10)


chart_postpeak_small$type<-"under"
chart_postpeak_large$type<-"over"
chart_postpeak<-rbind(chart_postpeak_small, chart_postpeak_large)

gam_postpeak<-ggplot(chart_postpeak, aes(x=Variable, y=Coef_Estimate, color=factor(type))) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variables", color="Field Size Threshhold") 

gam_postpeak

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_postpeak_pres.png", 
	width = 27.8, height = 21, units = "cm", res=300, pointsize=10)
print(gam_postpeak)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_postpeak_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_postpeak)
dev.off()




#Pre peak*******************************************  #
gam_prepeak_small_tro<-gam(year_prod~ s(time_to_peak, recoverable_oil) +
	oil_price_real +
	oil_price_real_l1+
	oil_price_real_l2 +
	oil_price_real_l3 +
	oil_price_real_l4 +
	oil_price_real_l5 +
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$large_field=="small",])


gam_prepeak_large_tro<-gam(year_prod~ s(time_to_peak, recoverable_oil) +
	oil_price_real +
	oil_price_real_l1+
	oil_price_real_l2 +
	oil_price_real_l3+
	oil_price_real_l4+
	oil_price_real_l5+
	oil_price_real_l6 +
	oil_price_real_l7+
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$large_field=="large" &
		 (fields_p$name!="EKOFISK" | fields_p$year<=1976 ),])


#pooled
gam_prepeak_pooled_tro<-gam(year_prod~ s(time_to_peak, recoverable_oil) +
	oil_price_real +
	oil_price_real_l1 + 
	oil_price_real_l2 + 
	oil_price_real_l3 + 
	oil_price_real_l4 + 
	oil_price_real_l5 + 
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & (fields_p$name!="EKOFISK" | fields_p$year<=1976 ),])

summary(gam_prepeak_small_tro)
summary(gam_prepeak_large_tro)
summary(gam_prepeak_pooled_tro)

#using max_prod

gam_prepeak_small<-gam(year_prod~ s(time_to_peak, max_prod) +
	oil_price_real +
	oil_price_real_l1+
	oil_price_real_l2 +
	oil_price_real_l3 +
	oil_price_real_l4 +
	oil_price_real_l5 +
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$large_field=="small",])


gam_prepeak_large<-gam(year_prod~ s(time_to_peak, max_prod) +
	oil_price_real +
	oil_price_real_l1 +
	oil_price_real_l2 +
	oil_price_real_l3 +
	oil_price_real_l4 +
	oil_price_real_l5 +
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$large_field=="large" & fields_p$name!="EKOFISK",])


#pooled
gam_prepeak_pooled<-gam(year_prod~ s(time_to_peak, max_prod) +
	oil_price_real +
	oil_price_real_l1 + 
	oil_price_real_l2 + 
	oil_price_real_l3 + 
	oil_price_real_l4 + 
	oil_price_real_l5 + 
	oil_price_real_l6 +
	oil_price_real_l7 +
	oil_price_real_l8,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$name!="EKOFISK",])

summary(gam_prepeak_small)
summary(gam_prepeak_large)
summary(gam_prepeak_pooled)



#make table, pre-peak

texreg(list(gam_prepeak_small_tro, gam_prepeak_large_tro, gam_prepeak_pooled_tro))


#make chart, pre-peak

coef(gam_prepeak_pooled_tro)

chart_prepeak_small<-sim_gam(model=gam_prepeak_small_tro,start=2,stop=10)
chart_prepeak_large<-sim_gam(gam_prepeak_large_tro, 2, 10)
chart_prepeak_pooled<-sim_gam(gam_prepeak_pooled_tro, 2, 10)

chart_prepeak_small$type<-"under"
chart_prepeak_large$type<-"over"
chart_prepeak_pooled$type<-"pooled"
chart_prepeak<-rbind(chart_prepeak_small, chart_prepeak_large, chart_prepeak_pooled)

gam_prepeak<-ggplot(chart_prepeak, aes(x=Variable, y=Coef_Estimate, color=factor(type))) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variables", color="Field Size Threshhold") 

gam_prepeak

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_prepeak_pres.png", 
	width = 27.8, height = 21, units = "cm", res=300, pointsize=10)
print(gam_prepeak)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_prepeak_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_prepeak)
dev.off()







 
#robustness checks***********************************************************************
gam.check(gam_price_under_2d)
gam.check(gam_price_over_2d)

#check with gamma
gam_price_under_2d_gamma<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_2d_gamma<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

summary(gam_price_under_2d_gamma)
summary(gam_price_over_2d_gamma)

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/qq_gam_price_under_2d_gamma.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
qq.gam(gam_price_over_2d_gamma) 
dev.off()



#perhaps getting overfitting, change gamma (the parameter, not the distribution) to 1.4
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,], gamma=1.4)

gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,], gamma=1.4)

summary(gam_price_under_2d)
summary(gam_price_over_2d)



texreg(list(gam_price_under_2d, gam_price_over_2d))

#do not include ekofisk
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split & fields_p$name!="EKOFISK",])

gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split & fields_p$name!="EKOFISK",])
summary(gam_price_under_2d)
summary(gam_price_over_2d)

#created fitted data
fields_p$smooth_price[fields_p$max_prod<=split]<-gam_price_under_2d$fitted.values
fields_p$smooth_price[fields_p$max_prod>split]<-gam_price_over_2d$fitted.values


#compare to gamma distribution
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])






#effect on investment******************************************



#effect of oil prices on investments:*********************

gam_price_invest_under<-gam(investmentMillNOK_real~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	year_prod + oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 +
	oil_price_real_l5 + oil_price_real_l6+ oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_invest_over<-gam(investmentMillNOK_real~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	year_prod + oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 +
	oil_price_real_l5 + oil_price_real_l6+ oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

gam_price_invest<-gam(investmentMillNOK_real~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	year_prod +
	oil_price_real*large_field + 
	oil_price_real_l1*large_field  + 
	oil_price_real_l2*large_field  + 
	oil_price_real_l3*large_field  + 
	oil_price_real_l4+ 
	oil_price_real_l5+ 
	oil_price_real_l6+ 
	oil_price_real_l7+ 
	oil_price_real_l8,
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p)



summary(gam_price_invest_under)
summary(gam_price_invest_over)
summary(gam_price_invest)

#make table of investment regressions
texreg(list(gam_price_invest, gam_price_invest_under, gam_price_invest_over))

#make chart of ivvestment regressions
chart_invest<-sim_gam(model=gam_price_invest,start=3, stop=12)
chart_invest$Variable<-as.character(chart_invest$Variable)
chart_invest<-chart_invest[chart_invest$Variable!="large_fieldsmall",]

invest_pooled<-ggplot(chart_invest, aes(x=Variable, y=Coef_Estimate)) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variables", color="Field Size Threshhold") 

invest_pooled

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/invest_pooled.png", 
	width = 27.8, height = 21, units = "cm", res=300, pointsize=10)
print(invest_pooled)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/invest_pooled_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(invest_pooled)
dev.off()


#Investment in build-out phase. 

#Investment with Ekofisk, but only up to 1976

#pooled
gam_invest_build_out<-gam(investmentMillNOK_real~s(time_to_peak) + recoverable_oil +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 + oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$name!="EKOFISK",])

gam_invest_build_out_small<-gam(investmentMillNOK_real~s(time_to_peak) + recoverable_oil +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 + oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$large_field=="small",])

gam_invest_build_out_large<-gam(investmentMillNOK_real~s(time_to_peak) + recoverable_oil +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 + oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), weights=recoverable_oil, 
	data=fields_p[fields_p$after_peak==0 & fields_p$large_field=="large"& 
		fields_p$name!="EKOFISK",])


summary(gam_invest_build_out)
summary(gam_invest_build_out_small)
summary(gam_invest_build_out_large)

#investment in Decline phase

gam_invest_decline<-gam(investmentMillNOK_real~s(producing_time, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 + oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1,])

gam_invest_decline_small<-gam(investmentMillNOK_real~s(producing_time, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 + oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1 & fields_old$large_field=="small",])

gam_invest_decline_large<-gam(investmentMillNOK_real~s(producing_time, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 + oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	family=gaussian(link=log), weights=recoverable_oil, 
	data=fields_old[fields_old$after_peak==1 & fields_old$large_field=="large",])


summary(gam_invest_decline)
summary(gam_invest_decline_small)
summary(gam_invest_decline_large)

#***********************************


#Full set
gam_price<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil)  + 
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p)

gam_price_unweighted<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil)  + 
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), data=fields_p)

gam_price2<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil)  + 
	s(oil_price_real_l4) + s(oil_price_real_l5) + s(oil_price_real_l6),
	family=gaussian(link=log), weights=recoverable_oil,data=fields_p)



















#experimentation - not in paper************************************************



#Have to be careful about price and effect of year dummies - remove affect of year

#under group
gam_price_under<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_under_gamma<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])



#over group


gam_price_over<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

gam_price_over_gamma<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])



gam_price_over_2d_s_price<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end) + recoverable_oil +
	s(oil_price_real),
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])
summary(gam_price_over_2d_s_price)

gam_price_over_2d_single<-gam(year_prod~s(I(-time_to_peak+peak_to_end), recoverable_oil) + 
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])


#ridge regression
#gam_ridge<-


fields_p$smooth_price[fields_p$max_prod<=split]<-gam_price_under$fitted.values
fields_p$smooth_price[fields_p$max_prod>split]<-gam_price_over$fitted.values
#fields_p$smooth_price_2[fields_p$max_prod<=split]<-gam_price_under2$fitted.values
#fields_p$smooth_price_2[fields_p$max_prod>split]<-gam_price_over2$fitted.values


#compare direct price/non price comparision
gam_under<-gam(year_prod~s(I(time_to_peak*recoverable_oil)) + s(I(peak_to_end*recoverable_oil)),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_over<-gam(year_prod~s(I(time_to_peak*recoverable_oil)) + s(I(peak_to_end*recoverable_oil)),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

fields_p$smooth_split[fields_p$max_prod<=split]<-gam_under$fitted.values
fields_p$smooth_split[fields_p$max_prod>split]<-gam_over$fitted.values

#test of coefficients on price


summary(gam_price)
summary(gam_price2)
summary(gam_price_unweighted)

summary_under <- summary(gam_price_under)
summary_under

summary_under_gamma<-summary(gam_price_under_gamma)
summary_under_gamma


summary_over <- summary(gam_price_over)
summary_over

summary_over_gamma <- summary(gam_price_over_gamma)
summary_over_gamma

summary_over_2d_single <- summary(gam_price_over_2d_single)
summary_over_2d_single


summary_under2 <- summary(gam_price_under2)
summary_over2 <- summary(gam_price_over2)
summary_under2
summary_over2

summary_comp_under<-summary(gam_under)
summary_comp_over<-summary(gam_over)
summary_comp_under
summary_comp_over

coef_under<-data.frame(summary_under$p.table)
coef_under$variable<-row.names(coef_under)
coef_under$field_type<-"small"
coef_over<-data.frame(summary_over$p.table)
coef_over$variable<-row.names(coef_over)
coef_over$field_type<-"large"

coef_split_price<-rbind(coef_under, coef_over)
coef_split_price[c("Estimate", "Std..Error")]<-coef_split_price[c("Estimate", "Std..Error")]*100

coeff_split_plot<-ggplot(coef_split_price[c(2:7, 9:14),]) +
geom_bar(aes(x=variable, y=Estimate), stat="identity") +
geom_errorbar(aes(x=variable, ymin=Estimate-2*Std..Error, ymax=Estimate+2*Std..Error )) +
facet_wrap(~field_type) +
labs(y="Effect of Oil Prices on Norwegian Oil Production, % per 10$")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/coeff_split_plot.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(coeff_split_plot)
dev.off()


#simulation of coefficients****************************************
#from Gelman ARM
# cov_beta<-gam_under$Vp
# beta_hat<-gam_under$coefficients
# sigma_hat<-sqrt(gam_under$sig2)
# n_minus_k<-gam_under$df.residual

# sim_gam<-function(cov_beta, beta_hat, sigma_hat, n_minus_k){
# sigma<-sigma_hat*sqrt((n_minus_k)/rchisq(1,n_minus_k))
# beta<-mvrnorm(1, beta_hat, cov_beta*sigma^2)
# return(beta=beta)
# }

# nsims<-1000
# #under_sims<-array(NA, dim=nsims)
# under_sims<-replicate(nsims, sim_gam(gam_under$Vp,gam_under$coefficients, sqrt(gam_under$sig2), gam_under$df.residual))
# #chart in big fields. 

#instructions from ARM
# sim_gam_price<-sim(gam_price_over_2d, n.sims=1000)
# price_coef_sim<-melt(sim_gam_price@coef[,c(8:13)],)
# price_coef_sim$Var1<-NULL
# names(price_coef_sim)<-c("coefficient", "estimate")

#from instructions GAM with R

#gam_price_over
beta<-coef(gam_price_over_2d)
Vb<-vcov(gam_price_over_2d)

## simulate replicate beta vectors from posterior...
Cv <- chol(Vb)  #cholesky decomposition the equivalent of taking square root in a single variance to get SD?
n.rep=1000
nb <- length(beta)
br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta  

#chart replicate beta vectors
price_br_over<-t(br[c(2:8),])
price_br_over_long<-data.frame(melt(price_br_over))
names(price_br_over_long)<-c("id", "Variable", "Coef_Estimate")

gam_price_over_dirty_box<-ggplot(price_br_over_long, aes(x=Variable, y=Coef_Estimate)) +
geom_boxplot() +
geom_hline(yintercept=0) +
geom_jitter(, alpha=.1)
gam_price_over_dirty_box

#presentation version
png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_over_dirty_box_pres.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_over_dirty_box)
dev.off()

#print version
png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_over_dirty_box_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_over_dirty_box)
dev.off()

#gam price under
beta<-coef(gam_price_under_2d)
Vb<-vcov(gam_price_under_2d)

## simulate replicate beta vectors from posterior...
Cv <- chol(Vb)  #cholesky decomposition the equivalent of taking square root in a single variance to get SD?
n.rep=1000
nb <- length(beta)
br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta  

#chart replicate beta vectors
price_br_under<-t(br[c(2:8),])
price_br_under_long<-data.frame(melt(price_br_under))
names(price_br_under_long)<-c("id", "Variable", "Coef_Estimate")

gam_price_under_dirty_box<-ggplot(price_br_under_long, aes(x=Variable, y=Coef_Estimate)) +
geom_boxplot() +
geom_jitter(, alpha=.1) +
geom_hline(yintercept=0)
print(gam_price_under_dirty_box)


png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/gam_price_under_dirty_box.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_under_dirty_box)
dev.off()

#combined over and under on one plot
price_br_under_long$type<-"under"
price_br_over_long$type<-"over"
price_br_long<-rbind(price_br_under_long, price_br_over_long)

gam_price_dirty_box<-ggplot(price_br_long, aes(x=Variable, y=Coef_Estimate, color=factor(type))) +
geom_boxplot(position="dodge") +
#geom_jitter(, alpha=.1) +
scale_color_grey() +
geom_hline(yintercept=0) +
labs(x="", y="Estimated Coefficients on Oil Price Variable", color="Field Size Threshhold") 
gam_price_dirty_box

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_pres.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_dirty_box)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_price_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_dirty_box)
dev.off()

coef(gam_price_under_2d_8)
#with 8 lags
sim_gam<-function(model, start=1, stop=length(coef(model))){
	#test
	#model<-gam_price_under_2d_8
	#start<-2
	#stop<-10
	#

	beta<-coef(model)
	Vb<-vcov(model)
	## simulate replicate beta vectors from posterior...
	Cv <- chol(Vb)  #cholesky decomposition the equivalent of taking square root in a single variance to get SD?
	n.rep=1000
	nb <- length(beta)
	br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta  

	#chart replicate beta vectors
	out_data<-t(br[c(start:stop),])
	out_data_long<-data.frame(melt(out_data))
	names(out_data_long)<-c("id", "Variable", "Coef_Estimate")
	return(out_data_long)
	}

chart_under_8<-sim_gam(model=gam_price_under_2d_8,start=2,stop=10)
chart_over_8<-sim_gam(gam_price_over_2d_8,2,10)

gam_price_under_8<-ggplot(chart_under_8, aes(x=Variable, y=Coef_Estimate)) +
geom_boxplot() +
geom_jitter(, alpha=.1) +
geom_hline(yintercept=0)
gam_price_under_8

gam_price_over_8<-ggplot(chart_over_8, aes(x=Variable, y=Coef_Estimate)) +
geom_boxplot() +
geom_jitter(, alpha=.1) +
geom_hline(yintercept=0)
gam_price_over_8





