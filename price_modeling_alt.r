library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv) #for generalized additive models
library(lubridate) 
library(grid)
library(boot)
library(arm)
#library(lme4) #lmer not available for 3.02
#library(lmer)

#try non-gam approaches
rm(list = ls())

#Price part of modeling
#price_modeling.R
# under oil_modeling.R
source("/Users/johannesmauritzen/Google Drive/github/rOil/oil_modeling_prep.r") 
split<-8

fields_p$after_peak<-ifelse(fields_p$time_to_peak==0,1,0)
fields_p$large_field<-as.factor(ifelse(fields_p$max_prod>split, "large","small"))

glm_model<- glm(log(year_prod) ~ peak_to_end^2:max_prod + peak_to_end^3:max_prod + peak_to_end*max_prod +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + 
	oil_price_real_l5 + oil_price_real_l6 + oil_price_real_l7 + oil_price_real_l8,
	, data=fields_p[fields_p$after_peak==1,])
summary(glm_model)

lmer_model<-lmer(log(year_prod) ~   peak_to_end + I(peak_to_end^2) + I(peak_to_end^3) +  
	(oil_price_real + oil_price_real_l1 + oil_price_real_l2 + 
	oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 + oil_price_real_l6 | large_field), data=fields_p[fields_p$after_peak==1,]) 

coef(lmer_model)
confint(lmer_model)
summary(lmer_model)



#after peak
gam_price_2d<-gam(year_prod~ s(peak_to_end, recoverable_oil) +
	oil_price_real +
	oil_price_real_l1 + 
	oil_price_real_l2 + 
	oil_price_real_l3 + 
	oil_price_real_l4 + 
	oil_price_real_l5 + 
	oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$after_peak==1,])

summary(gam_price_2d)

#january 2014 - use of price differences and price expectations

#original model
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real +oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])


alternative models

#using cubic regression spline basis for oil_price_real - but can zero term completly
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	s(oil_price_real, bs="cs") + s(oil_price_real_l1, bs="cs") + s(oil_price_real_l2, bs="cs") + s(oil_price_real_l3, bs="cs") + s(oil_price_real_l4, bs="cs") + s(oil_price_real_l5, bs="cs") + s(oil_price_real_l6, bs="cs"),
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])
plot(gam_price_under_2d)

summary(gam_price_under_2d)

gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6 + 
 	log(bond_index),
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

summary(gam_price_under_2d)

gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6 + 
	 log(bond_index),
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

summary(gam_price_over_2d)



#use of generalize additive mixed models (gamm)
#by before and after peak
#for large and small fields
#have fields be random effect
fields_p$after_peak<-as.factor(ifelse(fields_p$time_to_peak==0,"post-peak","pre-peak"))
fields_p$large_field<-as.factor(ifelse(fields_p$max_prod>split, "large","small"))






#gamm version - does not work
gamm_l6<-gamm(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) + 
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 +
	oil_price_real_l5 + oil_price_real_l6 + (large_field + large_field:oil_price_real_l6 - 1|large_field),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p)

#gamm4 version
gamm_l6<-gamm4(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) + 
	oil_price_real + oil_price_real_l1 + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 +
	oil_price_real_l5 + large_field,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p, 
	random=~(large_field + large_field:oil_price_real_l6 -1|large_field))

coef(gamm_l6$mer)
ranef(gamm_l6$mer)
fixef(gamm_l6$mer)

display(gamm_l6$mer)
confint(gamm_l6$mer)
vcov(gamm_l6$mer) #only fixed effects
sqrt(diag(vcov(gamm_l6$mer)))
str(gamm_l6$mer)

gamm_l6$variance

summary(gamm_l6$mer)


gamm_price_2d$mer
summary(gam_price_2d$gam)
summary(gam_price_2d$mer)

coef(gamm_price_2d$mer)




param_box<-function(beta, Vb, coef_first=1, coef_last=length(beta)){

Cv <- chol(Vb)  #cholesky decomposition the equivalent of taking square root in a single variance to get SD?
n.rep=1000
nb <- length(beta)
br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta  

#chart replicate beta vectors
price_br_over<-t(br[c(coef_first:coef_last),]) # this needs to change according to files
price_br_over_long<-data.frame(melt(price_br_over))
names(price_br_over_long)<-c("id", "variable", "estimate")

coef_box<-ggplot(price_br_over_long, aes(x=variable, y=estimate)) +
geom_boxplot() +
geom_jitter(, alpha=.1)

return(coef_box)
}

beta<-coef(gam_price_invest)
Vb<-vcov(gam_price_invest)

gam_price_invest_box<-param_box(beta, Vb, 3, 7)

gam_price_invest_box<-gam_price_invest_box + 
scale_y_continuous(limits=c(-.05, .15)) +
labs(x="", y="Estimated Coefficient")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/gam_price_invest_box.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_invest_box)
dev.off()
