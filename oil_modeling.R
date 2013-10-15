#temp file for oil work

library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv)
library(lubridate) 
library(grid)
library(boot)
library(arm)



#run fresh data import and clean:
#source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_clean.r") 

#prep data for modeling
source("/Users/johannesmauritzen/Google Drive/github/rOil/oil_modeling_prep.r") 


#gam modeling*************************************************************************

#compare to glm model (not correctly dealing with non-linearity)

glm_comp<-glm(year_prod~time_to_peak + I(time_to_peak^2) + I(time_to_peak^3) + 
	peak_to_end + I(peak_to_end^2) +
	recoverable_oil + oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), data=fields_p)

sum_glm_com<-summary(glm_comp)
glm_data<-data.frame(coef=sum_glm_com$coefficients)
glm_data$variable<-row.names(glm_data)

#make into percent
glm_data[c("coef.Estimate","coef.Std..Error")]<-glm_data[c("coef.Estimate","coef.Std..Error")]*100

coeff_plot<-ggplot(glm_data[c(8:13),]) +
geom_bar(aes(x=variable, y=coef.Estimate), stat="identity") +
geom_errorbar(aes(x=variable, ymin=coef.Estimate-2*coef.Std..Error, ymax=coef.Estimate+2*coef.Std..Error ))

glm_coef_plot<-coeff_plot +
labs(y="Effect of Oil Price on Norwegian Oil Production % per $10, GLM Model")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/glm_coef_plot.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(glm_coef_plot)
dev.off()

#using simulation for uncertainty ()
sim_glm<-sim(glm_comp, n.sims=1000)
price_coef_sim<-melt(sim_glm@coef[,c(8:13)],)
price_coef_sim$Var1<-NULL
names(price_coef_sim)<-c("coefficient", "estimate")

glm_dirty_box<-ggplot(price_coef_sim, aes(x=coefficient, y=estimate)) +
geom_boxplot() +
geom_jitter(, alpha=.1)

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/glm_dirty_box.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(glm_dirty_box)
dev.off()




#benchmark model - uses non-parametric function on both analysis time and year
prod_gam<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) + s(year), 
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p)

gam.check(prod_gam)


fields_p$smooth_bench<-prod_gam$fitted.values

#prod_gam2<-gam(year_prod~s(time_to_peak) + s(peak_to_end) + (recoverable_oil) + year, 
#	family=Gamma(link=log), weights=recoverable_oil, data=fields_p)

#fields_p$smooth_bench_dummies<-prod_gam2$fitted.values

#benchmark model split into under and over a certain limit
split<-8

prod_gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +  s(year), 
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])
prod_gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) + s(year), 
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])


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

#Full set
gam_price<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p)

gam_price2<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	s(oil_price_real_l4) + s(oil_price_real_l5) + s(oil_price_real_l6),
	family=gaussian(link=log), weights=recoverable_oil,data=fields_p)


#Have to be careful about price and effect of year dummies - remove affect of year
gam_price_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

gam_price_under2<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	s(oil_price_real_l4) + s(oil_price_real_l5) + s(oil_price_real_l6),
	family=gaussian(link=log), weights=recoverable_oil,data=fields_p[fields_p$max_prod<=split,])

gam_price_over2<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +
	+ s(oil_price_real_l4) + s(oil_price_real_l5) + s(oil_price_real_l6),
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

fields_p$smooth_price[fields_p$max_prod<=split]<-gam_price_under$fitted.values
fields_p$smooth_price[fields_p$max_prod>split]<-gam_price_over$fitted.values

#fields_p$smooth_price_2[fields_p$max_prod<=split]<-gam_price_under2$fitted.values
#fields_p$smooth_price_2[fields_p$max_prod>split]<-gam_price_over2$fitted.values



#model as a logit with 1 being the peak?  0 being min.  


#compare direct price/non price comparision
gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

fields_p$smooth_split[fields_p$max_prod<=split]<-gam_under$fitted.values
fields_p$smooth_split[fields_p$max_prod>split]<-gam_over$fitted.values


#test of coefficients on price
sum_full<-summary(gam_price)
sum_full2<-summary(gam_price2)

summary_under <- summary(gam_price_under)
summary_over <- summary(gam_price_over)
summary_under
summary_over

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


#simulation of coefficients
cov_beta<-gam_under$Vp
beta_hat<-gam_under$coefficients
sigma_hat<-sqrt(gam_under$sig2)
n_minus_k<-gam_under$df.residual

sim_gam<-function(cov_beta, beta_hat, sigma_hat, n_minus_k){
sigma<-sigma_hat*sqrt((n_minus_k)/rchisq(1,n_minus_k))
beta<-mvrnorm(1, beta_hat, cov_beta*sigma^2)
return(beta=beta)
}

nsims<-1000
#under_sims<-array(NA, dim=nsims)
under_sims<-replicate(nsims, sim_gam(gam_under$Vp,gam_under$coefficients, sqrt(gam_under$sig2), gam_under$df.residual))
#chart in big fields. 


	
	
#test on statfjord
gam_statfjord<-ggplot(fields_p[fields_p$name=="STATFJORD",]) +
geom_point(aes(x=year, y=year_prod, size=oil_price_real)) +
geom_line(aes(x=year, y=smooth_split), color="blue") +
geom_line(aes(x=year, y=smooth_price), color="red") +
labs(title="Statfjord")
gam_statfjord

gam_ekofisk<-ggplot(fields_p[fields_p$name=="EKOFISK",]) +
geom_point(aes(x=year, y=year_prod, size=oil_price_real)) +
geom_line(aes(x=year, y=smooth_split), color="blue") +
geom_line(aes(x=year, y=smooth_price), color="red") +
labs(title="Ekofisk")
gam_ekofisk



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

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/bench_vs_split.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(bench_vs_split)
dev.off()

#compare split benchmark with split price
bench_vs_price<-bench_vs_split %+%
fields_long[fields_long$smoothing_type %in% c("Split Benchmark", "With Price"),] +
scale_color_manual(values=c("red", "orange"))

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/bench_vs_price.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(bench_vs_price)
dev.off()

#compare split with split price
price_vs_non_price<-bench_vs_split %+%  
fields_long[fields_long$smoothing_type %in% c("With Price", "Non-Price"),] +
geom_line(aes(x=year, y=smoothed, color=smoothing_type)) +
scale_color_manual(values=c("orange", "green"))


png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/price_vs_non_price.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(price_vs_non_price)
dev.off()
#compare split eko with and without
#multi_gam_plot %+% fields_lim + 
#geom_line(aes(x=year, y=smooth_prod4, sep=""), color="blue") +
#geom_line(aes(x=year, y=smooth_eko), color="orange")










