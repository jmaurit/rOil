#temp file for oil work

library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv)
library(lubridate) 
library(grid)
library(boot)
library(arm)

#clear workspace
rm(list = ls())

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

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/glm_coef_plot.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(glm_coef_plot)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/glm_coef_plot_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
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

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/glm_dirty_box.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(glm_dirty_box)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/glm_dirty_box_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(glm_dirty_box)
dev.off()

#benchmark model - uses non-parametric function on both analysis time and year
prod_gam<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) + s(year), 
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p)

fields_p$smooth_bench<-prod_gam$fitted.values

#benchmark model split into under and over a certain limit
split<-8

prod_gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) +  s(year), 
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])
prod_gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(peak_to_end, recoverable_oil) + s(year), 
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])


fields_p$smooth_bench_split[fields_p$max_prod<=split]<-prod_gam_under$fitted.values
fields_p$smooth_bench_split[fields_p$max_prod>split]<-prod_gam_over$fitted.values

#Now see if oil prices have an effect
#produces gam estimates gam_price and predicted values smooth_price
#source("/Users/johannesmauritzen/Google Drive/github/rOil/price_modeling.r")


#test on statfjord
gam_statfjord<-ggplot(fields_p[fields_p$name=="STATFJORD",]) +
geom_point(aes(x=year, y=year_prod, size=oil_price_real)) +
geom_line(aes(x=year, y=smooth_bench_split), color="blue") +
labs(title="Statfjord")
gam_statfjord

gam_ekofisk<-ggplot(fields_p[fields_p$name=="EKOFISK",]) +
geom_point(aes(x=year, y=year_prod, size=oil_price_real)) +
geom_line(aes(x=year, y=smooth_bench_split), color="blue") +
labs(title="Ekofisk")
gam_ekofisk



#Show results Top - Down


#Show fit for following fits
include_fields<-c("ALBUSKJELL","ALVE", "COD", "EKOFISK", "TROLL","GULLFAKS", "KRISTIN", "STATFJORD", "ENOCH")

#function to to show field-level of data
fields_lim<-subset(fields_p, name %in% include_fields)

fields_lim<-fields_lim[,c("name", "year", "year_prod", "smooth_bench", 
	"smooth_bench_split")]

fields_long<-melt(fields_lim, id.vars=c("name", "year", "year_prod"))
names(fields_long)[4]<-"smoothing_type"
names(fields_long)[5]<-"smoothed"
fields_long$smoothing_type<-factor(fields_long$smoothing_type, 
	labels=c("Benchmark", "Split Benchmark"))

#show fields on their own:
field_inspection<-ggplot(fields_long) +
geom_line(aes(x=year, y=year_prod)) +
facet_wrap(~name, scales="free") +
labs(x="", y="Yearly Production, Mill SM3")

field_inspection

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/field_inspection.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(field_inspection)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/field_inspection_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(field_inspection)
dev.off()

#compare split benchmark with non-split
bench_vs_split <- ggplot(fields_long[fields_long$smoothing_type %in% c("Benchmark", "Split Benchmark"),]) +
geom_point(aes(x=year, y=year_prod)) +
geom_line(aes(x=year, y=smoothed, color=smoothing_type)) +
facet_wrap(~name, scales="free")

bench_vs_split

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/bench_vs_split.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(bench_vs_split)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/bench_vs_split_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
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



#Changes in the oil price - negative, positive








