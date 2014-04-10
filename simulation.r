library(ggplot2) #for plotting
library(mgcv) #for gam modeling
library(lubridate) #for extracting "year" component
library(plyr) 
library(tseries)
library(zoo)
library(reshape2)

#simulation.r

#import oil prices
#oil prices brent
#brent_price<-read.csv("/Users/johannesmauritzen/Google Drive/github/rOil/brent_price.csv")
brent_price<-read.csv
qplot(year, oil_price_real, data=brent_price, geom="line")

#import data of individual fields
unique_fields<-read.csv("/Users/johannesmauritzen/Google Drive/github/rOil/unique_oil_fields.csv")

#First Look at actual data *****************************************************
#distribution of size looks about log-normal
unique_fields<-unique_fields[unique_fields$recoverable_oil!=0,]
hist(unique_fields$recoverable_oil)
hist(log(unique_fields$recoverable_oil))

#log size mean of about 2.5
mean(log(unique_fields$recoverable_oil), na.rm=TRUE)

#log sd of about 1.8
sd(log(unique_fields$recoverable_oil), na.rm=TRUE)

#get first year of production from each field
prod_year<-year(as.Date(as.character(unique_fields$producing_from)))
prod_year<-prod_year[!is.na(prod_year)]
hist(prod_year)

#first production year and field size
qplot(prod_year, unique_fields$recoverable_oil[!is.na(unique_fields$producing_from)])



#look at oil prices post 1960.  
oil_price_series<-qplot(year, oil_price_real, data=brent_price[brent_price$year>1960,], geom="line", xlab="", 
	ylab="Real Oil Price, 2010 $ per Barrel")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/oil_price_series.png", 
	width = 27.81, height = 21, units = "cm", res=50, pointsize=10)
oil_price_series
dev.off()


acf(brent_price$oil_price_real)
pacf(brent_price$oil_price_real)

oil_price_real_short<-zoo(brent_price$oil_price_real[brent_price$year>1960], 
	order.by=brent_price$year[brent_price$year>1960])

arima_mod<-arima(oil_price_real_short, order=c(1,0,0))
sd(arima_mod$residuals)

#Shown to be non-stationary (or can not reject stationary)
adf.test(oil_price_real_short)

#model as oil prices as arima
arima_model_oil<-arima(oil_price_real_short, order=c(0,1,0))






# generate pre-data ************************************************ 
#random generation of oil field sizes
field_size<-round(exp(rnorm(77,mean=2.3, sd=1.5)), digits=1)
#hist(field_size)



#make function that inputs simulated field_size and outputs simulated first production year
#should be so that large fields are found more often early on.  
gen_year<-function(size, maxsize){
	#test
	#size=15
	#maxsize=1267
	#

	#let small fields be distributed uniformly from 1975 to 2013
	if(size<10){
		year<-trunc(runif(1, 1975, 2008))	
	} else{	#while large fields should be more common earlier on
		range<-FALSE
		while(range==FALSE){
			year<-trunc(rnorm(1,mean=(1973+(maxsize+300)/(size+300)), sd=10))
			ifelse(year>=1970 & year<=2013, range<-TRUE, range<-FALSE)
			#print(paste("in while loop", year))
			}	
		}
return(year)	
}

max<-max(field_size)

init_year<-trunc(sapply(field_size, gen_year, maxsize=max))
#hist(init_year)

#plot simulated initial year with simualted field_size
#qplot(init_year, field_size)



#simulated price data, stationary
years=1960:2014
episolons=rnorm(55,0,5)
prices = pmax((arima.sim(n=55, list(ar=.9), innov=episolons)+30), rep(5,55))
simul_prices<-data.frame(years, prices)









#Simulation of data****************************************************************

gam_mc<-function(formula, beta0=.20, use_true_prices=TRUE){ 
#fm - mean log field size, 
#fsd - mean log field standard dev.
#formula - gams formula 

#test
  # formula=formula(prod~s(time_to_peak, size) + s(peak_to_end, size) + price)

   # use_true_prices=TRUE
   # ar_par=.9
   # beta0=.20
#

#create function with inputs of field size, start year and oil price series
#and which then generates a production profile of a field

gen_production<-function(field, prices){
	#test
	#field=cbind(8, 2006,1)
	#prices=simul_prices
	#

	size<-field[1]
	start<-field[2]
	name<-field[3]
	#print(size)
	#print(start)
	names(prices)<-c("year", "price")
	#effect of oil prices
	#t is half the production years
	t<-trunc(sqrt(size)) + 3
	prod_time<- -t:(t)
	year=start:(start+2*t-1)

	#use a cumulative logistic function to represent shape of function
	cum_production = 1/(1+exp(-(prod_time)/3))*size

	#take difference to create production per year
	prod_shape = diff(cum_production)
	production<-data.frame(year, prod_shape)
	production<-merge(production, prices, by="year")
	#formula prodction = f(time)*exp(beta)*
	  #log(production) = f`(time) + beta*log(price)
	production$prod<-production$prod_shape*
	(exp(beta0*(production$price))*
		rlnorm(length(production$price), meanlog=0, sdlog=.05))
	
	production$name<-name
	production$prod_time<-1:length(production$prod)
	production$size<-size
	production$mean_price<-mean(production$price)
	return(production)
}

fields<-cbind(field_size, init_year,1:length(init_year))
	
if(use_true_prices==TRUE){
	prices<-brent_price[c("year", "oil_price_real")]

} else{
	prices<-simul_prices
}
names(prices)<-c("year", "prices")

#make units into 10 dollars units
prices$prices<-prices$prices/10


sim_fields<-apply(fields, 1, gen_production, prices=prices)

sim_fields<-Reduce(rbind, sim_fields)

 simulated_production<-ggplot(sim_fields)+
 geom_line(aes(x=year, y=prod, color=factor(name)),alpha=.3) +
 guides(color=FALSE) +
 labs(x="", y="Simulated Production from Fields")

 png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/simulated_production.png", 
 	width = 27.81, height = 21, units = "cm", res=50, pointsize=10)
 simulated_production
 dev.off()

#GAM simualtion *********************************

sim_fields<-ddply(sim_fields, .(name), mutate, max_year=prod_time[max(prod)==prod])
sim_fields<-ddply(sim_fields, .(name), mutate, time_to_peak=ifelse(prod_time<=max_year, prod_time, 0))
sim_fields<-ddply(sim_fields, .(name), mutate, peak_to_end=ifelse(prod_time>max_year, prod_time-max_year, 0))

sim_fields$time_to_peak_sq=sim_fields$time_to_peak^2
sim_fields$time_to_peak_cu=sim_fields$time_to_peak^3
sim_fields$peak_to_end_sq=sim_fields$peak_to_end^2
sim_fields$peak_to_end_cu=sim_fields$peak_to_end^3


gam_sim<-gam(formula,
	family=gaussian(link=log), weights=size, data=sim_fields)
summary(gam_sim)

return(coefficients(gam_sim)["price"])

}



#****************************************************

#bad formula


formula_0=formula(prod~time_to_peak + time_to_peak_sq + time_to_peak_cu + 
	peak_to_end + peak_to_end_sq + peak_to_end_cu +size + price)
gamm_mc_0<-replicate(1000, gam_mc(beta=0, formula=formula_0, use_true_prices=TRUE))

gamm_mc_0<-as.data.frame(gamm_mc_0)

lin_model_price_mc<-ggplot(gamm_mc_0, aes(x=gamm_mc_0)) + 
geom_histogram(aes(y=..density..)) +
geom_density() +
xlab("Estimated Coefficient on Price")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/lin_model_price_mc.png", 
	width = 27.81, height = 21, units = "cm", res=50, pointsize=10)
lin_model_price_mc
dev.off()


formula_1= formula(prod~s(prod_time,size) + price)

gamm_mc_1<-replicate(1000, gam_mc(beta0=0, formula=formula_1, use_true_prices=TRUE))
gamm_mc_1<-as.data.frame(gamm_mc_1)

gam_model_price_mc<-ggplot(gamm_mc_1, aes(x=gamm_mc_1)) + 
geom_histogram(aes(y=..density..)) +
geom_density() +
xlab("Estimated Coefficient on Price")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gam_model_price_mc.png", 
	width = 27.81, height = 21, units = "cm", res=50, pointsize=10)
gam_model_price_mc
dev.off()

#  formula_2= formula(prod~s(time_to_peak, size) + s(peak_to_end, size) + price)
#  gamm_mc_2<-replicate(25, gam_mc(beta0=.20, formula=formula_2, use_true_prices=FALSE))
#  qplot(gamm_mc_2, geom="histogram")

# gamm_mc<-data.frame(cbind(gamm_mc_1, gamm_mc_2))
# gamm_mc_long<-melt(gamm_mc)

# gam_mc_plot_2<-ggplot(gamm_mc_long, aes(x=value, fill=variable)) +
# geom_histogram(position="identity", alpha=.3)

# png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gamm_mc_plot_2.png", 
# 	width = 35, height = 21, units = "cm", res=300, pointsize=10)
# print(gamm_mc_plot_2)
# dev.off()


# formula_3= formula(prod~s(prod_time)+ size + price)
# gamm_mc_3<-replicate(200, gam_mc(beta0=.05, formula=formula_3, use_true_prices=FALSE))
# qplot(gamm_mc_3, geom="histogram")


# formula_4= formula(prod~s(prod_time)+ size + price)
# gamm_mc_4<-replicate(200, gam_mc(beta0=.05, formula=formula_3, use_true_prices=TRUE))
# qplot(gamm_mc_4, geom="histogram")	

# gamm_mc<-data.frame(cbind(gamm_mc_3, gamm_mc_4))
# gamm_mc_long<-melt(gamm_mc)

# gam_mc_plot<-ggplot(gamm_mc_long, aes(x=value, fill=variable)) +
# geom_histogram(position="identity", alpha=.3)

# png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/gamm_mc_plot.png", 
# 	width = 35, height = 21, units = "cm", res=300, pointsize=10)
# print(gamm_mc_plot)
# dev.off()







