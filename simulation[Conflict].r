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
brent_price<-read.csv("/Users/johannesmauritzen/Google Drive/github/rOil/brent_price.csv")
#qplot(year, oil_price_real, data=brent_price, geom="line")

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
qplot(year, oil_price_real, data=brent_price[brent_price$year>1960,], geom="line")
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














#Simulation of data****************************************************************

gam_mc<-function(fm, fsd, formula, use_true_prices=TRUE, ar_par=.9){ 
#fm - mean log field size, 
#fsd - mean log field standard dev.
#formula - gams formula 

#test
#fm=2.5
#fsd=1.3
#formula= formula(prod~s(prod_time)+ size + price)
#use_true_prices=FALSE
#ar_par=.9
#

#random generation of oil field sizes
field_size<-round(exp(rnorm(77,mean=fm, sd=fsd)), digits=1)
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
prices = pmax((arima.sim(n=55, list(ar=ar_par), innov=episolons)+30), rep(5,55))
simul_prices<-data.frame(years, prices)
#create function with inputs of field size, start year and oil price series
#and which then generates a production profile of a field

gen_production<-function(field, prices){
	#test
	#field=cbind(8, 2006,1)
	#

	size<-field[1]
	start<-field[2]
	name<-field[3]
	#print(size)
	#print(start)
	names(prices)<-c("year", "price")
	#effect of oil prices
	beta0=.02
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
	(exp(beta0*(production$price-mean(production$price))/10))*
		rlnorm(length(production$price), meanlog=0, sdlog=.05)
	
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

sim_fields<-apply(fields, 1, gen_production, prices=prices)

sim_fields<-Reduce(rbind, sim_fields)

ggplot(sim_fields)+
geom_line(aes(x=year, y=prod, color=factor(name)),alpha=.3) 


#GAM simualtion *********************************

sim_fields<-ddply(sim_fields, .(name), mutate, max_year=prod_time[max(prod)==prod])
sim_fields<-ddply(sim_fields, .(name), mutate, time_to_peak=ifelse(prod_time<=max_year, prod_time, 0))
sim_fields<-ddply(sim_fields, .(name), mutate, peak_to_end=ifelse(prod_time>max_year, prod_time-max_year, 0))


gam_sim<-gam(formula,
	family=gaussian(link=log), weights=size, data=sim_fields)
#summary(gam_sim)

return(coefficients(gam_sim)[3])

}



#****************************************************


formula_0=formula(prod~s(time_to_peak) + s(peak_to_end) + size + price)
gamm_mc_0<-replicate(200, gam_mc(fm=2.3, fsd=1.5, formula=formula_0), )
qplot(gamm_mc_0, geom="histogram")


formula_1= formula(prod~s(time_to_peak, size) + s(peak_to_end, size) + price)
gamm_mc_1<-replicate(200, gam_mc(fm=2.3, fsd=1.5, formula=formula_1), )
qplot(gamm_mc_1, geom="histogram")

formula_2= formula(prod~s(time_to_peak) + s(peak_to_end) + size + price)
gamm_mc_2<-replicate(200, gam_mc(fm=2.3, fsd=1.5, formula=formula_2), )
qplot(gamm_mc_2, geom="histogram")


formula_3= formula(prod~s(prod_time)+ size + price)
gamm_mc_3<-replicate(200, gam_mc(fm=2.3, fsd=1.5, formula=formula_3, use_true_prices=FALSE, ar_par=.9))
qplot(gamm_mc_3, geom="histogram")


formula_4= formula(prod~s(prod_time)+ size + price)
gamm_mc_4<-replicate(200, gam_mc(fm=2.3, fsd=1.5, formula=formula_3, use_true_prices=TRUE))
qplot(gamm_mc_4, geom="histogram")

gamm_mc<-data.frame(cbind(gamm_mc_3, gamm_mc_4))
gamm_mc_long<-melt(gamm_mc)

ggplot(gamm_mc_long, aes(x=value, fill=variable)) +
geom_histogram()





