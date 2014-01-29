#Oil Forecasting model


#first run bottom up modeling
#source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_modeling.r") 


#model for forecasting
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	 oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

summary_under_2d<-summary(gam_price_under_2d)
summary_over_2d<-summary(gam_price_over_2d)
summary_under_2d
summary_over_2d

gam_price_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	 oil_price_real +oil_price_real_l1 +oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), select=TRUE, weights=recoverable_oil, data=fields_p)
summary_2d<-summary(gam_price_2d)
summary_2d

#put together Smoothed results compare with total. 
fitted_year_prod<-ddply(fields_p, .(year), summarize, fitted_year_prod=sum(smooth_price))
actual_year_prod<-ddply(fields_p, .(year), summarize, actual_year_prod=sum(year_prod))
year_prod<-merge(fitted_year_prod, actual_year_prod)

tot_fitted_vs_actual<-ggplot(year_prod) +
geom_line(aes(x=year, y=actual_year_prod)) +
geom_line(aes(x=year, y=fitted_year_prod), color="red") +
labs(y="Actual(black) vs Fitted (red) Oil Production, Mill SM3", x="")

png("/Users/johannesmauritzen/Google Drive/oil/figures/tot_fitted_vs_actual.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(tot_fitted_vs_actual)
dev.off()

#create forecast
#use predict.gam()

prod_predictions<-function(field_data, model_under, model_over=NA, future_price=10)
{
	#test
	#field_data<-fields_p[fields_p$name=="TROLL",]
	#model_under<-gam_price_2d
	#model_over<-NA
	#future_price=30
	#test

	pred.years<-10
	field_name<-field_data$name[1]
	max_peak_to_end<-max(field_data$peak_to_end)
	max_year<-max(field_data$year)

	year<-(max_year+1):(max_year+pred.years)
	recoverable_oil<-rep(unique(field_data$recoverable_oil),pred.years)
	time_to_peak<-rep(0, pred.years)  #assumes all fields have peaked!!!
	peak_to_end<-as.numeric((max_peak_to_end+1):(max_peak_to_end+pred.years))
	
	actual_oil_price<-fields_active$oil_price_real[fields_active$name=="EKOFISK"]
	last_data<-length(actual_oil_price)
	oil_price_real <- seq(from=actual_oil_price[last_data], to=future_price, length.out=pred.years)
	oil_price_real_l1<-c(actual_oil_price[last_data], oil_price_real[1:(pred.years-1)])
	oil_price_real_l2<-c(actual_oil_price[(last_data-1):last_data], oil_price_real[1:(pred.years-2)])
	oil_price_real_l3<-c(actual_oil_price[(last_data-2):last_data], oil_price_real[1:(pred.years-3)])
	oil_price_real_l4<-c(actual_oil_price[(last_data-3):last_data], oil_price_real[1:(pred.years-4)])
	oil_price_real_l5<-c(actual_oil_price[(last_data-4):last_data], oil_price_real[1:(pred.years-5)])
	oil_price_real_l6<-c(actual_oil_price[(last_data-5):last_data], oil_price_real[1:(pred.years-6)])
	
	new.data<-data.frame(year, recoverable_oil, time_to_peak, peak_to_end, 
		oil_price_real,oil_price_real_l1, oil_price_real_l2, oil_price_real_l3, oil_price_real_l4,oil_price_real_l5, oil_price_real_l6)
	
	options(warn=-1)
	if(!is.na(model_over))
		#if model_over parameter provided - ie split into two
		
		{
		ifelse(field_data$max_prod<=split,
			pred_prod<-predict.gam(model_under, type="response", newdata=new.data, se.fit=TRUE),
			pred_prod<-predict.gam(model_over, type="response", newdata=new.data, se.fit=TRUE))

		}	

	if(is.na(model_over))
		#if model_over parameter is not provided - ie model not split into two
		{
			pred_prod<-predict.gam(model_under, type="response", newdata=new.data, se.fit=TRUE)
		}

	pred.data<-data.frame(name=rep(field_name, pred.years), year=year, 
	year_prod=pred_prod$fit, est_se=pred_prod$se, prediction=rep(future_price, pred.years))

	

	#test
	#troll_pred<-merge(fields_p[fields_p$name=="TROLL",], pred.data, 
	#	by=c("year","time_to_peak", "peak_to_end", "name","year_prod", "recoverable_oil"), all=TRUE)
	#ggplot(troll_pred) +
	#geom_line(aes(x=year, y=year_prod, color=prediction, group=name)) +
	#geom_ribbon(aes(x=year, ymin=year_prod-1.96*est_se, ymax=year_prod+1.96*est_se), alpha=.3)

	#test

	return(pred.data)
	}



#only include active fields
fields_active<-fields_p[is.na(fields_p$producing_to),]

make_predictions<-function(future_price){
predictions<-ddply(fields_active, .(name), prod_predictions, 
	model_under=gam_price_under_2d, model_over=gam_price_over_2d, future_price=future_price)
return(predictions)
}
future_prices<-as.array(seq(from=5, to=30, by=5))

predictions<-adply(future_prices, 1, make_predictions)

fields_pred<-merge(fields_p[,c("name", "year", "year_prod")], predictions, 
	by=c("name", "year", "year_prod"), all=TRUE)
fields_pred$prediction<-as.character(fields_pred$prediction)
fields_pred$prediction[is.na(fields_pred$prediction)]<-"actual"

#plot multi-fields
include_fields<-c("ALBUSKJELL","ALVE", "COD", "EKOFISK", "TROLL","GULLFAKS", "KRISTIN", "STATFJORD", "ENOCH")
fields_lim<-subset(fields_pred, name %in% include_fields)

multi_gam_plot <- ggplot(fields_lim) +
facet_wrap(~name, scales="free")

field_lev_forecast<-multi_gam_plot +
geom_line(aes(x=year, y=year_prod, color=prediction)) +
#geom_ribbon(aes(x=year, ymin=year_prod-1.96*est_se, ymax=year_prod+1.96*est_se), alpha=.3) +
labs(y="Oil Production, Mill SM3")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/field_lev_forecast.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(field_lev_forecast)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/field_lev_forecast_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(field_lev_forecast)
dev.off()


#sum together an dplot
sum_years<-function(year_data) c(sum(year_data$year_prod, na.rm=TRUE),sum(year_data$est_se, na.rm=TRUE))

tot.prod<-ddply(fields_pred, .(year, prediction), sum_years)
names(tot.prod)[3]<-"year_prod"
names(tot.prod)[4]<-"est_se"

#tot.prod$prediction<-factor(ifelse(tot.prod$year<2013,"actual","prediction"), labels=c("actual", "prediction"))

tot.prod$year<-as.numeric(tot.prod$year)
tot.prod$tot<-"tot"
tot.prod$est_se[tot.prod$year==2013]<-0
prod_2013<-tot.prod$year_prod[tot.prod$year==2013 & tot.prod$prediction=="actual"]
tot.prod$year_prod[tot.prod$year==2013]<-prod_2013

tot_forecast<-ggplot(tot.prod) +
geom_line(aes(x=year, y=year_prod, color=prediction)) +
#geom_ribbon(aes(x=year, ymin=year_prod-1.96*est_se, ymax=year_prod+1.96*est_se), alpha=.3) +
labs(y="Oil Production, Mill SM3", x="", color="2022 Oil Price, $10")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/tot_forecast.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(tot_forecast)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/tot_forecast_print.png", 
	width = 35, height = 21, units = "cm", res=300, pointsize=10)
print(tot_forecast)
dev.off()

#show oil price forecast numbers
years<-seq(from=2012, to=2022, by=1)
sc1<-seq(from =111, to=50 , length.out=11)
sc2<-seq(from =111, to=100 , length.out=11)
sc3<-seq(from =111, to=150 , length.out=11)
sc4<-seq(from =111, to=200 , length.out=11)
sc5<-seq(from =111, to=250 , length.out=11)
sc6<-seq(from =111, to=300 , length.out=11)

price_forecast<-data.frame(years, sc1, sc2, sc3, sc4, sc5, sc6)
price_forecast_long<-melt(price_forecast, id="years")

price_scenario<-ggplot(price_forecast_long, aes(x=years, y=value, color=variable)) +
geom_line() +
labs(x="", y="Oil Price", color="Scenario")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/price_scenario.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(price_scenario)
dev.off()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/figures/price_scenario_print.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(price_scenario)
dev.off()




#get uncertainty based on "prediction matrix"
#n_nat=Xp*Beta_hat ->
Xp_gam_under<-predict(prod_gam_under, type="lpmatrix")
V_beta<-prod_gam_under$Vp

Var_fitted<-Xp_gam_under%*%V_beta%*%t(Xp_gam_under)




