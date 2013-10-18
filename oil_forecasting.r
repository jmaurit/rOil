#Oil Forecasting model


#first run bottom up modeling
#source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_modeling.r") 

#first, without using price information
#put together Smoothed results compare with total. 
fitted_year_prod<-ddply(fields_p, .(year), summarize, fitted_year_prod=sum(smooth_bench_split))
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

prod_predictions<-function(field_data, model_under, model_over=NA)
{
	#test
	#field_data<-fields_p[fields_p$name=="TROLL",]
	#model_under<-gam_under
	#model_over<-gam_over
	#test

	pred.years<-15
	field_name<-field_data$name[1]
	max_peak_to_end<-max(field_data$peak_to_end)
	max_year<-max(field_data$year)

	year<-(max_year+1):(max_year+pred.years)
	recoverable_oil<-rep(unique(field_data$recoverable_oil),pred.years)
	time_to_peak<-rep(0, pred.years)  #assumes all fields have peaked!!!
	peak_to_end<-as.numeric((max_peak_to_end+1):(max_peak_to_end+pred.years))

	new.data<-data.frame(year, recoverable_oil, time_to_peak, peak_to_end)
	
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
			pred_prod<-predict.gam(model_under, type="resonse", newdata=new.data, se.fit=TRUE)
		}

	pred.data<-data.frame(name=rep(field_name,pred.years), year=year, 
	year_prod=pred_prod$fit, est_se=pred_prod$se, prediction=rep("prediction", pred.years))

	

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

predictions<-ddply(fields_active, .(name), prod_predictions, 
	model_under=prod_gam_under, model_over=prod_gam_over)

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
geom_line(aes(x=year, y=year_prod, color=prediction, group=name)) +
geom_ribbon(aes(x=year, ymin=year_prod-1.96*est_se, ymax=year_prod+1.96*est_se), alpha=.3) +
labs(y="Oil Production, Mill SM3")

png("/Users/johannesmauritzen/Google Drive/oil/figures/field_lev_forecast.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(field_lev_forecast)
dev.off()


#sum together an dplot
sum_years<-function(year_data) c(sum(year_data$year_prod, na.rm=TRUE),sum(year_data$est_se, na.rm=TRUE))

tot.prod<-ddply(fields_pred, .(year), sum_years)
names(tot.prod)[2]<-"year_prod"
names(tot.prod)[3]<-"est_se"

tot.prod$prediction<-factor(ifelse(tot.prod$year<2013,"actual","prediction"), labels=c("actual", "prediction"))

tot.prod$year<-as.numeric(tot.prod$year)
tot.prod$tot<-"tot"
tot.prod$est_se[tot.prod$year==2012]<-0

tot_forecast<-ggplot(tot.prod) +
geom_line(aes(x=year, y=year_prod, color=prediction, group="tot")) +
geom_ribbon(aes(x=year, ymin=year_prod-1.96*est_se, ymax=year_prod+1.96*est_se), alpha=.3) +
labs(y="Oil Production, Mill SM3")

png("/Users/johannesmauritzen/Google Drive/oil/figures/tot_forecast.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(tot_forecast)
dev.off()


#get uncertainty based on "prediction matrix"
#n_nat=Xp*Beta_hat ->
Xp_gam_under<-predict(prod_gam_under, type="lpmatrix")
V_beta<-prod_gam_under$Vp

Var_fitted<-Xp_gam_under%*%V_beta%*%t(Xp_gam_under)




