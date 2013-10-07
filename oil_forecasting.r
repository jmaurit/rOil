#Oil Forecasting model


#first run bottom up modeling
source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_modeling.r") 

#first, without using price information
#put together Smoothed results compare with total. 
fitted_year_prod<-ddply(fields_p, .(year), summarize, fitted_year_prod=sum(smooth_bench_split))
actual_year_prod<-ddply(fields_p, .(year), summarize, actual_year_prod=sum(year_prod))
year_prod<-merge(fitted_year_prod, actual_year_prod)

ggplot(year_prod) +
geom_line(aes(x=year, y=actual_year_prod)) +
geom_line(aes(x=year, y=fitted_year_prod), color="red")

#create forecast
#use predict.gam()

prod_predictions<-function(field_data, model_under, model_over=NA)
{
	#test
	field_data<-fields_p[fields_p$name=="TROLL",]
	model_under<-gam_under
	model_over<-gam_over
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
			print("using over")
		ifelse(field_data$max_prod<=split,
			pred_prod<-predict.gam(model_over, type="response", newdata=new.data),
			pred_prod<-predict.gam(model_under, type="response", newdata=new.data))

		}	

	if(is.na(model_over))
		#if model_over parameter is not provided - ie model not split into two
		{
			print("using under")
			pred_prod<-predict.gam(model_under, type="resonse", newdata=new.data)
		}

	pred.data<-data.frame(name=rep(field_name,pred.years), year=year, 
	year_prod=pred_prod, prediction=rep("prediction", pred.years),
	time_to_peak=time_to_peak, peak_to_end=peak_to_end, recoverable_oil=recoverable_oil)

	#test
	troll_pred<-merge(fields_p[fields_p$name=="TROLL",], pred.data, 
		by=c("year","time_to_peak", "peak_to_end", "name","year_prod", "recoverable_oil"), all=TRUE)
	
	ggplot(troll_pred) +
	geom_line(aes(x=year, y=year_prod, color=prediction))
	#test



	return(new.data)
	}


prod_predictions<-ddply(fields_p, .(name), prod_predictions, 
	model_under=prod_gam_under, model_over=prod_gam_over)
fields_pred<-merge(fields_p, prod_predictions, by=c("year","year_prod", "name"), all=TRUE)

fields_pred$prediction<-as.character(fields_pred$prediction)

fields_pred$prediction[is.na(fields_pred$prediction)]<-"actual"

ggplot(fields_pred[fields_pred$name=="STATFJORD",]) +
	geom_line(aes(x=year, y=year_prod, color=prediction))

fields_pred$year_prod<-as.numeric(fields_pred$year_prod)

sum_years<-function(year_data) sum(year_data$year_prod, na.rm=TRUE)
tot.prod<-ddply(fields_pred[c("year_prod","year")], .(year), sum_years)
names(tot.prod)[2]<-"year_prod"

tot.prod$year<-as.numeric(tot.prod$year)

ggplot(tot.prod, aes(x=year, y=year_prod)) +
geom_line()


ggplot2(ekofisk_pred, aes()) + 