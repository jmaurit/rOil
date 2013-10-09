#oil_bootstram
library(boot)
library(arm)
#with model
#prod_gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) +  s(year), 
#	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])
#prod_gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(year), 
#	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

prod_gam_over_bootstrap <- function(field_data){
prod_gam_over<-gam(year_prod~s(time_to_peak, recoverable_oil) + s(year), 
	family=Gamma(link=log), weights=recoverable_oil, data=field_data[field_data$max_prod>split,])
	return(prod_gam_over$fitted.values)
}

prod_gam_under_bootstrap <- function(field_data){
prod_gam_under<-gam(year_prod~s(time_to_peak, recoverable_oil) +  s(year), 
	family=Gamma(link=log), weights=recoverable_oil, data=field_data[field_data$max_prod<=split,])
	return(prod_gam_under$fitted.values)

}
#With the function fc defined, we can use the boot command, 
#providing our dataset name, our function, and the number of bootstrap samples to be drawn.

prod_gam_over_boot <- boot(fields_p, prod_gam_over_bootstrap , R=10)



#better idea - generate one forecast point at a time
# for each point where time to peak, peak to end, and recoverable_oil are approximate
#

