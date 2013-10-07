#oil_uncertainty.r

#explore getting variances and predictions from GAMs - what is the best?  Boot- strap?
#see p.239 (252) of Generalized Additive Models


#on terms of response variable (ie after log)
predict_under_res<-predict.gam(prod_gam_under, type="response", se.fit=TRUE)
predict_over_res<-predict.gam(prod_gam_over, type="response", se.fit=TRUE)

#fitted values
fields_p$res_fit[fields_p$max_prod<=split]<-predict_under_res$fit
fields_p$res_fit[fields_p$max_prod>split]<-predict_over_res$fit

#approximate s.e. using approximation
fields_p$res_fit_se[fields_p$max_prod<=split]<-predict_under_res$se
fields_p$res_fit_se[fields_p$max_prod>split]<-predict_over_res$se



fields_lim<-subset(fields_p, name %in% include_fields)

multi_gam_plot <- ggplot(fields_lim) +
geom_point(aes(x=year, y=year_prod)) +
facet_wrap(~name, scales="free")

multi_gam_plot + 
geom_line(aes(x=year, y=res_fit), color="red") +
geom_ribbon(aes(x=year, ymin=res_fit-1.96*res_fit_se, ymax=res_fit+1.96*res_fit_se), alpha=.3)
