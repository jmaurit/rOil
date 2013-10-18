#Price part of modeling
#price_modeling.R
# under oil_modeling.R

#Full set
gam_price<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil)  + 
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p)

gam_price_unweighted<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil)  + 
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), data=fields_p)

gam_price2<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil)  + 
	s(oil_price_real_l4) + s(oil_price_real_l5) + s(oil_price_real_l6),
	family=gaussian(link=log), weights=recoverable_oil,data=fields_p)


#Have to be careful about price and effect of year dummies - remove affect of year

#under group
gam_price_under_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])
summary(gam_price_under_2d)


gam_price_under<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_price_under_gamma<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])



#over group

#preferred model:
gam_price_over_2d<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end, recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])
summary(gam_price_over_2d)

gam_price_over<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

gam_price_over_gamma<-gam(year_prod~s(time_to_peak)+ s(peak_to_end) + s(recoverable_oil) +
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])



gam_price_over_2d_s_price<-gam(year_prod~s(time_to_peak, recoverable_oil)+ s(peak_to_end) + recoverable_oil +
	s(oil_price_real),
	family=gaussian(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])
summary(gam_price_over_2d_s_price)

gam_price_over_2d_single<-gam(year_prod~s(I(-time_to_peak+peak_to_end), recoverable_oil) + 
	oil_price_real + oil_price_real_l2 + oil_price_real_l3 + oil_price_real_l4 + oil_price_real_l5 +oil_price_real_l6,
	family=Gamma(link=log),weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])


#ridge regression
#gam_ridge<-


fields_p$smooth_price[fields_p$max_prod<=split]<-gam_price_under$fitted.values
fields_p$smooth_price[fields_p$max_prod>split]<-gam_price_over$fitted.values
#fields_p$smooth_price_2[fields_p$max_prod<=split]<-gam_price_under2$fitted.values
#fields_p$smooth_price_2[fields_p$max_prod>split]<-gam_price_over2$fitted.values


#compare direct price/non price comparision
gam_under<-gam(year_prod~s(I(time_to_peak*recoverable_oil)) + s(I(peak_to_end*recoverable_oil)),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod<=split,])

gam_over<-gam(year_prod~s(I(time_to_peak*recoverable_oil)) + s(I(peak_to_end*recoverable_oil)),
	family=gaussian(link=log), weights=recoverable_oil, data=fields_p[fields_p$max_prod>split,])

fields_p$smooth_split[fields_p$max_prod<=split]<-gam_under$fitted.values
fields_p$smooth_split[fields_p$max_prod>split]<-gam_over$fitted.values

#test of coefficients on price


summary(gam_price)
summary(gam_price2)
summary(gam_price_unweighted)

summary_under <- summary(gam_price_under)
summary_under

summary_under_gamma<-summary(gam_price_under_gamma)
summary_under_gamma

summary_under_2d<-summary(gam_price_under_2d)
summary_under_2d


summary_over <- summary(gam_price_over)
summary_over

summary_over_gamma <- summary(gam_price_over_gamma)
summary_over_gamma

summary_over_2d_single <- summary(gam_price_over_2d_single)
summary_over_2d_single


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

#instructions from ARM
sim_gam_price<-sim(gam_price_over_2d, n.sims=1000)
price_coef_sim<-melt(sim_gam_price@coef[,c(8:13)],)
price_coef_sim$Var1<-NULL
names(price_coef_sim)<-c("coefficient", "estimate")

#from instructions

#gam_price_over
beta<-coef(gam_price_over_2d)
Vb<-vcov(gam_price_over_2d)

## simulate replicate beta vectors from posterior...
Cv <- chol(Vb)  #cholesky decomposition the equivalent of taking square root in a single variance to get SD?
n.rep=1000
nb <- length(beta)
br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta  

#chart replicate beta vectors
price_br<-t(br[c(2:7),])
price_br_long<-data.frame(melt(price_br))
names(price_br_long)<-c("id", "variable", "estimate")

gam_price_over_dirty_box<-ggplot(price_br_long, aes(x=variable, y=estimate)) +
geom_boxplot() +
geom_jitter(, alpha=.1)

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/gam_price_over_dirty_box.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_over_dirty_box)
dev.off()


#gam price under
beta<-coef(gam_price_under_2d)
Vb<-vcov(gam_price_under_2d)

## simulate replicate beta vectors from posterior...
Cv <- chol(Vb)  #cholesky decomposition the equivalent of taking square root in a single variance to get SD?
n.rep=1000
nb <- length(beta)
br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta  

#chart replicate beta vectors
price_br<-t(br[c(2:7),])
price_br_long<-data.frame(melt(price_br))
names(price_br_long)<-c("id", "variable", "estimate")

gam_price_under_dirty_box<-ggplot(price_br_long, aes(x=variable, y=estimate)) +
geom_boxplot() +
geom_jitter(, alpha=.1)

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/gam_price_under_dirty_box.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(gam_price_under_dirty_box)
dev.off()




