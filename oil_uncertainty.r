#oil_uncertainty.r

#explore getting variances and predictions from GAMs - what is the best?  Boot- strap?
#see p.239 (252) of Generalized Additive Models


#Simple GLM model uncertainty:
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




#instruction for 
#https://stat.ethz.ch/pipermail/r-help/2011-April/275632.html

 #> Is it possible to estimate prediction interval using GAM?  I looked > 
#through ?gam,

#- The easiest and most general way is by posterior simulation. Here's an 
#example...

## Prediction interval example for Gamma GAM

library(mgcv)

## simulate some data...
f <- function(x) (0.2 * x^11 * (10 * (1 - x))^6 + 10 *
             (10 * x)^3 * (1 - x)^10)/2
x <- runif(200)
fx <- f(x)
Ey <- exp(fx);scale <- .5 ## mean and GLM scale parameter
## Note that `shape' and `scale' in `rgamma' are almost
## opposite terminology to that used with GLM/GAM...
set.seed(8)
y <- rgamma(Ey*0,shape=1/scale,scale=Ey*scale)

## fit smooth model to x, y data...
b <- gam(y~s(x,k=20),family=Gamma(link=log),method="REML")

## extract parameter estiamtes and cov matrix...
beta <- coef(b);Vb <- vcov(b)

## simulate replicate beta vectors from posterior...
Cv <- chol(Vb)
n.rep=10000;nb <- length(beta)
br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta

## turn these into replicate linear predictors...
xp <- 0:200/200
Xp <- predict(b,newdata=data.frame(x=xp),type="lpmatrix")
lp <- Xp%*%br
fv <- exp(lp) ## ... finally, replicate expected value vectors

## now simulate from Gamma deviates with mean as in fv
## and estimated scale...

yr <- matrix(rgamma(fv*0,shape=1/b$scale,scale=fv*scale),nrow(fv),ncol(fv))

plot(rep(xp,n.rep),yr,pch=".") ## plotting replicates
points(x,y,pch=19,cex=.5) ## and original data

## compute 95% prediction interval...
PI <- apply(yr,1,quantile,prob=c(.025,0.975))
## and plot it...
lines(xp,PI[1,],col=2,lwd=2);lines(xp,PI[2,],col=2,lwd=2)

## Confidence interval for comparison...
pred <- predict(b,newdata=data.frame(x=xp),se=TRUE)
lines(xp,exp(pred$fit),col=3,lwd=2)
u.ci <- exp(pred$fit + 2*pred$se.fit)
l.ci <- exp(pred$fit - 2*pred$se.fit)
lines(xp,u.ci,col=3,lwd=2);lines(xp,l.ci,col=3,lwd=2)


