#temp file for oil work

library(ggplot2)
library(plyr)
library(reshape2)
library(mgcv)
library(lubridate) 

#modeling with price
fields<-read.csv("/Users/johannesmauritzen/Google Drive/Oil/cleanedData/oil_fields.csv")

#This should be done in the cleaning file
fields$producing_from<-as.Date(as.character(fields$producing_from), format="%Y-%m-%d")
fields$producing_to<-as.Date(as.character(fields$producing_to), format="%Y-%m-%d")

#create variable for production time
ddply(fields, .name, mutate, prod_time=)

gam(year_prod, data=fields)


