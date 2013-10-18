#oil_modeling_prep

#under oil_modeling.R

#modeling with price
fields<-read.csv("/Users/johannesmauritzen/Google Drive/github/rOil/oil_fields.csv")

#make oil denominated in 10 $ units
fields$oil_price_real<-fields$oil_price_real/10
fields$oil_price_real_l1<-fields$oil_price_real_l1/10
fields$oil_price_real_l2<-fields$oil_price_real_l2/10
fields$oil_price_real_l3<-fields$oil_price_real_l3/10
fields$oil_price_real_l4<-fields$oil_price_real_l4/10
fields$oil_price_real_l5<-fields$oil_price_real_l5/10
fields$oil_price_real_l6<-fields$oil_price_real_l6/10

#weed out fields where oil is not produced
fields_p<-fields[fields$tot.prod>0,]
fields_p$name<-as.character(fields_p$name)

#make sure in date format 
fields_p$producing_from<-as.Date(fields_p$producing_from)
fields_p$producing_to<-as.Date(fields_p$producing_to)

#create variable for each field that gives the top year
max_year<-function(year_prod, year)
{
	#test
	#field<-fields_p[fields_p$name=="STATFJORD",]
	#year_prod<-field$year_prod
	#year<-field$year
	#test
	peak_prod<-max(year_prod, na.rm=TRUE)
	peak_year<-year[year_prod==peak_prod]
	peak_year<-peak_year[!is.na(peak_year)]
	return(peak_year)
}

fields_p<-ddply(fields_p, .(name), mutate, peak_year=max_year(year_prod, year))



#create variable, production time
fields_p<-ddply(fields_p, .(name), mutate, producing_time=year-year(producing_from))

#put in peak production
fields_p<-ddply(fields_p, .(name), mutate, max_prod=max(year_prod, na.rm=TRUE))


fields_p<-fields_p[!is.na(fields_p$year_prod), ]
fields_p<-fields_p[!is.na(fields_p$producing_time), ]

#create time to peak variable and time from peak
	#the idea being that affect is different.  
fields_p<-ddply(fields_p, .(name), mutate, field_time_to_peak=as.Date(paste(peak_year, "01-01", 
	sep="-"))-producing_from)
fields_p$field_time_to_peak<-as.numeric(fields_p$field_time_to_peak)

fields_p<-ddply(fields_p, .(name), mutate, time_to_peak=peak_year-year)
fields_p$time_to_peak<-as.numeric(fields_p$time_to_peak)

fields_p<-fields_p[!fields_p$year_prod==0,]

#split into two variables - time_to_peak and peak_to_end
fields_p$peak_to_end<-0
fields_p$peak_to_end[fields_p$time_to_peak<=0]<-abs(fields_p$time_to_peak[fields_p$time_to_peak<=0])+1

fields_p$time_to_peak[fields_p$time_to_peak<0]<-0

#split ekofisk into two?
#returns an alternative data set, fields_eko, which has ekofisk split into two
#source("/Users/johannesmauritzen/Google Drive/Oil/rOil/oil_modeling_eko.r")

#get rid of all 2013 data (only want full year)
fields_p<-fields_p[!fields_p$year==2013,]
