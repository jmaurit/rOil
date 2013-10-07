#create data set with split ekofisk

ekofisk<-fields[fields$name=="EKOFISK",]

ggplot(ekofisk) +
geom_point(aes(x=year, y=year_prod, size=investmentMillNOK_real, color=oil_price_real))

fields_eko<-fields

fields_eko$name<-as.character(fields_eko$name)
fields_eko$name[which(fields_eko$name=="EKOFISK" & fields_eko$year<1988)]<-"EKOFISK_T1"
fields_eko$name[which(fields_eko$name=="EKOFISK" & fields_eko$year>=1988)]<-"EKOFISK_T2"


fields_eko<-fields_eko[fields_eko$tot.prod>0,]

#make sure in date format 
fields_eko$producing_from<-as.Date(fields_eko$producing_from)
fields_eko$producing_to<-as.Date(fields_eko$producing_to)

#create variable for each field that gives the top year
max_year<-function(year_prod, year)
{
	#test
	#field<-fields_eko[fields_eko$name=="STATFJORD",]
	#year_prod<-field$year_prod
	#year<-field$year
	#test
	peak_prod<-max(year_prod, na.rm=TRUE)
	peak_year<-year[year_prod==peak_prod]
	peak_year<-peak_year[!is.na(peak_year)]
	return(peak_year)
}

fields_eko<-ddply(fields_eko, .(name), mutate, peak_year=max_year(year_prod, year))



#create variable, production time
fields_eko<-ddply(fields_eko, .(name), mutate, producing_time=year-year(producing_from))

#put in peak production
fields_eko<-ddply(fields_eko, .(name), mutate, max_prod=max(year_prod, na.rm=TRUE))


fields_eko<-fields_eko[!is.na(fields_eko$year_prod), ]
fields_eko<-fields_eko[!is.na(fields_eko$producing_time), ]

#create time to peak variable and time from peak
	#the idea being that affect is different.  
fields_eko<-ddply(fields_eko, .(name), mutate, field_time_to_peak=as.Date(paste(peak_year, "01-01", 
	sep="-"))-producing_from)
fields_eko$field_time_to_peak<-as.numeric(fields_eko$field_time_to_peak)

fields_eko<-ddply(fields_eko, .(name), mutate, time_to_peak=peak_year-year)
fields_eko$time_to_peak<-as.numeric(fields_eko$time_to_peak)



