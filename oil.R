#descriptive analysis of oil data


library(ggplot2)
library(plyr)
library(reshape2)
library(ggmap)
library(rgdal)
library(sp)
library(maptools)
library(maps)
library(mapdata) #worldHiRes
library(mapproj)
library(rgeos)
library(gdata) #for reading xls
library(lubridate) #for date manipulation
library(Quandl)
library(mgcv)

#run fresh data import and clean:
#source("/Users/johannesmauritzen/Google Drive/github/rOil/oil_clean.r") 

#

#import last saved file
fields<-read.csv("/Users/johannesmauritzen/Google Drive/github/rOil/oil_fields.csv")

#Analysis*************************************************************************************
#fields<-read.csv("/Users/johannesmauritzen/Google Drive/Oil/cleanedData/oil_fields.csv")

#plot geographic production
#create sums for oil production and investment
fields<-ddply(fields, .(name), mutate, total.oil=sum(year_prod, na.rm=TRUE))
fields<-ddply(fields, .(name), mutate, total.invest=sum(investmentMillNOK, na.rm=TRUE))


#limit to one entry per field
tot.prod.fields<-fields[!duplicated(fields$name),] 

#maps of total production in North and Norwegian Sea
northsea<-get_map(location = c(lon = 2.74, lat = 59.00), zoom=6)
ggmap(northsea) +
geom_point(aes(x = lon, y = lat, size=tot.prod, color=init_year),alpha=.7, data = tot.prod.fields)+
scale_color_continuous(low="red", high="black")

norwegiansea<-get_map(location = c(lon = 7, lat = 65.00), zoom=6)
ggmap(norwegiansea) +
geom_point(aes(x = lon, y = lat, size=tot.prod, color=init_year),alpha=.7, data = tot.prod.fields)+
scale_color_continuous(low="red", high="black")

barentsea<-get_map(location = c(lon = 17, lat = 73.00), zoom=5)
ggmap(barentsea)

#create label columns - to label two biggest fields
tot.prod.fields$labels<-rep(NA, length(tot.prod.fields[,1]))
tot.prod.fields$labels[tot.prod.fields$name=="EKOFISK"]<-tot.prod.fields$name[tot.prod.fields$name=="EKOFISK"]
tot.prod.fields$labels[tot.prod.fields$name=="STATFJORD"]<-tot.prod.fields$name[tot.prod.fields$name=="STATFJORD"]

#add data for Johan Sverdrup
#what is the size of Johan Sverdrup:
#1,700-3,300 million barrels
#1sm3=6.29 barrels of oil
#standard cubic meter
#Temperature: 15 °C, Pressure: 1.01325 barA
#npd gives 300 SM3 recoverable oil 
sverdrup<-data.frame(name="JOHAN SVERDRUP", recoverable_oil=300, lon=2.36, lat=58.49, labels="SVERDRUP", init_year=2010)
tot.prod.fields_sverd<-merge(tot.prod.fields, sverdrup, 
		by=c("name","lon", "lat", "recoverable_oil", "labels","init_year"), all=TRUE)

#Map Reserves
north_sea_reserves<-ggmap(northsea) +
geom_point(aes(x = lon, y = lat, size=recoverable_oil, color=init_year),alpha=.7, data = tot.prod.fields_sverd)+
geom_text(aes(label=labels), data=tot.prod.fields_sverd, size=3) +
scale_color_continuous(low="red", high="black") +
labs(color="Initial Production Year", size="Total Recoverable Oil, Mill SM3")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/north_sea_reserves.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(north_sea_reserves)
dev.off()	

reserves_norwegian_sea<-ggmap(norwegiansea) +
geom_point(aes(x = lon, y = lat, size=recoverable_oil, color=init_year),alpha=.7, data = tot.prod.fields)+
#geom_text(aes(label=labels), data=tot.prod.fields, size=3) +
scale_color_continuous(low="red", high="black") +
labs(color="Initial Production Year", size="Total Recoverable Oil, Mill SM3")

#print(reserves_norwegian_sea)
png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/norwegian_sea_reserves.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(reserves_norwegian_sea)
dev.off()	

reserves_barent_sea<-ggmap(barentsea) +
geom_point(aes(x = lon, y = lat, size=recoverable_oil, color=init_year),alpha=.7, data = tot.prod.fields)+
#geom_text(aes(label=labels), data=tot.prod.fields, size=3) +
scale_color_continuous(low="red", high="black") +
labs(color="Initial Production Year", size="Total Recoverable Oil, Mill SM3")


#Show production of several wells on same line
#find largest fields
top10<-as.character(head(tot.prod.fields$name[order(tot.prod.fields$recoverable_oil, decreasing=TRUE)],10))

#function to to show field-level of data
fields_lim<-subset(fields, name %in% top10)
fields_lim<-fields_lim[,c("name", "year", "year_prod")]
fields_long<-melt(fields_lim, id.vars=c("name", "year", "year_prod"))

top10_production<-ggplot(fields_long, aes(x=year, y=year_prod, color=name)) +
geom_line() +
scale_color_grey() +
labs(x="", y="Yearly Oil Production, Mill SM3", color="Field")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/top10_production.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(top10_production)
dev.off()	


#Production Fall *************************************************************************



#create multiple lines for production fall
production2<-fields[order(fields$year),c("year", "year_prod","init_year", "oil_price_real")]

#first, get all production from fields that have produced at or before 1980 (but not those that started after)
#first, tot. yearly production
production_tot<-ddply(production2,.(year), mutate, tot_year_prod=sum(year_prod, na.rm=TRUE))
production_tot<-production_tot[!duplicated(production_tot$year),]
#exclude top year
production_tot<-production_tot[!production_tot$year==2013,]

#plot total production with oil price
oil_decline<-ggplot(production_tot) +
geom_line(aes(x=year, y=tot_year_prod, color=oil_price_real)) +
xlab("") + ylab("Norwegian Production of Oil, Mill SM3") +
labs(color="Oil Price, 2010 Prices", title="The Decline of Norwegian Oil") +
scale_color_continuous(low="blue", high="red")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/oil_decline.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(oil_decline)
dev.off()

#direct total - check
#production2_tot<-ddply(tot.month.prod, .(year), summarize, tot_year_prod=sum(oil_prod, na.rm=TRUE))

#test, production for active pre-1980 fields

prod_sum<-function(year_prod,init_year, before_year){
	#test
	#year_prod<-production2$year_prod[production2$year==2000]
	#init_year<-production2$init_year[production2$year==2000]
	#before_year=1990
	#test	
	year_prod<-year_prod[init_year<=before_year]
	tot_exist<-sum(year_prod, na.rm=TRUE)
	return(tot_exist)		
}


#production_1980<-ddply(production2,.(year), summarize, pre2000=prod_sum(year_prod, init_year, 1980))

years<-seq(1970,2010, by=5)

production.lim<-matrix(NA, ncol=length(years), nrow=length(unique(production2$year)))
production.lim<-data.frame(production.lim)

c=1
for(i_year in years){
	#test
	#c=1
	#i_year<-1995
	#test
	#print(c)
	prod.lim<-ddply(production2,.(year), summarize, lim.prod=prod_sum(year_prod, init_year, before_year=i_year))
	production.lim[,c]<-prod.lim[,2]
	c<-c+1
}

col.names<-as.character(years)
names(production.lim)<-col.names
production.lim<-cbind(unique(production2$year), production.lim, production_tot[,2])
names(production.lim)[1]<-"years"
names(production.lim)[length(production.lim)]<-"total"
production.lim<-production.lim[-length(production.lim[,1]),] #get rid of 2013 (full year effect)

production.lim.long<-melt(production.lim, id="years")
names(production.lim.long)[3]<-"static_production"
#production.lim.long$variable<-as.numeric(as.character(production.lim.long$variable))

tot_exist_prod_cf<-ggplot(production.lim.long) +
geom_line(aes(x=years, y=static_production, color=variable))+
labs(x="", y="Production from Existing Fields, Mill Sm3", color="Fields Producing From:") +
scale_color_discrete()

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/tot_exist_prod_cf.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(tot_exist_prod_cf)
dev.off()

#Demonstrate use of time with just one 
statfjord<-fields[fields$name=="STATFJORD",]

statfjord_dem<-ggplot(statfjord, aes(x=year, y=year_prod)) +
geom_point() +
geom_vline(xintercept=1992, linetype="dashed", color="gray") +
geom_segment(aes(x=1979, y=0, xend=1992, yend=0), arrow=arrow(length=unit(.2, "cm"))) +
geom_segment(aes(x=1992, y=0, xend=2013, yend=0), arrow=arrow(length=unit(.2, "cm"))) +
geom_text(aes(label="Time to peak", x=1985, y=1 )) +
geom_text(aes(label="Time from peak", x=2000,y=1)) +
labs(x="", y="Yearly Production from Statfjord Field, Mill SM3") 

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/statfjord_dem.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(statfjord_dem)
dev.off()


#***************** Look at investments**************************
tot_investments_real<-ddply(fields, .(year),summarize, tot_invest_real=sum(investmentMillNOK_real, na.rm=TRUE))
tot_investments_real<-tot_investments_real[1:(length(tot_investments_real[,1])-2),]

#plot
tot_investments_real<-merge(tot_investments_real, oil_long, all.x=TRUE)

invest_with_oil_price<-ggplot(tot_investments_real) +
geom_line(aes(x=year, y=tot_invest_real, color=oil_price_real)) +
xlab("") + ylab("Investments, MILL NOK, 2010 Prices") +
labs(color="Oil Price, 2010 Prices") +
scale_color_continuous(low="blue", high="red")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/invest_with_oil_price.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(invest_with_oil_price)
dev.off()

#with oil prices
tot_investments_real$oil_price_nom<-NULL
invest.melt<-melt(tot_investments_real, id="year")
invest.melt$variable<-factor(invest.melt$variable, labels=c("Total Investment, Mill 2010 NOK", 
		"Oil Price, 2010 USD/Barrell", "Oil Price, l1", "Oil Price, l2", "Oil Price, l3", "Oil Price, l4",
		"Oil Price, l5", "Oil Price, L6"))



ggplot(invest.melt) +
geom_line(aes(x=year, y=value)) +
facet_grid(variable~., scales="free_y") +
ylab("") + xlab("")

#Production per investment kroner
#MillSm3

tot_investments_real<-merge(tot_investments_real, production_tot, all.x=TRUE)
tot_investments_real$nok_per_sm3<-with(tot_investments_real, tot_invest_real/tot_year_prod)
tot_investments_real<-tot_investments_real[-1,]

invest_per_prod<-ggplot(tot_investments_real) +
geom_line(aes(x=year, y=nok_per_sm3, color=oil_price_real)) +
xlab("") + ylab("NOK Investment per sm3 of Oil Production") +
labs(color="Oil Price, 2010 Prices") +
scale_color_continuous(low="blue", high="red")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/invest_per_prod.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(invest_per_prod)
dev.off()

#lag, five years
tot_investments_real$lag5_prod<-NA
tot_investments_real$lag5_prod[1:37]<-tot_investments_real$tot_year_prod[5:41]
tot_investments_real$nok_per_sm3_lag5<-with(tot_investments_real, tot_invest_real/lag5_prod)

invest_per_prod_l5<-ggplot(tot_investments_real) +
geom_line(aes(x=year, y=nok_per_sm3_lag5, color=oil_price_real)) +
xlab("") + ylab("NOK Investment per sm3 of Oil Production, 5 year lag") +
labs(color="Oil Price, 2010 Prices") +
scale_color_continuous(low="blue", high="red")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/invest_per_prod_l5.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(invest_per_prod_l5)
dev.off()

#Time from discovery to production *********************
field_unique<-fields[!duplicated(fields$name),] 
field_unique$approved_to_producing<-with(field_unique, producing_from-approved_at)
field_unique$approved_to_producing<-as.numeric(field_unique$approved_to_producing)

ggplot(field_unique,aes(x=producing_from, y=approved_to_producing)) +
geom_point() +
stat_smooth(method="lm") +
xlab("") + ylab("Time between approval and production, days")



#Reserves over time and investment*************************

size_vs_init_prod<-ggplot(field_unique) +
geom_point(aes(x=producing_from, y=recoverable_oil, size=total.invest, color=oil_price_real)) +
geom_point(aes(x=as.Date("2017-01-01"), y=recoverable_oil), data=sverdrup, shape=4, color="red") +
geom_text(aes(x=as.Date("2017-01-01"), y=recoverable_oil-10, label="Sverdrup"), data=sverdrup) +
xlab("First Production year") + ylab("Total Recoverable Oil, Mill Sm3") + 
labs(color="Oil Price, 2010 Prices", size="Total Investment, Mill NOK:") +
scale_color_continuous(low="blue", high="red")


png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/size_vs_init_prod.png", width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(size_vs_init_prod)
dev.off()

#investment over time by year and reserves?  IE question is where is current investment going?

#chart investment x time x reserve size

#average age of producing oil fields over time*********************************

#first create variable giving age since first year of production
fields$first_prod_year<-year(fields$producing_from)
fields$age<-with(fields, year-first_prod_year)

#now average by age
field_age<-ddply(fields, .(year), summarize, average_age=mean(age, na.rm=TRUE))
ggplot(field_age, aes(x=year, y=average_age)) +
geom_line() +
labs(x="", y="Average age of Producing Wells")

#Size of reserves found over time
field_unique<-fields[!duplicated(fields$name),] 
found_reserves_year<-ddply(field_unique,.(init_year), summarize, found_reserves=sum(recoverable_oil, na.rm=TRUE))

ggplot(found_reserves_year, aes(x=init_year, y=found_reserves))+
geom_histogram(stat="identity") +
labs(x="", y="Discovered Recoverable Oil")

#individual reserves
#try statfjord
statfjord<-fields[fields$name=="STATFJORD",]
ggplot(statfjord) +
geom_point(aes(x=year, y=year_prod, size=investmentMillNOK))

statfjord_plot<-ggplot(statfjord) +
geom_point(aes(x=year, y=year_prod, size=investmentMillNOK*(1/deflator), color=oil_price_real)) +
scale_color_continuous(low="black", high="red") +
labs(x="", y="Yearly Oil Production, Mill SM3", 
	title="Statfjord Production", size="Investment, Mill NOK, 2010 Prices", Color="Oil Price, 2010 Prices")
print(statfjord_plot)

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/statfjord_plot.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(statfjord_plot)
dev.off()

gam_statfjord<-gam(year_prod~s(year), data=statfjord)
statfjord<-statfjord[!is.na(statfjord$year_prod),]
statfjord$smooth<-gam_statfjord$fitted.values

statfjord_gam<-ggplot(statfjord) +
geom_point(aes(x=year, y=year_prod)) +
geom_line(aes(x=year, y=smooth)) +
labs(x="", y="Statfjord Yearly Oil Production, Mill SM3")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/statfjord_gam.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(statfjord_gam)
dev.off()

gam2_statfjord<-gam(year_prod~s(year) + s(investmentMillNOK*(1/deflator)), data=statfjord)
statfjord$smooth2<-gam2_statfjord$fitted.values

field_plot %+% statfjord +
geom_line(aes(x=year, y=smooth)) +
geom_line(aes(x=year, y=smooth2), color="blue")

#what is the relationship between investment production

ggplot(statfjord) +
geom_point(aes(x=year, y=year_prod, size=investmentMillNOK)) +
geom_line(aes(x=year, y=smooth))
 

#Ekofisk
ekofisk<-fields[fields$name=="EKOFISK",]
ekofisk<-ekofisk[!is.na(ekofisk$year_prod),]
ekofisk<-ekofisk[!is.na(ekofisk$year_prod),]


gam_ekofisk<-gam(year_prod~s(year), data=ekofisk)
gam2_ekofisk<-gam(year_prod~s(year) + investmentMillNOK*(1/deflator), data=ekofisk)
gam3_ekofisk<-gam(year_prod~s(year) + oil_price_real, data=ekofisk)


ekofisk$smooth<-gam_ekofisk$fitted.values
ekofisk$smooth2<-gam2_ekofisk$fitted.values
ekofisk$smooth3<-gam3_ekofisk$fitted.values


ekofisk_plot<-statfjord_plot %+% ekofisk +
geom_line(aes(x=year, y=smooth)) +
labs(title="Ekofisk Production", color="Oil Price, 2010 Prices")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/ekofisk_plot.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(ekofisk_plot)
dev.off()

ekofisk_plot +
geom_line(aes(x=year, y=smooth2), color="blue") +
geom_line(aes(x=year, y=smooth3), color="red")

#Survival time of differing fields****************************************



#first restrict to producing fields
fields_p$producing_from<-as.Date(fields_p$producing_from)
fields_p$producing_to<-as.Date(fields_p$producing_to)

fields_p<-fields[fields$tot.prod>0,]


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

fields_p<-ddply(fields_p, .(name), mutate, field_life_dead=producing_to - producing_from)

fields_p<-ddply(fields_p, .(name), mutate, field_life_alive=as.Date("2012-09-01")-producing_from)
fields_p$field_life_alive[!is.na(fields_p$producing_to)]<-NA
fields_p$field_life<-pmax(fields_p$field_life_dead, fields_p$field_life_alive, na.rm=TRUE)

field_unique<-fields_p[!duplicated(fields_p$name),] 
field_unique$yes_dead<-0
field_unique$yes_dead[!is.na(field_unique$producing_to)]<-1
field_unique$yes_dead<-factor(field_unique$yes_dead, labels=c("no", "yes"))

field_unique$field_life<-as.numeric(field_unique$field_life)

field_life<-ggplot(field_unique) +
geom_point(aes(x=recoverable_oil, y=field_life, color=yes_dead)) +
scale_color_manual(values=c("black", "red")) +
labs(x="Estimated Recoverable Oil", y="Field Life, Days", color="Field is Shut Down")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/field_life.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(field_life)
dev.off()

#create variable - time to peak
field_unique$time_to_peak<-with(field_unique, as.Date(paste(peak_year, "01-01", sep="-"))-producing_from)
field_unique$time_to_peak<-as.numeric(field_unique$time_to_peak)
field_unique$peak_2013<-0; field_unique$peak_2013[field_unique$peak_year==2013]<-1
field_unique$peak_2013<-factor(field_unique$peak_2013, labels=c("no", "yes"))

time_to_peak_plot<-ggplot(field_unique, aes(x=recoverable_oil, y=time_to_peak, color=peak_2013)) +
geom_point() +
scale_color_manual(values=c("black", "red")) +
stat_smooth(method="lm") +
labs(x="Estimated Recoverable Oil", y="Time to peak, Days", color="2013 is Peak Year (censored)")

png("/Users/johannesmauritzen/Google Drive/github/rOil/presentations/time_to_peak_plot.png", 
	width = 27.81, height = 21, units = "cm", res=300, pointsize=10)
print(time_to_peak_plot)
dev.off()

#differences between large and small

#split into two groups - large and small over and under
#first look at sizes

prod_plot<-ggplot(fields_unique, aes(max_prod)) +
geom_histogram(binwidth=.5)

prod_plot

prod_plot +
geom_histogram(aes(log(max_prod)))

prod_plot +
geom_point(aes(y=field_time_to_peak, x=max_prod)) +
labs(x="Max Yearly Production of Field", y="Time to peak of Field") +
stat_smooth(aes(y=field_time_to_peak, x=max_prod), method=lm)

#relationship between oil price and investments
price_invest<-fields_p[c(12:18, 26)]
price_invest_long<-melt(price_invest, id.vars="investmentMillNOK_real")

ggplot(price_invest_long) +
geom_point(aes(x=value, y=investmentMillNOK_real, color=variable), alpha=.3)

ggplot(price_invest) +
geom_point(aes(x=oil_price_real, y=investmentMillNOK_real))


#reserves to production ratio (how many years left) Have to work backward on this:


#what is the size of Johan Sverdrup:
#1,700-3,300 million barrels
#1sm3=6.29 barrels of oil
#standard cubic meter
#Temperature: 15 °C, Pressure: 1.01325 barA 






