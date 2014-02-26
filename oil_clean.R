#Oil clean

#use function install.packages("package.name") to install missing packages
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

#oil prices brent
brentprices<-read.xls("http://www.eia.gov/dnav/pet/hist_xls/RBRTEm.xls", sheet="Data 1", stringsAsFactors=FALSE)
names(brentprices)<-c("date_read", "brent_price")
brentprices<-brentprices[(-1:-3),]
brentprices$brent_price<-as.numeric(brentprices$brent_price)
brentprices$date_read<-tolower(brentprices$date_read)
brentprices$date_read<-paste(1,brentprices$date_read, sep="-")
brentprices$date<-as.Date(brentprices$date_read, format="%d-%b-%Y")
brentprices$date_read<-NULL

#from bp via Quandl
oil_long<-read.csv('http://www.quandl.com/api/v1/datasets/BRP/CRUDE_OIL_PRICES.csv?&trim_start=1861-01-01&trim_end=2010-01-01&sort_order=desc', colClasses=c('Year'='Date'))
names(oil_long)<-c("year", "oil_price_nom", "oil_price_real")
oil_long$year<-substr(oil_long$year,1,4)
oil_long$year<-as.numeric(oil_long$year)
#qplot(x=year, y=oil_price_real, geom="line", data=oil_long)

#merge with oil price data
#aggregate into years
brentprices$month<-months(brentprices$date)
brentprices$year<-year(brentprices$date) #from lubridate
annual_brentprices<-ddply(brentprices, .(year), summarize, annual_brentprice=mean(brent_price))

oil_plus<-annual_brentprices[25:27,]
oil_price_real<-oil_plus[,2]
oil_plus<-cbind(oil_plus,oil_price_real)
oil_plus<-rename(oil_plus, c("annual_brentprice"="oil_price_nom"))
oil_long<-rbind(oil_long, oil_plus)
oil_long<-oil_long[order(oil_long$year),]

write.csv(oil_long, "/Users/johannesmauritzen/Google Drive/github/rOil/brent_price.csv")


#investment deflator from quandl
#import deflator
#The Fixed Investment implicit deflator is the ratio of Fixed Investment in current local currency to Fixed Investment in constant local currency. 
#The base year varies by country. Fixed Investment deflator, LCU
#2010 base year
investment_deflator<-read.csv('http://www.quandl.com/api/v1/datasets/WORLDBANK/NOR_NEGDIFTOTXN.csv?&trim_start=1960-12-31&trim_end=2015-12-31&sort_order=desc', colClasses=c('Date'='Date'))
investment_deflator$year<-year(investment_deflator$Date)
investment_deflator$Date<-NULL
names(investment_deflator)[1]<-"deflator"


#Merrill Lynch US Corporate Bonds Total Return Index
corp_bonds<-read.csv('http://www.quandl.com/api/v1/datasets/ML/TRI.csv?&trim_start=1973-05-31&trim_end=2014-01-09&collapse=annual&sort_order=desc')
corp_bonds$bond_index<-corp_bonds$Value
corp_bonds$year<-year(as.Date(as.character(corp_bonds$Date)))
corp_bonds$Date<-NULL
corp_bonds$Value<-NULL

#from npd
overview<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)

month.prod<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_production_monthly&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.56&CultureCode=nb-no", stringsAsFactors=FALSE)

tot.month.prod<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_production_totalt_NCS_month__DisplayAllRows&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.56&CultureCode=nb-no", stringsAsFactors=FALSE)
#in MillSm3

licensee<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_licensee_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)
#write.csv(licensee, "/Users/johannesmauritzen/Google Drive/Oil/cleanedData/licensee.csv")


owners<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_owner_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)
#write.csv(owners, "/Users/johannesmauritzen/Google Drive/Oil/cleanedData/owners.csv")


operators<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_operator_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)
#write.csv(operators, "/Users/johannesmauritzen/Google Drive/Oil/cleanedData/operators.csv")


status<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_activity_status_hst&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)
#write.csv(status, "/Users/johannesmauritzen/Google Drive/Oil/cleanedData/status.csv")


investments<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_investment_yearly&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)

descriptions<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_description&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)

reserves<-read.csv("http://factpages.npd.no/ReportServer?/FactPages/TableView/field_reserves&rs:Command=Render&rc:Toolbar=false&rc:Parameters=f&rs:Format=CSV&Top100=false&IpAddress=158.37.94.112&CultureCode=en", stringsAsFactors=FALSE)


#fieldArea
setwd("/Users/johannesmauritzen/Google Drive/Oil/NorwayGeo/fldArea")
field_area<-readOGR(".", "v_geo_fldarea", stringsAsFactors=FALSE)
field_area_data<-fortify(field_area)

n<-length(field_area)
field.geo<-data.frame("name"=rep(NA, n), "lon"=rep(NA, n), "lat"=rep(NA, n))
for(i in 1:n){
	field.geo$name[i]<-field_area$FIELDNAME[i]
	field.geo$lon[i]<-field_area@polygons[[i]]@labpt[1]
	field.geo$lat[i]<-field_area@polygons[[i]]@labpt[2]
}

field.geo<-field.geo[!duplicated(field.geo$name),]



#plot of points
#northsea<-get_map(location = c(lon = 2.74, lat = 60.56), zoom=6)
#ggmap(northsea) +
#geom_point(aes(x = lon, y = lat), data = field.geo)

#clean and merge

oldNames<-colnames(month.prod)
colnames(month.prod)<-c("name", "year", "month","oil_prod", "gas_prod", "ngl_prod",
                        "condensate_prod", "oe_prod", "water_prod", "info_carrier")

colnames(tot.month.prod)<-c("year", "month","oil_prod", "gas_prod", "ngl_prod",
                            "condensate_prod", "oe_prod", "water_prod")
#total date
tot.month.prod$month.2<-as.character(tot.month.prod$month)
tot.month.prod$month.2[which(tot.month.prod$month<10)]<-paste(0,tot.month.prod$month, sep="")[which(tot.month.prod$month<10)]
tot.month.prod$date<-with(tot.month.prod, paste(month.2, year, sep="/"))
tot.month.prod$date<-paste(1,tot.month.prod$date, sep="/")
tot.month.prod$date<-as.Date(tot.month.prod$date,format="%d/%m/%Y")

#with(tot.month.prod, plot(date, oil_prod, type="l", ylab="oil production, Mill Sm3"))
#with(tot.month.prod, plot(date, gas_prod, type="l", ylab="gas production, Net Bill Sm3"))
#with(tot.month.prod, plot(date, ngl_prod, type="l", ylab="ngl production, Net Mill Sm3"))

month.prod$date<-with(month.prod, paste(month, year, sep="/"))
month.prod$date<-paste(1,month.prod$date, sep="/")
month.prod$date<-as.Date(month.prod$date,format="%d/%m/%Y")

#limit to oil_production*****************************************************

oil_production<-month.prod[, c("name", "year", "month","oil_prod")]

#yearly production of oil
year_prod<-ddply(oil_production, .(name, year), summarise, year_prod=sum(oil_prod))

#create cumulative 
year_prod_cum<-ddply(year_prod, .(name), mutate, cum=cumsum(year_prod) )

#merge with investments
investments<-investments[,1:3]
names(investments)<-c("name", "year", "investmentMillNOK")
investments$investmentMillNOK<-as.numeric(investments$investmentMillNOK)
investments<-investments[investments$year<2014,] #only those 2013 or earlier

fields<-merge(year_prod_cum, investments, by=c("name", "year"), all=TRUE)

#merge with location data?
fields<-merge(fields, field.geo, by="name", all=TRUE)

#merge with reserves data
reserves<-rename(reserves, replace=c("fldName"="name", "fldRecoverableOil"="recoverable_oil", "fldRemainingOil"="remaining_oil"))
oil_reserves<-reserves[,c("name", "recoverable_oil", "remaining_oil")]
fields<-merge(fields, oil_reserves, by="name", all=TRUE)


#create lags
manual_lag<-function(series, lag_num)
#creates a lag of the series and returns the series with equal length
{
	series_lag<-rep(NA, length(series))
	series_lag[(1+lag_num):length(series)]<-series[1:(length(series)-lag_num)]
	return(series_lag)
}

#test<-manual_lag(oil_long$oil_price_real, 1)

oil_long$oil_price_real_l1<-manual_lag(oil_long$oil_price_real, 1)
oil_long$oil_price_real_l2<-manual_lag(oil_long$oil_price_real, 2)
oil_long$oil_price_real_l3<-manual_lag(oil_long$oil_price_real, 3)
oil_long$oil_price_real_l4<-manual_lag(oil_long$oil_price_real, 4)
oil_long$oil_price_real_l5<-manual_lag(oil_long$oil_price_real, 5)
oil_long$oil_price_real_l6<-manual_lag(oil_long$oil_price_real, 6)
oil_long$oil_price_real_l7<-manual_lag(oil_long$oil_price_real, 7)
oil_long$oil_price_real_l8<-manual_lag(oil_long$oil_price_real, 8)


#create oil price differences and lags
oil_long$diff_oil_price<-c(NA, diff(oil_long$oil_price_real))
oil_long$pos_price_diff<-ifelse(oil_long$diff_oil_price>0, oil_long$diff_oil_price,      0.0000001)
oil_long$neg_price_diff<-ifelse(oil_long$diff_oil_price<0, abs(oil_long$diff_oil_price), 0.0000001)

#oil_long$pos_dif_l1<-manual_lag(oil_long$pos_price_diff, 1)
#oil_long$pos_dif_l2<-manual_lag(oil_long$pos_price_diff, 2)
#oil_long$pos_dif_l3<-manual_lag(oil_long$pos_price_diff, 3)
#oil_long$pos_dif_l4<-manual_lag(oil_long$pos_price_diff, 4)
#oil_long$pos_dif_l5<-manual_lag(oil_long$pos_price_diff, 5)
#oil_long$pos_dif_l6<-manual_lag(oil_long$pos_price_diff, 6)

#oil_long$neg_dif_l1<-manual_lag(oil_long$neg_price_diff, 1)
#oil_long$neg_dif_l2<-manual_lag(oil_long$neg_price_diff, 2)
#oil_long$neg_dif_l3<-manual_lag(oil_long$neg_price_diff, 3)
#oil_long$neg_dif_l4<-manual_lag(oil_long$neg_price_diff, 4)
#oil_long$neg_dif_l5<-manual_lag(oil_long$neg_price_diff, 5)
#oil_long$neg_dif_l6<-manual_lag(oil_long$neg_price_diff, 6)

oil_long$diff_oil_price_3yr<-c(rep(NA, 3), diff(oil_long$oil_price_real,3))
oil_long$pos_diff_3yr<-ifelse(oil_long$diff_oil_price_3yr>0, oil_long$diff_oil_price_3yr,      0.000001)
oil_long$neg_diff_3yr<-ifelse(oil_long$diff_oil_price_3yr<0, abs(oil_long$diff_oil_price_3yr), 0.000001)

#merge prices with field
fields$year<-as.numeric(fields$year)
fields<-merge(fields, oil_long, by="year", all.x=TRUE)

#sort
fields<-fields[order(fields$name, fields$year), ]

#merge with deflator
fields<-merge(fields, investment_deflator, all.x=TRUE)

#merge total return corporate bond index
fields<-merge(fields, corp_bonds, all.x=TRUE)

#find initial year of production
fields<-ddply(fields, .(name), mutate, init_year=min(year, na.rm=TRUE))

#add totoals in normal and new, unique data frame
fields<-ddply(fields, .(name), mutate, tot.prod=sum(year_prod, na.rm=TRUE))


#merge with status data
status<-status[,1:4]
names(status)<-c("name", "from", "to", "status")
status$from<-as.Date(status$from, format="%d.%m.%Y")
status$to<-as.Date(status$to, format="%d.%m.%Y")

producing<-status[status$status=="PRODUCING",]
producing$status<-NULL
names(producing)[2:3]<-c("producing_from", "producing_to")


PDO_approved<-status[status$status=="PDO APPROVED", c("name", "from")]
names(PDO_approved)[2]<-"approved_at"

shutdown<-status[status$status=="SHUT DOWN", c("name", "from")]
names(shutdown)[2]<-"shutdown"

fields<-merge(fields, producing, by="name", all=TRUE)
fields<-merge(fields, PDO_approved, by="name", all=TRUE)
fields<-merge(fields, shutdown, by="name", all=TRUE)


#write local copy
fields<-fields[order(fields$name, fields$year),]
fields<-fields[!is.na(fields$name),]
fields<-fields[!is.na(fields$year),]

#deflated investment
fields$investmentMillNOK_real<-with(fields, investmentMillNOK*(1/deflator))

write.csv(fields, "/Users/johannesmauritzen/Google Drive/github/rOil/oil_fields.csv")
