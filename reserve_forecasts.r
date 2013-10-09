#forecast estimates
library(gdata)

reserve_estimates_2004<-read.xls("http://npd.no/Global/Norsk/2%20-%20Tema/Ressursregnskap%20og%20analyser/Norsk/Dokumenter/Ressursregnskapet%20per%2031.12.2004%20(MS%20Excel%20-%200,3MB).xls",
		sheet=2)