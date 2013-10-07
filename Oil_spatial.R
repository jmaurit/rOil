#Oil_spatial
#http://en.wikipedia.org/wiki/Shapefile


#field area shape file
#The datum of the geometry of the Shapefile is European Datum 1950 (ED50, 
#spatial reference ID is EPSG:4230).
#from project file:
#GEOGCS["GCS_European_1950",
#DATUM["D_European_1950",SPHEROID["International 1924",6378388,297,AUTHORITY["EPSG","7022"]],
#AUTHORITY["EPSG","6230"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4230"]]
#from http://factpages.npd.no/factpages/default.aspx?culture=en&nav1=licence&nav2=PageView%7cPetReg

library(ggmap)
library(rgdal)
library(sp)
library(maptools)
library(maps)
library(mapdata) #worldHiRes
library(mapproj)
library(rgeos)


#use ggmap to get map
northsea<-get_map(location = c(lon = 2.74, lat = 60.56), zoom=6)
ggmap(northsea)

#norway
setwd("/Users/johannesmauritzen/Google Drive/data-sets/NorwaySpatial/33_N2000_shape")
Norway_areal<-readOGR(".", "NO_Arealdekke_lin")
plot(Norway_areal, add=TRUE)

#fields
setwd("/Users/johannesmauritzen/Google Drive/Oil/NorwayGeo/fldArea")
field_area<-readOGR(".", "v_geo_fldarea")
field_area_data<-fortify(field_area)

#discovery 
setwd("/Users/johannesmauritzen/Google Drive/Oil/NorwayGeo/dscArea")
discovery<-readOGR(".", "v_geo_dscarea")
#shapefile<-readShapeSpatial("v_geo_dscarea.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))
discovery.gg<-fortify(shapefile)




#pipeline
setwd("/Users/johannesmauritzen/Google Drive/Oil/NorwayGeo/pipLine")
pipeline<-readOGR(".", "v_geo_pipline")
#pipline_data<-readShapeSpatial("v_geo_pipline.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))
pipeline_data<-fortify(piplin)


#Norwegian continental shelf structures
setwd("/Users/johannesmauritzen/Google Drive/Oil/NorwayGeo/NCS_structural/domes")
domes<-readOGR(".", "domes")
faults<-readOGR(".", "faults_bounddaries")
faults.gg<-fortify(faults)
structural<-readOGR(".", "structural_elements")
northsea<-get_map(location = c(lon = 2.74, lat = 60.56), zoom=7)
ggmap(northsea) +
geom_polygon(aes(x = long, y = lat, group = group), data = faults.gg,
    colour = 'black', fill = 'black', alpha = 0, size = .2) +
geom_polygon(aes(x = long, y = lat, group = group), data = discovery,
    colour = 'black', fill = 'black', alpha = 0, size = .2) +
geom_polygon(aes(x = long, y = lat, group = group), data = field_area_data,
    colour = 'black', fill = 'black', alpha = 0, size = .2) +
#geom_polygon(aes(x = long, y = lat, group = group), data = discovery,
  #  colour = 'white', fill = 'green', alpha = .7, size = .2) +
geom_line(aes(x = long, y = lat, group = group), data = pipeline_data,
    colour = 'red', alpha = .4, size = .3)  


