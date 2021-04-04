#0.PREPARACIÓN####
packages<-c("tidyverse","sp","rgdal",
                      "raster","DHARMa","mgcv")
sapply(packages,require,character.only=TRUE)
#1.Relativizar coordenadas####

#ej.tenerife
maxlat<-15
maxlong<-28


#readtmean
#readprec
#readtmax
#evaptranstot
pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)