#0.PREPARACIÓN####
packages<-c("tidyverse","sp","rgdal",
                      "raster","DHARMa","mgcv")
sapply(packages,require,character.only=TRUE)
#1.Relativizar coordenadas####

zi<-xi-min/max-min



#readtmean
#readprec
#readtmax
#evaptranstot
pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)