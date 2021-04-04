#0.PREPARACIÓN####
packages<-c("tidyverse","sp","rgdal",
                      "raster","DHARMa","mgcv")
sapply(packages,require,character.only=TRUE)
#1.Relativizar coordenadas####

zi<-xi-min/max-min
fields::Tps(x, Y, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
    miles = TRUE, method = "GCV", GCV = TRUE, ...) #función thinplate



#readtmean
#readprec
#readtmax
#evaptranstot
pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)