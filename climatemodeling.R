#0.PREPARACIÓN####
packages<-c("tidyverse","sp","rgdal",
            "raster","DHARMa","mgcv","fields","viridis","leaflet",
            "htmltools","htmlwidgets")
sapply(packages,require,character.only=TRUE,quietly=TRUE)
projectionUTM<-"+proj=utm +zone=28 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
projectiongeo<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#1.1. Cotejar coordenadas ISA Gran Canaria

GC<-readOGR("estacionesAEMET_GC.shp")
GCgeo<-spTransform(GC,projectiongeo)
coordinates(GCgeo)
limGC<-extent(GCgeo)
plot(GCgeo)
GCdf<-data.frame(GC) %>% mutate(LONG=LONG/10000,LAT=LAT/10000) %>% 
  select(-c(XUTM30,YUTM30,PROV_NOMBR,NOMHOJA,ALTI_ANE,PROV_ID,
            NUM_CUENCA,HOJA_ID,GR_CUENCA_,TIPO_CORRI,AMBITO,
            AMBITO_ID,CORRIENTE,CDR1,CDR2,optional,COMENTARIO,
            coords.x1,coords.x2))
GCdf$x<-coordinates(GCgeo)[,1]
GCdf$y<-coordinates(GCgeo)[,2]
tmean<-read_delim("TEMP.MEDIA MENSUAL.CSV",delim=";",skip=1)%>% 
  subset(NOM_PROV=="LAS PALMAS") %>% na.omit() %>% mutate(x=LONGITUD/-100000,y=LATITUD/10000) %>% 
  mutate(LONGITUD=NULL,LATITUD=NULL)  %>% subset(x<15*-1 & y>27.3) %>% 
  group_by(Indicativo,NOMBRE)

tmean<- tmean %>% summarise(across(enero:diciembre,mean),n=n(),across(x:y,max))

GCdf<-GCdf %>% rename(Indicativo=IND_INM)
GCdf[c("FID_","INDCTV","TIPO")]<-NULL
GCdf[c("SGE","TIPO_CORR_","TOTANOS","TOTANOSC")]<-NULL
GCdf[c("MUNI_ID")]<-NULL
GCtmean<-full_join(GCdf,tmean,by="Indicativo",keep=TRUE,suffix=c("_GC","_AEMET"))
GCtmean<-GCtmean %>% relocate(Indicativo_AEMET,.after=Indicativo_GC) %>% 
relocate(NOMBRE,.after = LUGAR)
  View(GCtmean)
GCtmean %>% na.omit() %>% View()
GCtmean %>% select(Indicativo_AEMET) %>% na.omit() %>% dim()
GCtmean %>% select(Indicativo_GC) %>% na.omit() %>% dim()
SpatialPointsDataFrame(coords=tmean[c("x","y")],data=tmean) %>% plot()
points(GC)


mapaweb<-leaflet() %>% 
  addTiles(group="Open Street Maps") %>%
  addProviderTiles(providers$Esri.WorldImagery,group="Ortoimágenes") %>% 
  addWMSTiles(baseUrl="http://www.madrid.org/cartografia/ide/wms/WMS_ORTOIMAGENES.xml",
              layers="ORTO_50CM_20092",
              group = "Open Street Maps",
              options = WMSTileOptions(format = "image/png", transparent = F)) %>%
  setView(lng = -15.3, lat = 28.3, zoom = 12)%>% 
  addMarkers(data=GCdf,
             #group="Actuaciones LIFE CAÑADAS",
             #icon=iconoLIFE,
             lng=GCdf$x,lat=GCdf$y, 
             popup = GCdf$LUGAR) %>% 
  #addMarkers(data=tmean,
             #group="Actuaciones LIFE CAÑADAS",
             # icon=greenLeafIcon,
             # lng=tmean$x,lat=tmean$y#,
             # # popup = tmean$NOMBRE
  # ) %>% 
  
  
  
  addLayersControl(
    baseGroups = c("Open Street Maps","Ortoimágenes"),
    #overlayGroups = c("Vías Pecuarias","Unidad de Arcosas", "Actuaciones LIFE CAÑADAS"),
    options = layersControlOptions(collapsed = TRUE)
  )
saveWidget(mapaweb,file="mapaweb.html")
#1.Relativizar coordenadas####

predictores<-GCdf %>% select(x,y,ALT)
precipitacion<-
fields::Tps(x, Y, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
            miles = TRUE, method = "GCV", GCV = TRUE, ...) #función thinplate

zi<-xi-min/max-min




#readtmean
#readprec
#readtmax
#evaptranstot
pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)