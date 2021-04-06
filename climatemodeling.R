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
limGC<-extent(GC)
plot(GC)
GCdf<-data.frame(GC) %>% mutate(LONG=LONG/10000,LAT=LAT/10000)

tmean<-read_delim("TEMP.MEDIA MENSUAL.CSV",delim=";",skip=1) %>% 
  subset(NOM_PROV=="LAS PALMAS") %>% mutate(x=LONGITUD/-100000,y=LATITUD/10000) %>% 
  mutate(LONGITUD=NULL,LATITUD=NULL) %>% rename(year=5) %>% subset(x<15*-1 & y>27.3)
SpatialPointsDataFrame(coords=tmean[c("x","y")],data=tmean) %>% plot()
points(GC)

greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)
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
             lng=GCdf$LONG,lat=GCdf$LAT, 
             popup = GCdf$LUGAR) %>% 
  addMarkers(data=tmean,
             #group="Actuaciones LIFE CAÑADAS",
             icon=greenLeafIcon,
             lng=tmean$x,lat=tmean$y#,
            # popup = tmean$NOMBRE
             ) %>% 
  
  
  
  addLayersControl(
    baseGroups = c("Open Street Maps","Ortoimágenes"),
    #overlayGroups = c("Vías Pecuarias","Unidad de Arcosas", "Actuaciones LIFE CAÑADAS"),
    options = layersControlOptions(collapsed = TRUE)
  )
saveWidget(mapaweb,file="mapaweb.html")
#1.Relativizar coordenadas####

zi<-xi-min/max-min
fields::Tps(x, Y, m = NULL, p = NULL, scale.type = "range", lon.lat = FALSE,
    miles = TRUE, method = "GCV", GCV = TRUE, ...) #función thinplate



#readtmean
#readprec
#readtmax
#evaptranstot
pet<-batchPetHgsm(petfiles, 01, tmin, tmean, tmax, rad)