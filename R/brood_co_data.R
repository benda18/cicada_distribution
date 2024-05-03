library(renv)
library(readr)
library(dplyr)
library(leaflet)

renv::snapshot()

rm(list=ls()[!ls() %in% c("bound.states", 
                          "bound.counties", 
                          "brood.out")]);cat('\f')


#https://www.arcgis.com/apps/mapviewer/index.html?url=https://apps.fs.usda.gov/ArcX/rest/services/EDW/EDW_PeriodicalCicadaBroods_01/MapServer
#https://data-usfs.hub.arcgis.com/datasets/usfs::periodical-cicada-broods-feature-layer/explore?location=34.791425%2C-81.588036%2C6.60

mk3 <- function(x){
  out <- x %>%
    as.character()
  if(nchar(out) < 3){
    temp <- paste(rep("0",3-nchar(out)),sep = "", 
                  collapse = "")
    
    out <- paste(temp,out, sep = "", collapse = "")
  }
  
  
  return(out)
  
}


data.frame(state  = c(NA), 
           county = c(NA), 
           brood  = c(NA))


b1 <- data.frame(state  = c(rep(51,8), 
                            rep(54,2)), 
                 county = c(15,69,19,165,23,163,171,031,071,31), 
                 brood  = c("Brood I"))

b1$county <- unlist(lapply(b1$county, mk3))

b2 <- data.frame(state  = c(rep("09", 5), 
                            11, 
                            rep(24, 5), 
                            rep(34, 21), 
                            rep(36, 10), 
                            rep(37, 6), 
                            rep(42, 10), 
                            rep(51, 27)), 
                 county = c(1,9,7,5,3,1,37,3,33,9,17,35,21,
                            1,17,19,23,5,9,39,7,37,27,11,15,
                            31,41,3,13,33,25,29,79,71,39,119,27,
                            111,1,83,21,87,81,171,67,169,197,157,45,
                            95,29,103,11,101,91,17,95,77,3,31,109,
                            139,11,65,147,29,85,510,157,009,179,113,37,
                            61,111,59,87,107,47,019,143,145,75,33,177), 
                 brood  = c("Brood II"))

b2$county <- unlist(lapply(b2$county, mk3))


master.co <- rbind(b1,b2) %>%
  group_by(state,county, brood) %>%
  summarise() %>%
  mutate(., sco = paste(state,county,sep = ""))


if(!"bound.states" %in% ls()){
  bound.states <- tigris::states(cb = T) %>% 
    #.[.$STUSPS %in% brood.out$state,]
    .[.$STUSPS %in% state.abb[!state.abb %in% c("HI", "AK")],]
  
}

if(!"bound.counties" %in% ls()){
  bound.counties <- tigris::counties(cb = T) %>%
    .[.$GEOID %in% master.co$sco,]
}





leaflet() %>%
  # add different provider tiles
  addProviderTiles(
    "CartoDB.Positron",
    group = "CartoDB.Positron"
  ) %>%
  addProviderTiles(
    "OpenStreetMap",
    # give the layer a name
    group = "OpenStreetMap"
  ) %>%
  # addProviderTiles(
  #   "Esri.WorldStreetMap",
  #   group = "Esri.WorldStreetMap"
  # ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    group = "Esri.WorldImagery"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "CartoDB.Positron",
      "OpenStreetMap",
      #"Esri.WorldStreetMap",
      "Esri.WorldImagery"
    ),
    # overlayGroups = c("Potential Sundown Towns", 
    #                   "Place that once expelled<br>
    #         entire Black population", 
    #                   "Green Book Addresses"),
    # position it on the topleft
    position = "bottomleft",
    options = layersControlOptions(collapsed = F)
  ) %>%
addPolygons(
  data = bound.counties, 
  #group = bound.counties, 
  stroke = T, 
  fillColor = "cyan", 
  fillOpacity = 0.33, 
  color = "blue", 
  opacity = 0.33, 
  weight = 1, 
  label = bound.counties$NAME,
  labelOptions = labelOptions(
    textsize = 14,
    permanent = F,
    noHide = NULL
  )
) #%>%
  # addPolylines(data = bound.states, 
  #              group = "States",
  #              stroke = T,
  #              #fillColor = "brown", 
  #              #fillOpacity = 0.33,
  #              opacity = 1,
  #              color = "blue",
  #              weight = 1.2)

