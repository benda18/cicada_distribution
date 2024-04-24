library(renv)
library(tigris)
library(dplyr)
library(lubridate)
library(leaflet)
library(ggplot2)
library(data.table)

snapshot()

rm(list=ls());cat('\f')

# https://cicadas.uconn.edu/broods/

b17 <- data.frame(lifecycle = "17-year", 
                  brood_num = 1:17, 
                  brood_name = c("Shenandoah", 
                                 "East Coast", 
                                 "Iowan", 
                                 "Kansan", 
                                 NA, 
                                 "Brushy Mountains", 
                                 "Onondaga", 
                                 NA, 
                                 NA, 
                                 "Great Eastern", 
                                 NA, NA, 
                                 "Northern Illinois", 
                                 rep(NA,4)), 
                  last_emerg = c(2012,2013,2014,2015,
                                 2016,2017,2018,2019,
                                 2020,2021,1954,NA,
                                 2024,2008,
                                 rep(NA,3))) %>%
  as_tibble()

b13 <- data.frame(lifecycle = "13-year", 
                  brood_num = c(18:24, 27, 29, 30), 
                  brood_name = c(NA, 
                                 "Great Southern", 
                                 rep(NA,2), 
                                 "Baton Rouge", 
                                 "Lower Mississippi", 
                                 rep(NA, 4+0)), 
                  last_emerg = c(NA, 2024,NA,1870, 
                                 2014, 2014, 
                                 rep(NA,4+0))) %>%
  as_tibble() 

b.all <-rbind(b17,b13)

cw.broodregion <- data.frame(brood_num = 1:17, 
                             gen_reg = c("TN, VA, WV", 
                                         "CT, GA, MD, NC, NJ, NY, OK, PA, SC, VA", 
                                         "IA, IL, MO", 
                                         "IA, KS, MO, NE, OK, TX", 
                                         "MD, OH, PA, VA, WV", 
                                         "GA, NC, SC", 
                                         "NY", 
                                         "OH, PA, WV",
                                         "NC, VA, WV",
                                         "DE, GA, IL, IN, KY, MD, MI, NC, NJ, NY, OH, PA, TN, VA, WV", 
                                         "CT", 
                                         NA, 
                                         "IA, IL, IN, WI", 
                                         "KY, GA, IN, MA, MD, NC, NJ, NY, OH, PA, TN, VA, WV", 
                                         rep(NA, 3)))

cw.broodregion <- rbind(cw.broodregion, 
                        data.frame(brood_num = c(19, 21, 22, 23), 
           gen_reg = c("AL, AR, GA, IN, IL, KY, LA, MD, MO, MS, NC, OK, SC, TN, VA", 
                       "FL", 
                       "LA, MS, OH, KY", 
                       "AR, IL, IN, KY, LA, MO, MS, TN"))) %>%
  .[complete.cases(.),]


brood.out <- NULL
for(i in 1:nrow(cw.broodregion)){
  if(!is.na(cw.broodregion$gen_reg[i])){
    
    brood.out <- rbind(brood.out, 
                       data.frame(brood_num = cw.broodregion$brood_num[i], 
                                  state = trimws(unlist(strsplit(cw.broodregion[i,]$gen_reg, ",")))))
    
    
  }
  
}

brood.out %>% as_tibble()

# mapping----

bound.states <- tigris::states(cb = T) %>%
  .[.$STUSPS %in% brood.out$state,]

bound.counties <- tigris::counties(cb = T) %>%
  .[.$STUSPS %in% brood.out$state,]

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



not.counties <- data.frame(state = 39, 
                           conum = c(161, 125,39,171,51,69,137,95,173,63,
                                     147,143,123,43))


bound.counties$STATEFP[bound.counties$STUSPS == "OH"]

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
    fillOpacity = 0.66, 
    color = "blue", 
    opacity = 0.33, 
    weight = NA, 
    label = as.numeric(bound.counties$COUNTYFP),
    labelOptions = labelOptions(
      textsize = 14,
      permanent = T, 
      noHide = NULL
    )
  ) %>%
  addPolylines(data = bound.states, 
              group = "States",
              stroke = T,
              fillColor = "brown", 
              fillOpacity = 0.33,
              opacity = 1,
              color = "black",
              weight = NA)
