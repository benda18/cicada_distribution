library(renv)
library(tigris)
library(dplyr)
library(lubridate)
library(leaflet)
library(ggplot2)
library(data.table)
library(rmarkdown)

renv::snapshot()

rm(list=ls()[!ls() %in% c("bound.states", 
                          "bound.counties", 
                          "brood.out")]);cat('\f')

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


if(!"brood.out" %in% ls()){
  brood.out <- NULL
  for(i in 1:nrow(cw.broodregion)){
    if(!is.na(cw.broodregion$gen_reg[i])){
      
      brood.out <- rbind(brood.out, 
                         data.frame(brood_num = cw.broodregion$brood_num[i], 
                                    state = trimws(unlist(strsplit(cw.broodregion[i,]$gen_reg, ",")))))
      
      
    }
    
  }
  
  brood.out %>% as_tibble()
}


# mapping----


if(!"bound.states" %in% ls()){
  bound.states <- tigris::states(cb = T) %>% 
  #.[.$STUSPS %in% brood.out$state,]
    .[.$STUSPS %in% state.abb[!state.abb %in% c("HI", "AK")],]
  
}

if(!"bound.counties" %in% ls()){
  bound.counties <- tigris::counties(cb = T) %>%
    .[.$STUSPS %in% c(brood.out$state, c("AZ", "NM", "CO", "NV", 
                                         "CA", "ID", "WA", 
                                         "OR", "MT", "WY", "UT", 
                                         "ND", "SD", "MN", 
                                         "RI", "ME", "VT", "NH")),]
}


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


# defin counties----
defin.counties <- rbind(data.frame(state = "MI", 
                                   conam = c("Berrien", "Cass", "St. Joseph", "Jackson",
                                             "Branch", "Hillsdale")), 
                        data.frame(state = "WI", 
                                   conam = c("Crawford", "Grant", "Iowa", "Richland", 
                                             "Saulk", "Dane", "Jefferson", "Waukesha", 
                                             "Milwaukee", "Green", "Rock", "Walworth")), 
                        data.frame(state = "NY",
                                   conam = c("Ontario", "Yates", "Seneca", "Kings", 
                                             "Queens", "Nassau", "Suffolk", "Rockland",
                                             "Richmond",
                                             "Bronx", "New York", "Westchester", 
                                             "Putnam", "Rensselaer", "Columbia", "Dutchess", 
                                             "Albany", "Greene", "Ulster", "Orange")), 
                        data.frame(state = "MA", 
                                   conam = c("Plymouth", "Barnstable")), 
                        data.frame(state = "NE", 
                                   conam = c("Otoe", "Cass", "Sarpy")), 
                        data.frame(state = "KS", 
                                   conam = c("Atchison", "Jackson", 
                                             "Pottawatomie", "Wabaunsee", 
                                             "Lyon", "Greenwood", "Woodson", 
                                             "Wilson", "Allen", "Bourbon", 
                                             "Labette", "Cherokee", "Osage", 
                                             "Coffey", "Douglas", "Johnson", 
                                             "Wyandotte", "Morris", "Chase", 
                                             "Marion")),
                        data.frame(state = "OK", 
                                   conam = c("Osage", "Washington", "Nowata", 
                                             "Craig", "Mayes", "Wagoner", 
                                             "Tulsa", "Pawnee", "Muskogee", 
                                             "Adair", "McCurtain", "Choctaw", 
                                             "Bryan", "Love", "Carter",
                                             "Jefferson", "Comanche" )), 
                        data.frame(state = "TX", 
                                   conam = c("Red River", "Lamar", "Fannin", 
                                             "Cooke", "Kaufman")), 
                        data.frame(state = "NC", 
                                   conam = c("Durham", "Wake", "Orange", 
                                             "Chatham", "Franklin", "Warren", 
                                             "Halifax","Northampton", 
                                             "Alamance", "Randolph", "Guilford", 
                                             "Rockingham", "Stokes", "Forsyth", 
                                             "Yadkin", "Surry", "Alleghany", 
                                             "Wilkes", "Watauga", "Caldwell", 
                                             "Burke", "McDowell", "Mitchell", 
                                             "Yancey", "Madison", "Buncombe", 
                                             "Haywood", "Jackson", "Robeson", 
                                             "Iredell", "Mecklenburg", "Cabarrus", 
                                             "Rutherford", "Polk", "Henderson", 
                                             "Transylvania", "Cherokee")), 
                        data.frame(state = "SC", 
                                   conam = c("Oconee", "Pickens", "Anderson", 
                                             "Greenville", "Spartanburg", 
                                             "Laurens", "Union", "York", 
                                             "Aiken")), 
                        data.frame(state = "LA", 
                                   conam = c("Caddo", "Caliborne", "East Carroll", 
                                             "West Carroll", "Morehouse", "Ouachita", 
                                             "Richland", "Madison", "Concordia", 
                                             "Catahoula", "Franklin", "Tensas", 
                                             "Pointe Coupee", "West Feliciana", 
                                             "East Feliciana", "St. Helena", 
                                             "Livingston", "East Baton Rouge", 
                                             "Claiborne")), 
                        data.frame(state = "AR", 
                                   conam = c("Sebastian", "Scott", "Logan", "Johnson", 
                                             "Van Buren", "Searcy", "Newton", "Stone", 
                                             "Izard", "Sharp", "Randolph", 
                                             "Benton", "Washington", "Crawford", 
                                             "Franklin", "Madison", "Baxter",
                                             "Carroll", "Boone", "Marion", "Fulton", 
                                             "Sevier", "Howard", "Pike", "Clark", 
                                             "Hot Spring", "Columbia", "Cleveland", 
                                             "Drew", "Jefferson", 
                                             "Monroe", "Phillips", "Lee", "St. Francis", 
                                             "Crittenden", "Cross", "Poinsett", 
                                             "Craighead", "Mississippi", "Prairie")),
                        data.frame(state = "AL", 
                                   conam = c("Lamar", "Sumter", "Hale", 
                                             "Jefferson", "Blount",
                                             "St. Clair", "Etowah", "DeKalb", "Cherokee", 
                                             "Jackson", "Marshall", "Randolph", "Perry", 
                                             "Dallas", "Autauga", "Lowndes", "Elmore", 
                                             "Montgomery", "Macon", "Russell")),
                        data.frame(state = "GA",
                                   conam = c("Walker", "Chattooga", "Floyd", "Polk", "Fulton", 
                                             "Cherokee", "Gilmer", "Fannin", "Union", "White", 
                                             "Habersham", "Rabun", "Jackson", "Madison", 
                                             "Richmond"))
                        # data.frame(state = "KY", 
                        #            conam = c("Pendleton", "Union"))
                        ) %>%
  mutate(., 
         cost = paste(conam, state))
def.not.counties <- bound.counties[bound.counties$STUSPS %in% 
                                     defin.counties$state & 
                                     !paste(bound.counties$NAME, 
                                            bound.counties$STUSPS) %in% 
                                     defin.counties$cost,] %>%
  mutate(., 
         cost = paste(NAME, STUSPS)) %>%
  .$cost

# notin counties----
not.counties <- rbind(data.frame(state = "OH", 
                                 conam = c("Williams", "Defiance", 
                                           "Paulding", "Van Wert", 
                                           "Putnam", "Wood", "Seneca", 
                                           "Crawford", "Morrow", "Fulton", "Henry", 
                                           "Lucas", "Ottawa", "Sandusky", "Hardin", 
                                           "Ashtabula", "Pickaway")), 
                      data.frame(state = "IN", 
                                 conam = c("Jasper", "Newton", "Cass", "Wabash", 
                                           "Whitley", "LaGrange")), 
                      data.frame(state = "PA", 
                                 conam = c("Erie", "Warren", "Forest", 
                                           "Elk", "McKean", "Clearfield", 
                                           "Cameron", "Potter", "Lycoming", 
                                           "Tioga", "Northumberland", 
                                           "Sullivan", "Bradford", 
                                           "Susquehanna", "Wayne", 
                                           "Lackawanna")), 
                      data.frame(state = "CT", 
                                 conam = c("Northeastern Connecticut", 
                                           "Southeastern Connecticut", 
                                           "Capitol")), 
                      data.frame(state = "IL", 
                                 conam = c("Henderson", "Schuyler", 
                                           "Brown", "Menard", "Clark", 
                                           "Moultrie", "Fayette", 
                                           "Calhoun")), 
                      data.frame(state = "MO", 
                                 conam = c("St. Louis", "Shelby", 
                                           "Adair", "Sullivan", 
                                           "Carroll", "Daviess", "Platte", 
                                           "Clay","Clinton", 
                                           "Cass", "Bates", "McDonald", "Newton", 
                                           "Jasper", "Stone", "Taney", "Douglas", 
                                           "Dunklin")), 
                      data.frame(state = "IA", 
                                 conam = c("Appanoose", "Lucas", "Clarke", 
                                           "Washington", "Harrison", "Shelby", 
                                           "Audubon", "Guthrie", "Carroll", 
                                           "Calhoun", "Humboldt", 
                                           "Pocahontas", "Wright", "Franklin", 
                                           "Hardin", "Grundy", 
                                           "Butler", "Floyd", "Chickasaw", 
                                           "Winneshiek", "Allamakee", 
                                           "Howard", "Mitchell", "Worth", 
                                           "Cerro Gordo", "Hancock", "Winnebago", 
                                           "Kossuth", "Emmet", "Palo Alto", 
                                           "Dickinson", "Clay", "Buena Vista", 
                                           "Sac", "Osceola", "O'Brien", "Cherokee", 
                                           "Ida", "Crawford", "Lyon", "Sioux", 
                                           "Plymouth", "Woodbury", 
                                           "Monona")), 
                      data.frame(state = "MS", 
                                 conam = c("Sunflower", "Washington", "Sharkey", "Walthall", 
                                           "Waithall", "Jefferson Davis", "Covington", "Jones", 
                                           "Wayne", "Lamar", "Forrest", "Perry", "Greene", 
                                           "Pearl River", "Stone", "George", "Hancock", 
                                           "Harrison", "Jackson", "Newton", "Neshoba", 
                                           "Kemper", "Choctaw", "Chickasaw", "Clay", "Winston", 
                                           "Noxubee", "Lowndes", "Oktibbeha", "Monroe", 
                                           "Itawamba")),
                      data.frame(state = "AZ", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "AZ"]), 
                      data.frame(state = "NM", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "NM"]), 
                      data.frame(state = "CO", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "CO"]), 
                      data.frame(state = "MN", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "MN"]), 
                      data.frame(state = "ND", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "ND"]), 
                      data.frame(state = "SD", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "SD"]), 
                      data.frame(state = "MT", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "MT"]), 
                      data.frame(state = "WY", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "WY"]), 
                      data.frame(state = "ID", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "ID"]), 
                      data.frame(state = "WA", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "WA"]), 
                      data.frame(state = "OR", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "OR"]), 
                      data.frame(state = "CA", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "CA"]), 
                      data.frame(state = "NV", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "NV"]), 
                      data.frame(state = "UT", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "UT"]), 
                      data.frame(state = "RI", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "RI"]), 
                      data.frame(state = "ME", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "ME"]), 
                      data.frame(state = "VT", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "VT"]), 
                      data.frame(state = "NH", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "NH"]), 
                      data.frame(state = "FL", 
                                 conam = bound.counties$NAME[bound.counties$STUSPS == 
                                                               "FL"])
                      )

not.counties$cost <- paste(not.counties$conam, 
                           not.counties$state)

paste(bound.counties$NAME, bound.counties$STUSPS)

bound.counties2 <- bound.counties[!paste(bound.counties$NAME, 
                                         bound.counties$STUSPS) %in%
                                    c(def.not.counties, not.counties$cost),] 

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
    data = bound.counties2, 
    #group = bound.counties, 
    stroke = T, 
    fillColor = "cyan", 
    fillOpacity = 0.66, 
    color = "blue", 
    opacity = 0.33, 
    weight = NA, 
    # label = bound.counties2$NAME,
    # labelOptions = labelOptions(
    #   textsize = 14,
    #   permanent = F,
    #   noHide = NULL
    # )
  ) %>%
  addPolygons(
    data = bound.counties, 
    #group = bound.counties, 
    stroke = T, 
    # fillColor = "cyan", 
    # fillOpacity = 0.66, 
    color = "grey", 
    opacity = 0.33, 
    weight = NA, 
    label = (bound.counties$NAME),
    labelOptions = labelOptions(
      textsize = 14,
      permanent = F,
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
