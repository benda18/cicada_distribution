library(renv)
library(readr)
library(dplyr)

rm(list=ls());cat('\f')

parse_cic <- function(x){
  out <- as.data.frame(matrix(unlist(strsplit(x, "\n")), ncol = 3, byrow = T))
  colnames(out) <- c("county", "state", "brood")
  return(out)
}


b1 <- "Augusta
VA
Brood I
Shenandoah
VA
Brood I
Pendleton
WV
Brood I
Campbell
VA
Brood I
Rockbridge
VA
Brood I
Botetourt
VA
Brood I
Hardy
WV
Brood I
Bedford
VA
Brood I
Rockingham
VA
Brood I
Frederick
VA
Brood I"


parse_cic(b1)


