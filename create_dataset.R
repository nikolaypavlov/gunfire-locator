setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)

load("~/Documents/gun_data/15_08_14/fnames.rda")
fnames$position <- as.numeric(fnames$position)

len <- 1024
f <- 44100 

gunfire.events <- c(1:25,27:34,40:80)

create_dataset <- function(valid.positions, names) {
  w <- c()
  for (i in valid.positions) {
    start <- names$position[i] - len/4 + 1
    end <- names$position[i] + len*3/4
    wav <- normalize(readWave(names$origin[i]))
    w <- rbind(w, wav@left[start:end])
  }
  w
}

waves <- create_dataset(gunfire.events, fnames)

save(waves, file="data/gunfire.rda")
