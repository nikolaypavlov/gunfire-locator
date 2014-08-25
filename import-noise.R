setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)

wl <- 1024
step <- 512
f <- 44100

#
# Noise 
#
dir <- "~/Documents/Audio_Noise/"
noise.files <- list.files(dir, full.names=T)

waves <- list()
for (file in noise.files) {
  waves[[basename(file)]] <- resamp(readWave(file), g=f, output="Wave")
  waves[[basename(file)]] <- normalize(waves[[basename(file)]])
}

noise <- lapply(waves, function(x) rollapply(x@left, width=wl, function(y) y, by=step))
noise <- do.call(rbind.data.frame, noise)

save(noise, file="data/noise.rda")
