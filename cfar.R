setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)
library(dtt)

source("functions.R")

cell_size <- 1024
thr <- 1
wave <- readWave("~/Documents/shots.wav")@left

filt1 <- butter(4, c(100/44100, 500/44100), type="pass")
filt2 <- butter(4, 700/44100, type="high")

candidates <- get_index(wave, rollapply(wave, width = cell_size*5, by=cell_size, cfar))
candidates <- check_bands(wave, candidates, thr, cell_size, filt1, filt2)

plot.ts(wave)
rect(xleft = candidates, ybottom = min(wave), xright = candidates+cell_size*4, ytop = max(wave), col= '#FF003322')
