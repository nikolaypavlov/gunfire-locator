setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)
library(signal)

outdir <- "~/Documents/gun_data/15_08_14/"
dir <- "~/Documents/gunfire-15-08-2014/"
origin.files <- list.files(dir, full.names=T, pattern=".*wav")

thr <- 0.5
len <- 4096
f <- 44100
bit <- "16"
filt <- butter(4, 700/44100, type="low")

alter_idx <- function(idx, len) {
  vec <- idx[1]
  for (i in idx) {
    if (tail(vec,1) + len < i) {
      vec <- append(vec, i)
    }
  }
  vec
}

add_fading <- function(x, f, start_len, end_len) {
  fadein <- length(x) / 32
  fadeout <- length(x) / 32
  x <- fadew(x, f, fadein/f, fadeout/f)
  x <- append(rep(0, start_len*f), x)
  x <- append(x, rep(0, end_len*f))
  x <- Wave(x, samp.rate = f, bit = 16)
  x <- normalize(x, unit=bit)
  x
}

save_wave <- function(x, filepath) {
  num <- length(list.files(filepath))
  filename <- paste0(filepath, num+1, ".wav")
  writeWave(x, filename)
  filename
}

extract_candidates <- function(files, thr, len) {
  fnames <- c()
  for (file in files) {
    wave <- readWave(file)
    wave <- Wave(wave@left, samp.rate=wave@samp.rate, bit = 16, pcm=TRUE)
    w <- Wave(as.numeric(filter(filt, wave@left)), samp.rate=wave@samp.rate, bit = 16, pcm=TRUE)
    w <- normalize(w)
    idx <- alter_idx(which(w@left > thr), len*3/4)
    idx <- idx[2:length(idx)]
    for (i in idx) {
      w <- wave@left[(i - len/4 + 1):(i + len*3/4)]
      w <- add_fading(w, f, 1, 0.5)
      name <- save_wave(w, outdir)
      fnames <- rbind(fnames, data.frame(candidate=name, origin=file, position=i, stringsAsFactors=F))
    }
  }
  save(fnames, file=paste0(outdir, "fnames.rda"))
}

extract_candidates(origin.files, thr, len)
