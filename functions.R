setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)
library(dtt)

psd <- function(spec) {
  (abs(spec)^2) / length(spec)
}

energy <- function(x) {
  sum(abs(x)^2)
}

energy_band_dist <- function(x, filt1, filt2) {
  band1 <- filter(filt1, x)
  band2 <- filter(filt2, x)
  energy(band1) / energy(band2)
}

check_bands <- function(x, n, thr, wl, filt1, filt2) {
  vec <- c()
  for (i in n) {
    if (energy_band_dist(x[i:(i+wl)], filt1, filt2) > thr) {
      vec <- append(vec, i)
    }
  }
  vec
}

normalize11 <- function(x) {
  (x - mean(x)) / max(abs(x-mean(x)))
}

get_index <- function(x, n, cell_size=1024, guard_cells=1, trained_cells=1) {
  ((which(n) + guard_cells + trained_cells) - 1) * cell_size + 1
}

cfar <- function(x, false_alarm_rate=0.01, cell_size=1024, guard_cells=1, trained_cells=1) {
  n <- 2 * trained_cells
  thr <- n * (false_alarm_rate^(-1/n) - 1)
  trained_size <- cell_size * trained_cells
  right_position <- cell_size*(2*guard_cells + trained_cells + 1) + 1
  test_cell_start <- trained_size + (guard_cells*cell_size) + 1
  test_cell_end <- test_cell_start + cell_size - 1
  
  trained <- append(x[1:trained_size], x[right_position:length(x)])
  test <- x[test_cell_start:test_cell_end]
  
  noise_power <- sum(trained^2) / 2*trained_cells
  sig_power <- sum(test^2)
  (sig_power / noise_power) > thr
}

spec_moment <- function(spec, m) {
  len <- length(spec)
  spec.psd <- psd(spec)
  (1/len^(m+1)) * sum(spec.psd[1:(len/2)] * (1:(len/2))^m)
}

flatness <- function(x) {
  exp(mean(log(x))) / mean(x)
}

triangular <- function(x, center) {
  bank <- c()
  for (k in x) {
    if (k <= center) {
      val <- 1 - abs((k - center) / (center - head(x,1)))
      bank <- append(bank, val)
    } else if (center <= k) {
      val <- 1 - abs((k - center) / (center - tail(x,1)))
      bank <- append(bank, val)
    }  
  }
  bank
}

melbanks <- function(len, minfreq, maxfreq, num, f) {
  mels <- seq(mel(minfreq), mel(maxfreq), length.out=num+2)
  freqs <- mel(mels, inverse=T)
  picks <- ceiling((len) * freqs / f) 
   
  banks <- matrix(ncol=len/2, nrow=0)
  for (m in 2:(length(picks)-1)) {
    win <- triangular(seq(picks[m-1], picks[m+1]), picks[m])
    win <- append(rep(0, picks[m-1] - head(picks, 1)), win)
    
    if (picks[m+1] != tail(picks, 1)) {
      win <- append(win, rep(0, tail(picks, 1) - picks[m+1]))
    }
    
    banks <- rbind(banks, win)
  }
  banks
}

mfcc <- function(spec, f, minfreq=1, maxfreq=f/2, numcep=26) {
  # 1. Get power spectra density
  spec.psd <- psd(spec)
  
  # 2 Compute the Mel-spaced filterbank
  banks <- melbanks(length(spec), minfreq, maxfreq, numcep, f)
  
  # 3 Calculate filterbank energies
  banks.energies <- apply(banks, 1, function(x) log(sum(x * spec.psd)))
  
  # 4 Get cepstral coefficents
  ceps <- dct(as.numeric(banks.energies))
  ceps
}

extract_features <- function(x, f) {
  len <- length(x)
  spec <- fft(x)
  freqs <- (1:len) * f / len
  
  # Features
  zcr <- zcr(x,f, wl=length(x)-1, plot=F)[2]
  spec.m0 <- spec_moment(spec, 0)
  spec.m1 <- spec_moment(spec, 1)
  spec.m2 <- spec_moment(spec, 2)
  spec.m3 <- spec_moment(spec, 3)
  spec.flatness <- flatness(abs(spec))
  spec.centroid <- sum(freqs * abs(spec)) / sum(abs(spec))
  melfcc <- mfcc(spec, f)
  melfcc <- melfcc[1:(length(melfcc)/2)]
  
  data <- c(zcr, spec.centroid, spec.m0, spec.m1, spec.m2, spec.m3, spec.flatness, melfcc)
  names <- c("zcr", "spec.centroid", "spec.m0", "spec.m1", "spec.m2", "spec.m3", "spec.flatness", paste('melfcc.', 1:length(melfcc), sep='')) 
  names(data) <- names
  data.frame(t(data))
}
