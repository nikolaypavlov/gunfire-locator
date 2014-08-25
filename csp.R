setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)

cspfun <- function(x.spec, y.spec) {
  # Phase Transform (PHAT) Weighting Function
  phat <- 1/abs(Conj(x.spec) * y.spec)
  #phat <- 1
  
  # Generalized Cross Correlation (GCC)
  fft(phat * Conj(x.spec) * y.spec, inverse=T)
}

delay_est <- function(x, thr) {
  len <- length(x)
  pos.idx <- which.max(abs(x[1:thr]))
  neg.idx <- which.max(abs(x[len:(len-thr+1)]))
  if (abs(x[pos.idx]) < abs(x[len-neg.idx+1])) {
    -1 * neg.idx 
  } else {
    pos.idx
  }
}

azimuth <- function(t1, t2, t3) {
  ex = (t1 + t2) * sqrt(1/3)
  ey = (t2 - t1)
  ez = t3 * (sqrt(3/2)) - (t2 + t1) / sqrt(6)
  
  az <- 0
  if (ex <= 0) {
    az <- atan(ey/ex)
  } else {
    az <- atan(ey/ex) + 180
  }
  az
  #ang <- atan(ez/sqr(ex*ex + ey*ey))
}

rec1 <- readWave("~/Downloads/mic/Petarda/p_asimut_40/asimut_40/1_001.wav")
rec2 <- readWave("~/Downloads/mic/Petarda/p_asimut_40/asimut_40/2_001.wav")
rec3 <- readWave("~/Downloads/mic/Petarda/p_asimut_40/asimut_40/3_001.wav")
rec4 <- readWave("~/Downloads/mic/Petarda/p_asimut_40/asimut_40/4_001.wav")

#rec0 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 1_001 (7).wav")
#rec1 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 2_001 (7).wav")
#rec2 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 3_001 (7).wav")
#rec3 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 4_001 (7).wav")


c <- 331
thr <- 108
rate <- rec1@samp.rate
from <- rate * 34.848
to <- from + rate
d01 <- 0.4
d02 <- 0.4
d03 <- 0.4

mic0_spec <- colMeans(rollapply(rec0@left[from:to], width=2048, fft, by=1024))
mic1_spec <- colMeans(rollapply(rec1@left[from:to], width=2048, fft, by=1024))
mic2_spec <- colMeans(rollapply(rec2@left[from:to], width=2048, fft, by=1024))
mic3_spec <- colMeans(rollapply(rec3@left[from:to], width=2048, fft, by=1024))

csp1 <- Re(cspfun(mic0_spec, mic1_spec))
csp2 <- Re(cspfun(mic0_spec, mic2_spec))
csp3 <- Re(cspfun(mic0_spec, mic3_spec))

t1 <- delay_est(csp1, thr)
t2 <- delay_est(csp2, thr)
t3 <- delay_est(csp3, thr)

d1 <- c * t1/rate
d2 <- c * t2/rate
d3 <- c * t3/rate

#th1 <- acos(d1 / d01) * 180/pi
#th2 <- acos(d2 / d02) * 180/pi
#th3 <- acos(d3 / d03) * 180/pi

az <- azimuth(t1, t2, t3)

par(mfcol = c(2, 1))
csp <- csp2
t <- t2

plot.ts(rec0@left[from:to][1:512], col=1, main="Mic1, Mic2 signals", xlab="Sample")
lines(rec2@left[from:to][1:512], col=2)

if (t > 0) {
  plot(abs(csp)[1:256], main="CSP", xlab="Delay estimate", pch=19, cex=0.6)
} else {
  len <- length(csp)
  plot(abs(csp)[len:(len-255)], main="-CSP", xlab="Delay estimate", pch=19, cex=0.6)
}

#plot(abs(csp2)[1:256], main="", xlab="Delay estimate", pch=19, cex=0.6)
#plot(abs(csp3)[1:256], main="CSP", xlab="Delay estimate", pch=19, cex=0.6)
# 201
