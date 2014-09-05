filt3 <- function(x, coef_a, coef_b) {
  y <- c()
  
  coef_a <- -coef_a
  
  y[1] <- coef_b[1]*x[1]
  y[2] <- coef_b[1]*x[2] + coef_b[2]*x[1] + coef_a[2]*y[1]
  y[3] <- coef_b[1]*x[3] + coef_b[2]*x[2] + coef_b[3]*x[1] + coef_a[2]*y[2] + coef_a[3]*y[1]
  y[4] <- coef_b[1]*x[4] + coef_b[2]*x[3] + coef_b[3]*x[2] + coef_b[4]*x[1] + coef_a[2]*y[3] + coef_a[3]*y[2] + coef_a[4]*y[1]
  
  for (i in 5:length(x)) {
    y[i] <- coef_b[1]*x[i] + coef_b[2]*x[i-1] + coef_b[3]*x[i-2] + coef_b[4]*x[i-3] + coef_b[5]*x[i-4] + coef_a[2]*y[i-1] + coef_a[3]*y[i-2] + coef_a[4]*y[i-3] + coef_a[5]*y[i-4]
  }
  y
}