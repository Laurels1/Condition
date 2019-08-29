#-------------------------------------------------------------------------------
#User created functions for plotting condition:
ztrans <- function(x){
  meanx <- mean(x, na.rm = T)
  sdx   <- sd(  x, na.rm = T)
  z     <- (x - meanx) / sdx
  return(z)
}

image.x <- function(x){
  output <- seq(0, 1, by = 1 / x)
  return(output)
}

my.points <- function (x, y, pt.col, ...){
  points(x, y, pch = 16, col = pt.col, cex = 1.5, ...)
}
my.lines  <- function(x, y, ...){
  lines(x, y, col = 'grey', lwd = 4, ...)
}
my.axis   <- function(x, ...){
  axis(x, cex.axis=1.5, lwd=2, ...)
}
my.box    <- function(lwd = 2, ...){
  box(lwd = lwd, ...)
}
my.legend <- function(leg.txt, ...){
  legend('topleft', legend = leg.txt, bty='n', cex=1.8, ...)
}
my.mtext  <- function(x, txt, line, ...){
  mtext(x, text = txt, line = line, cex = 2, outer = T, ...)
}
