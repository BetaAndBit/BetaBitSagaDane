cegielki <- function(dane, tytul =  deparse(substitute(dane)), osx = c(19,35), osy=c(0,4)) {
  hist(dane, col="grey", osx[1]:osx[2], xlim=osx, border="white", las=1,xlab="",ylab=tytul, yaxt="n", ylim=osy, main="", xaxt="n")
  abline(h=osy[1]:osy[2], col="white", lwd=3)
  abline(v=osx[1]:osx[2], col="white", lwd=3)
  axis(1,osx[1]:osx[2], col="white", col.ticks="black")
}

mniejszywa <- function(dane, tytul =  deparse(substitute(dane)), osx = c(19,35), osy=c(0,4)) {
  plot(ecdf(dane, las=1, ylab=tytul, xlab="", main="", xlim=osx,  bty="n", lwd=3, yaxt="n", xaxt="n")
  axis(1, osx[1]:osx[2], col="white", col.ticks="black")
  axis(2, seq(0,1,0.2), paste0(seq(0,100,20),"%"), col="white", col.ticks="white", las=1)
  abline(h=seq(0.2,0.8,0.2), lty=2, col="grey")
}


