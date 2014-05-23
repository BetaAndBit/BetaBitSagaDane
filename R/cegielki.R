cegielki <- function(dane, tytul =  deparse(substitute(dane)), osx = c(19,35), osy=c(0,4)) {
  hist(dane, col="grey", osx[1]:osx[2], xlim=osx, border="white", las=1,xlab="",ylab=tytul, yaxt="n", ylim=osy, main="", xaxt="n")
  abline(h=osy[1]:osy[2], col="white", lwd=3)
  abline(v=osx[1]:osx[2], col="white", lwd=3)
  axis(1,osx[1]:osx[2], col="white", col.ticks="black")
}

