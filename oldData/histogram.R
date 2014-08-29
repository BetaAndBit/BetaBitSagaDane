histogram <- 
function(x, col="grey40", ...) {
  barplot(table(x), las=1, col=col, border="white", xaxt="n", col.axis=col, horiz=T, ...)
  pos <- axis(1,col.axis=col, col.ticks=col,col=col,las=1)
  abline(v=pos,col="white")
}

lastChar <- function(x, n=1) {
  substr(x, nchar(x)-n+1,nchar(x))
}

mosaic <- function(tab, ...) {
  if (ncol(tab) < 12) {
    col <- brewer.pal(ncol(tab),"RdYlGn")
  } else {
    col <- "grey"
  }
  mosaicplot(t(tab), col=col, main="", las=1, ...)
}

effects.plot  <- function(x1,x2,y,fun=mean, ...) {
  lx1 <- levels(x1)
  plot(by(y, list(x1,x2), fun) , c(1,2,1,2),pch=c(19,19,21,21), cex=table(x1,x2)/10, las=1, xlab="", ylab="", yaxt="n", ylim=c(0.5,1+length(lx1)), bty="n")
  axis(2,1:length(lx1),levels(x1), las=1)
  abline(h=1:length(lx1),col="grey",lty=3)
  points(by(y, x1, mean) ,c(1,2),pch="|", cex=table(x1)/20, las=1, xlab="", ylab="", xaxt="n")
  points(by(y, list(x1,x2), mean) , c(1,2,1,2),pch=c(19,19,21,21), cex=table(x1,x2)/10)
  legend("top", rev(unique(x2)), pch=c(19,21), cex=1, pt.cex=2, bty="n", ncol=2)
}


kartezjanskie2biegunowe <- function(punkt, srodek) {
  wynik <- c(sqrt((punkt[1] - srodek[1])^2 + (punkt[2] - srodek[2])^2),
     180*atan2(punkt[1] - srodek[1], punkt[2] - srodek[2])/pi)
  names(wynik) = c("odleglosc", "kat")
  wynik
}

