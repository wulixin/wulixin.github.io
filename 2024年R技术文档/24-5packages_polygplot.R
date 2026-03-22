
# install from Github
devtools::install_github("lgnbhl/polyglot")

library(polyglot)
theta <- 2 * pi * (0:5) * 2/5

A <- list(list(x=sin(theta), y=cos(theta)))
B <- polysimplify(A, filltype="nonzero")

opa <- par(mfrow=c(1,2))
plot(c(-1,1),c(-1,1), type="n", axes=FALSE, xlab="", ylab="")
with(A[[1]], segments(x[-6], y[-6], x[-1], y[-1], col="red"))
points(A[[1]], col="red")

with(A[[1]], text(x[1:5], y[1:5], labels=1:5, cex=2))
plot(c(-1,1),c(-1,1), type="n", axes=FALSE, xlab="", ylab="")
polygon(B[[1]], lwd=3, col="green")
with(B[[1]], text(x,y,labels=seq_along(x), cex=2))
par(opa)

A <- list(list(x=1:10, y=c(1:5,5:1)))
B <- list(list(x=c(2,8,8,2),y=c(0,0,10,10)))

plot(c(0,10),c(0,10), type="n", axes=FALSE,
     xlab="", ylab="", main="intersection of closed polygons")
invisible(lapply(A, polygon))
invisible(lapply(B, polygon))
C <- polyclip(A, B)
polygon(C[[1]], lwd=3, col=3)

plot(c(0,10),c(0,10), type="n", axes=FALSE,
     xlab="", ylab="", main="intersection of open polyline and closed polygon")
invisible(lapply(A, polygon))
invisible(lapply(B, polygon))
E <- polyclip(A, B, closed=FALSE)
invisible(lapply(E, lines, col="purple", lwd=5))

