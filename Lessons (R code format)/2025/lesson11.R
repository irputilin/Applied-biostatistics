library(shipunov)
df <- iris

image(scale(iris[,-5]), axes=F)
axis(2, at=seq(0, 1, length.out=4),
     labels=abbreviate(colnames(iris[,-5])), las=2)

library(MASS)
parcoord(iris[,-5], col=as.numeric(iris[,5]), lwd=2)
legend("top", bty="n", lty=1, lwd=2, col=1:3,
         legend=names(table(iris[, 5])))

Boxplots(iris[,-5], iris[,5], legpos = 'topright')
Linechart(iris[,-5], iris[,5], mad = F)


pairs(iris[, 1:4], pch=21, bg=as.numeric(iris[, 5]))
oldpar <- par(xpd=TRUE)
legend(0, 1.09, horiz=TRUE, legend=levels(iris[, 5]),
       pch=21, pt.bg=1:3, bty='n')
par(oldpar)

library(vcd)
ternaryplot(scale(iris[, 2:4], center=FALSE),
            cex=.3, col=iris[, 5], main="")


library(scatterplot3d)

scatterplot3d(iris[,2:4], color = as.numeric(iris[,5]),
              pch=17, angle = 15, )


library(rgl)
plot3d(iris[,1:3], col=as.numeric(iris[,5]))




x <- iris[,1]
y <- iris[,2]
z <- iris[,3]

groups <- iris$Species
levs <- levels(groups)
group.col <- c("red", "green", "blue")


rgl.spheres(x, y, z, r = 0.2,
            color = group.col[as.numeric(groups)]) 


for (i in 1:length(levs)) {
  group <- levs[i]
  selected <- groups == group
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  ellips <- ellipse3d(cov(cbind(xx,yy,zz)), 
                      centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95) 
  shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE) 
  # show group labels
  texts3d(mean(xx),mean(yy), mean(zz), text = group,
          col= group.col[i], cex = 2)
}


play3d(spin3d(axis = c(0, 0, 1)), duration = 10)
