plot.post <- function(x,main=NULL) {
  mn.x <- round(mean(x),5)
  v.x <- round(var(x),3)
  den <- density(x)
  rng <- c(min(den$y),max(den$y))

  diff <- rng[2]-rng[1]
  main <- ifelse(is.null(main),"Posterior Distribution",
                         paste("Posterior Distribution for",main))
  plot(density(x),col="blue",ylim=c(rng[1],rng[2]+diff*.3),lwd=3,
       main=main)
  legend("topleft",legend=c(paste("Mean =",mn.x),
                            paste("Variance = ",v.x)),bty="n")
  abline(v=mn.x,col="red",lwd=2)
  rng.x <- range(den$x)
  x.diff <- rng.x[2] - rng.x[1]

  opts <- par()
    left <- rng.x[1] + x.diff*2/3
    right <- rng.x[2]
    par(fig = c(grconvertX(c(left,right),from="user",to="ndc"),
                grconvertY(c(rng[2],rng[2]+diff*.3),from="user",to="ndc")),
        mar = c(.1,.1,1,.1), new = TRUE)
    plot(x,type="l",col="gray30",cex.main=.5,axes=F,main="Trace Plot")
    axis(1,cex.axis=.5)
    axis(2,cex.axis=.5)
  par(opts)
}


y <- rgamma(10000,50,.01)
plot.post(y,"y")
