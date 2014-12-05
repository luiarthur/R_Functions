plot.in.plot <- function(minor.plot,coords="topright",scale=1/3) {
  # coords = x1,y1,x2,y2
  # minor.plot is a function with no parameters that plots the smaller plot
  x1 <- x2 <- y1 <- y2 <- NULL
  if (is.numeric(coords)) {
    x1 <- coords[1]; x2 <- coords[2]
    y1 <- coords[3]; y2 <- coords[4]
  } else if (coords=="topright"){
    s <- par("usr")
    x1 <- s[1] + (s[2]-s[1]) * (1-scale)
    x2 <- s[2] - (s[2]-s[1]) * .01
    y1 <- s[3] + (s[4]-s[3]) * (1-scale)
    y2 <- s[4]
  } else if (coords=="bottomright") {
    s <- par("usr")
    x1 <- s[1] + (s[2]-s[1]) * (1-scale)
    x2 <- s[2] - (s[2]-s[1]) * .01
    y1 <- s[3] + (s[4]-s[3]) * .1
    y2 <- s[3] + (s[4]-s[3]) * (scale)
  }
  opts <- par(no.readonly=T)
    par(fig = c(grconvertX(c(x1,x2),from="user",to="ndc"),
                grconvertY(c(y1,y2),from="user",to="ndc")),
        mar = c(.1,.1,1,.1), new = TRUE)
    minor.plot()
    #axis(1,cex.axis=.5)
    #axis(2,cex.axis=.5)
  par(opts)
}

#x <- rnorm(10000)
#plot(density(x),ylim=c(0,.5))
#
#minor <- function() {
#  plot(x,type="l",axes=F,main="Trace",cex.main=.8) 
#  axis(1,cex.axis=.5)
#  axis(2,cex.axis=.5)
#}
#
##plotinplot(minor,c(1,4,.4,.5))
#plot.in.plot(minor,"topright")
#plot.in.plot(minor,"bottomright")

