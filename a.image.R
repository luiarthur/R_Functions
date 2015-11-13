a.image <- function(Q,color=rev(heat.colors(100)),#paste0("gray",100:0),
                    numbers=F,num.cex=1,temperature=1,figs=3,
                    numcolor="black",axis.num=T,fine=100,spec=NULL,...) {

  if (length(color)==1) {
    q <- seq(1,0,len=fine)
    if (color=="blue") {
      color <- rgb((q^temperature+1)/2,(q+3)/4,1)
    } else if (color=="red") {                   
      color <- rgb(1,(q^temperature+1)/2,(q+3)/4)
    } else if (color=="green") {                 
      color <- rgb((q^temperature+1)/2,1,(q+3)/4)
    }
  }

  if (!is.null(spec)) {
    image(t(apply(Q,2,rev)),yaxt="n",xaxt="n",col=color,
          zlim=spec,...)
  } else {
    image(t(apply(Q,2,rev)),yaxt="n",xaxt="n",col=color,...)
  }
  
  ec <- apply(Q,2,sum) 
  er <- apply(Q,1,sum)
  seq1 <- seq(0,1,len=length(ec))
  seq2 <- seq(1,0,len=length(er))

  if (is.logical(axis.num)) {
    if (axis.num) {
      axis(1,tck=0,at=seq1,lab=ec)
      axis(2,tck=0,at=seq2,lab=er,las=2,...)
    }
  } else {
    if (!is.null(axis.num[[1]])) axis(3,lty=0,at=seq1,lab=axis.num[[1]],...) #3
    if (!is.null(axis.num[[2]])) axis(2,lty=0,at=seq2,lab=axis.num[[2]],las=2,...)
  }


  if (numbers) {
    xx <- rep(1:ncol(Q),each=nrow(Q))
    yy <- rep(1:nrow(Q),ncol(Q))
    text(seq1[xx],seq2[yy],c(round(Q,figs)),col=numcolor,font=2,cex=num.cex)
  }
}
