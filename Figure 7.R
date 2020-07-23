D<-function(n1,n2,x1,x2){
  p1<-1/(1+exp(-1*x1))
  p2<-1/(1+exp(-1*x2))
  probability<-1-(1-(p1^n1)-(1-p1)^n1)*(1-(p2^n2)-(1-p2)^n2)
  return(probability)
}
D(50,50,-1.4879,1.4879)
################################
xdata <- c(6,8,10,16,20,30,50,100)
PD <- c(0.8098066,0.7092449,0.6141232,0.4139,0.2661804,0.105622,0.01550464,0.0001211342)
PCh <- c(0.654585,0.525748,0.452262,0.262517,0.182088,0.069489,0.00974448,7.57213e-5)
PBh <-c(0.7110237,0.6040345,0.5132568,0.3361951,0.2127309,0.08302165,0.01210335,9.553868e-05)


# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, PD, type="o", col="blue", pch=15, lty=1, ylim=c(0,0.82),xlab="", ylab="" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, PCh, col="#990000", pch=17)
lines(xdata, PCh, col="#990000",lty=6)
points(xdata, PBh, col="black", pch=16)
lines(xdata, PBh, col="black",lty=2)
legend("topright", legend=c("C-R", "B","H-C-R"),
       col=c("blue","black", "#990000"), pch=c(15,16,17),lty=c(1,2,6), cex=1)

