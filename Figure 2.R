CR<-c(1000,1:9)
B<-c(1000,1:9)
HCR<-c(1000,1:9)
k<-function(p){
for(i in 1:10){
  N<-i*5
  
l<-function(delta)
  {
  ((delta/(p*(1-p)))+((delta^2)*(1-2*p))/(2*(p^2)*(1-p)^2))^2/(((((delta^2)/(p*(1-p)))+1)^N)-1)
}
f<-optimize(l,interval = c(0.0001,0.3),maximum=TRUE)
HCR[i]<-f[[2]]


ll<- 1/(N*p*(1-p))

CR[i]<-ll
lll<- 1/(N*p*(1-p))+((1-2*p)^2)/(4*N*(N-1)*p^2*(1-p^2))

B[i]<-lll
print("****************************")
}
  print(CR)
  print(HCR)
  print(B)
  ########################
  xdata <- c(5,10,15,20,25,30,35,40,45,50)
  lowerD <- CR
  lowerBh <- B
  lowerHCR<-HCR
  
  
  # plot the first curve by calling plot() function
  # First curve is plotted
  plot(xdata, lowerD, type="o", col="blue", pch=15, lty=1, ylim=c(0,0.9),xlab="", ylab="" )
  
  # Add second curve to the same plot by calling points() and lines()
  # Use symbol '*' for points.
  points(xdata,lowerBh, col="Black", pch=16)
  lines(xdata, lowerBh, col="Black",lty=2)
  points(xdata, lowerHCR, col="Red", pch=17)
  lines(xdata, lowerHCR, col="#990000",lty=6)
  legend("topright", legend=c("C-R", "B","H-C-R"),
         col=c("blue","black", "#990000"), pch=c(15,16,17),lty=c(1,2,6), cex=1)
  
  
}
k(0.4)