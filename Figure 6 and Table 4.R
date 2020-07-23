
remove(list=ls())
set.seed(2434)
simulate<- function(x1,n1,x2,n2,n1ordinary,n2ordinary,beta,mu,B)
{
  coefsdavood<-matrix(0,B,2)
  coefsordinary<-matrix(0,B,2)
  
  for (i in 1:B)
  {
    p1<-1/(1+exp(-beta*(x1-mu)))
    p2<-1/(1+exp(-beta*(x2-mu)))
   y1<-rbinom(n1,1,prob=p1)
    y2<-rbinom(n2,1,prob=p2)
    y<-c(y1,y2)
    x11<-rep(x1,n1)
    x22<-rep(x2,n2)
    x<-c(x11,x22)
    coefsdavood[i,]<-coef(glm(y~x,family=binomial))
    
    p1o<-1/(1+exp(-beta*(1.54-mu)))
    p2o<-1/(1+exp(-beta*(-1.54-mu)))
    y1o<-rbinom(n1ordinary,1,prob=p1o)
    y2o<-rbinom(n2ordinary,1,prob=p2o)
    yo<-c(y1o,y2o)
    x11o<-rep(1.54,n1ordinary)
    x22o<-rep(-1.54,n2ordinary)
    xo<-c(x11o,x22o)
    coefsordinary[i,]<-coef(glm(yo~xo,family=binomial))
  }
  coefs<-cbind(coefsdavood,coefsordinary)
  return(coefs)
  
  #hist(coefs[,2])   
  #hist(coefs[,1]/coefs[,2])
  
  
}
betas<-simulate(-1.1710,5,1.1710,5,5,5,1,0,200000)

mudavood<-(-betas[,1]/betas[,2])
betadavood<-betas[,2]
muordinary<-(-betas[,3]/betas[,4])
betaordinary<-(betas[,4])
da<-cbind(mudavood,betadavood)
or<-cbind(muordinary,betaordinary)
A=rep(1,200000)
d=rep(1,200000)
conter1<-0
conter2<-0
for (i in 1:200000){
  if (abs(or[i,2])> 0.000000000001) {
    A[i]<- (-or[i,1]/or[i,2])
    }
  else{
    
    A[i]<-0
    
    conter1<-conter1+1
    
  }
  if (abs(da[i,2])> 0.00000000001) {
    d[i]<- (-da[i,1]/da[i,2]) }
  else{
    
    d[i]<-0
    conter2<-conter2+1
    
  }
  
}

print(conter1)

print(conter2)
m<-sum(A)/(200000-conter1)
dd<-sum(d)/(200000-conter1)
#variance
B=rep(1:200000)
o=rep(1:200000)
k1<-0
k2<-0
for (i in 1:200000){
  if (A[i]!=0) {
    B[i]<- (A[i]-m)^2 }
  
  else{
    
    B[i]<-0
    k1<-k1+1
    
  }

  if (d[i]!=0) {
    o[i]<- (d[i]-dd)^2 }
  
  else{
    
    o[i]<-0
    k2<-k2+1
    
  }
  
}

Vm1<-sum(B)/(200000-k1-1)
vm2<-sum(o)/(200000-k2-1)
Mseordinary1<-Vm1+(m)^2
mseordinary2<- var(or[,2])+(mean(or[,2])-1)^2
MSE11<-vm2+dd^2
MSE21<- var(da[,2])+(mean(da[,2])-1)^2    
#print(Mseordinary1)
print(mseordinary2)
#print(Vm1)
print(var(or[,2]))
#print(m)
print(mean(or[,2])-1)
##
#print(MSE11)
print(MSE21)
#print(vm2)
print(var(da[,2]))
#print(dd)
print(mean(da[,2])-1)
#############################################
################################
#bias of beta N=50
xdata <- c(8,10,16,20,30,50)
biasCh <- c(5.381023,4.230797,2.094619,1.369414,0.5293091,0.1218339)
biasBh <- c(5.576431,4.440668,2.292605,1.514825,0.5927624,0.1354536)
biasD <-c(5.766561,4.783129,2.591737,1.785584,0.7082051,0.1582593)


# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, biasD, type="o", col="blue", pch=15, lty=1, ylim=c(0,6),xlab="", ylab="" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, biasCh, col="#990000", pch=17)
lines(xdata, biasCh, col="#990000",lty=6)
points(xdata, biasBh, col="black", pch=16)
lines(xdata, biasBh, col="black",lty=2)
legend("topright", legend=c("C-R", "B","H-C-R"),
       col=c("blue","black", "#990000"), pch=c(15,16,17),lty=c(1,2,6), cex=1)

