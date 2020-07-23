remove(list=ls())
simulate<- function(x1,x2,n1,n2,n1ordinary,n2ordinary,x3,x4,n3,n4,beta,mu,B)
{
  coefsdavood<-matrix(0,B,2)
  coefsordinary<-matrix(0,B,2)
  coefsB<-matrix(0,B,2)
  for (i in 1:B)
  {
    p1<-1/(1+exp(-beta*(x1-mu)))
    p2<-1/(1+exp(-beta*(x2-mu)))
    p3<-1/(1+exp(-beta*(x3-mu)))
    p4<-1/(1+exp(-beta*(x4-mu)))
    y1<-rbinom(n1,1,prob=p1)
    y2<-rbinom(n2,1,prob=p2)
    y3<-rbinom(n3,1,prob=p3)
    y4<-rbinom(n4,1,prob=p4)
    y<-c(y1,y2)
    x11<-rep(x1,n1)
    x22<-rep(x2,n2)
    x<-c(x11,x22)
    coefsdavood[i,]<-coef(glm(y~x,family=binomial))
    
    yB<-c(y3,y4)
    x11B<-rep(x3,n3)
    x22B<-rep(x4,n4)
    xB<-c(x11B,x22B)
    coefsB[i,]<-coef(glm(yB~xB,family=binomial))
    
    
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
  coefs<-cbind(coefsdavood,coefsordinary,coefsB)
  return(coefs)
  
  #hist(coefs[,2])   
  #hist(coefs[,1]/coefs[,2])
  
  
}
betas<-simulate(-1.43875,1.43875,25,25,25,25,-1.480595,1.489239,25,25,1,0,200000)

mudavood<-(-betas[,1]/betas[,2])
betadavood<-betas[,2]
muordinary<-(-betas[,3]/betas[,4])
betaordinary<-(betas[,4])
muB<-(-betas[,5]/betas[,6])
betaB<-(betas[,6])

or<-cbind(muordinary,betaordinary)
da<-cbind(mudavood,betadavood)
BH<-cbind(muB,betaB)
A=rep(1,200000)
z=rep(1,200000)
d=rep(1,200000)
l=rep(1,200000)
t=rep(1,200000)
e=rep(1,200000)
conter1<-0
conter2<-0
conter3<-0
for (i in 1:200000){
  if (abs(or[i,2])> 0.00000000001) {
    A[i]<- (-or[i,1]/or[i,2])
    z[i]<-or[i,2]}
  else{
    
    A[i]<-99
    z[i]<-99
    conter1<-conter1+1
    
  }
  if (abs(da[i,2])> 0.00000000001) {
    d[i]<- (-da[i,1]/da[i,2])
    l[i]<-da[i,2]}
  else{
    
    d[i]<-99
    l[i]<-99
    conter2<-conter2+1
    
  }
   
    if (abs(BH[i,2])> 0.00000000001) {
      t[i]<- (-BH[i,1]/BH[i,2])
      e[i]<-BH[i,2]}
    else{
      
      t[i]<-99
      e[i]<-99
      conter3<-conter3+1
      
    }
    
}
AA<-A[A!=99]
zz<-z[z!=99]
dd<-d[d!=99]
ll<-l[l!=99]
tt<-t[t!=99]
ee<-e[e!=99]
ordinarry<-cbind(AA,zz)
modifiedCH<-cbind(dd,ll)
modifiedBH<-cbind(tt,ee)
AO<-mean(AA)
BO<-mean(zz)
ACH<-mean(dd)
BCH<-mean(ll)
ABH<-mean(tt)
BBH<-mean(ee)
print(AO)
print(BO)
print(ACH)
print(BCH)
print(ABH)
print(BBH)
efficiency<- function(x1,x2,W1,W2,x3,x4,n1,n2,AO,BO,ACH,BCH,ABH,BBH)
{
  p1o<-1/(1+exp(-BO*(1.54-AO)))
  p2o<-1/(1+exp(-BO*(-1.54-AO)))
  p1<-1/(1+exp(-BCH*(x1-ACH)))
  p2<-1/(1+exp(-BCH*(x2-ACH)))
  p3<-1/(1+exp(-BBH*(x3-ABH)))
  p4<-1/(1+exp(-BBH*(x4-ABH)))
  
  
  MD<-matrix(0,2,2)
  MD[1,1]<-(n1*p1o*(1-p1o)*(BO)^2) +(n2*p2o*(1-p2o)*(BO)^2)
  MD[1,2] <- (n1*p1o*(1-p1o)*(-BO*(1.54-AO)))+(n2*p2o*(1-p2o)*(-BO*(-1.54-AO)))
  MD[2,1] <- (n1*p1o*(1-p1o)*(-BO*(1.54-AO)))+(n2*p2o*(1-p2o)*(-BO*(-1.54-AO)))
  MD[2,2] <- (n1*p1o*(1-p1o)*((1.54-AO)^2))+(n2*p2o*(1-p2o)*((-1.54-AO)^2))
  
  MCH<-matrix(0,2,2)
  MCH[1,1]<-((((((W1^2)/(p1^2*(1-p1)^2))+(0.25*((1-2*p1)^2)*((W1^4)/(p1^4*(1-p1)^4)))+(((1-2*p1))*((W1^3)/(p1^3*(1-p1)^3))))/(((((W1^2)/(p1*(1-p1)))+1)^n1)-1))^(-1))*(BCH^2))+((((((W2^2)/(p2^2*(1-p2)^2))+(0.25*((1-2*p2)^2)*((W2^4)/(p2^4*(1-p2)^4)))+(((1-2*p2))*((W2^3)/(p2^3*(1-p2)^3))))/(((((W2^2)/(p2*(1-p2)))+1)^n2)-1))^(-1))*(BCH^2))

  MCH[1,2]<-((((((W1^2)/(p1^2*(1-p1)^2))+(0.25*((1-2*p1)^2)*((W1^4)/(p1^4*(1-p1)^4)))+(((1-2*p1))*((W1^3)/(p1^3*(1-p1)^3))))/(((((W1^2)/(p1*(1-p1)))+1)^n1)-1))^(-1))*(-BCH*(x1-ACH)))+((((((W2^2)/(p2^2*(1-p2)^2))+(0.25*((1-2*p2)^2)*((W2^4)/(p2^4*(1-p2)^4)))+(((1-2*p2))*((W2^3)/(p2^3*(1-p2)^3))))/(((((W2^2)/(p2*(1-p2)))+1)^n2)-1))^(-1))*(-BCH*(x2-ACH)))
  MCH[2,1]<-((((((W1^2)/(p1^2*(1-p1)^2))+(0.25*((1-2*p1)^2)*((W1^4)/(p1^4*(1-p1)^4)))+(((1-2*p1))*((W1^3)/(p1^3*(1-p1)^3))))/(((((W1^2)/(p1*(1-p1)))+1)^n1)-1))^(-1))*(-BCH*(x1-ACH)))+((((((W2^2)/(p2^2*(1-p2)^2))+(0.25*((1-2*p2)^2)*((W2^4)/(p2^4*(1-p2)^4)))+(((1-2*p2))*((W2^3)/(p2^3*(1-p2)^3))))/(((((W2^2)/(p2*(1-p2)))+1)^n2)-1))^(-1))*(-BCH*(x2-ACH)))

  MCH[2,2]<-((((((W1^2)/(p1^2*(1-p1)^2))+(0.25*((1-2*p1)^2)*((W1^4)/(p1^4*(1-p1)^4)))+(((1-2*p1))*((W1^3)/(p1^3*(1-p1)^3))))/(((((W1^2)/(p1*(1-p1)))+1)^n1)-1))^(-1))*((x1-ACH)^2))+((((((W2^2)/(p2^2*(1-p2)^2))+(0.25*((1-2*p2)^2)*((W2^4)/(p2^4*(1-p2)^4)))+(((1-2*p2))*((W2^3)/(p2^3*(1-p2)^3))))/(((((W2^2)/(p2*(1-p2)))+1)^n2)-1))^(-1))*((x2-ACH)^2))
  
  MB<-matrix(0,2,2)
  MB[1,1]<-((((1/(n1*p3*(1-p3)))+(((1-2*p3)^2)/(4*n1*(n1-1)*p3^2*(1-p3)^2)))^(-1))*(BBH^2))+((((1/(n2*p4*(1-p4)))+(((1-2*p4)^2)/(4*n2*(n2-1)*p4^2*(1-p4)^2)))^(-1))*(BBH^2))
  MB[1,2]<-((((1/(n1*p3*(1-p3)))+(((1-2*p3)^2)/(4*n1*(n1-1)*p3^2*(1-p3)^2)))^(-1))*(-BBH*(x3-ABH)))+((((1/(n2*p4*(1-p4)))+(((1-2*p4)^2)/(4*n2*(n2-1)*p4^2*(1-p4)^2)))^(-1))*(-BBH*(x4-ABH)))                             
  MB[2,1]<-((((1/(n1*p3*(1-p3)))+(((1-2*p3)^2)/(4*n1*(n1-1)*p3^2*(1-p3)^2)))^(-1))*(-BBH*(x3-ABH)))+((((1/(n2*p4*(1-p4)))+(((1-2*p4)^2)/(4*n2*(n2-1)*p4^2*(1-p4)^2)))^(-1))*(-BBH*(x4-ABH)))
  MB[2,2]<-MB[1,2]<-((((1/(n1*p3*(1-p3)))+(((1-2*p3)^2)/(4*n1*(n1-1)*p3^2*(1-p3)^2)))^(-1))*((x3-ABH)^2))+((((1/(n2*p4*(1-p4)))+(((1-2*p4)^2)/(4*n2*(n2-1)*p4^2*(1-p4)^2)))^(-1))*((x4-ABH)^2)) 
 MD
 MCH
 MB
  InfD<-det(MD)
  InfCH<-det(MCH)
  InfBH<-det(MB)
  R1=(InfD)/(InfCH)
  R2=(InfBH)/(InfCH)
  R3=(InfD)/(InfBH)
  Eff<-c(R1,R2,R3)
  return(Eff)
}

HH<-efficiency(-1.43875,1.43875,0.0242071,-0.0242071,-1.480595,1.489239,25,25,AO,BO,ACH,BCH,ABH,BBH)
print(HH)

#######################################
##############################################################
#ratio of determinant information matrix
xdata <- c(16,20,30,50,100)
R1 <- c(0.776290,0.853789,0.9691746,1.09798,1.056552)
R2 <- c(0.217562,0.600836,0.9670325,1.04141,1.026150)



# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, R1, type="o", col="blue", pch=15, lty=1, ylim=c(0.1,1.12),xlab="", ylab="" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, R2, col="black", pch=16)
lines(xdata, R2, col="black",lty=2)
legend("bottomright", legend=c("R1", "R2"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)

#############################################
