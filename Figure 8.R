remove(list=ls())

simulate<- function(x11,x12,x21,x22,x31,x32,n1,n2,n3,z11,z12,z21,z22,z31,z32,n1ordinary,n2ordinary,n3ordinary,beta0,beta1,beta2,B)
{
  coefsH<-matrix(0,B,3)
  coefsordinary<-matrix(0,B,3)
  
  for (i in 1:B)
  {
    p1<-1/(1+exp(-(beta0+beta1*x11+beta2*x12)))
    p2<-1/(1+exp(-(beta0+beta1*x21+beta2*x22)))
    p3<-1/(1+exp(-(beta0+beta1*x31+beta2*x32)))
    
    y1<-rbinom(n1,1,prob=p1)
    y2<-rbinom(n2,1,prob=p2)
    y3<-rbinom(n3,1,prob=p3)
    
    y<-c(y1,y2,y3)
    x111<-rep(x11,n1)
    x121<-rep(x21,n2)
    x131<-rep(x31,n3)
    x1<-c(x111,x121,x131)
    x211<-rep(x12,n1)
    x221<-rep(x22,n2)
    x321<-rep(x32,n3)
    x2<-c(x211,x221,x321)
    
    coefsH[i,]<-coef(glm(y~x1+x2,family=binomial))
    
  
    p4<-1/(1+exp(-(beta0+beta1*z11+beta2*z12)))
    p5<-1/(1+exp(-(beta0+beta1*z21+beta2*z22)))
    p6<-1/(1+exp(-(beta0+beta1*z31+beta2*z32)))
    
    y4<-rbinom(n1ordinary,1,prob=p4)
    y5<-rbinom(n2ordinary,1,prob=p5)
    y6<-rbinom(n3ordinary,1,prob=p6)
    
    yo<-c(y4,y5,y6)
    z111<-rep(z11,n1ordinary)
    z121<-rep(z21,n2ordinary)
    z131<-rep(z31,n3ordinary)
    z1<-c(z111,z121,z131)
    z211<-rep(z12,n1ordinary)
    z221<-rep(z22,n2ordinary)
    z321<-rep(z32,n3ordinary)
    z2<-c(z211,z221,z321)
     coefsordinary[i,]<-coef(glm(yo~z1+z2,family=binomial))
  }
  coefs<-cbind(coefsH,coefsordinary)
  return(coefs)
  
  
}
betas<-simulate(1.0468,0,0,1.0468,0,0,8,8,8,2.399,0,0,2.399,0,0,8,8,8,0,1,1,200000)

beta0H<-betas[,1]
beta1H<-betas[,2]
beta2H<-betas[,3]
beta0ordinary<-betas[,4]
beta1ordinary<-betas[,5]
beta2ordinary<-betas[,6]
B0H<-mean(beta0H)
B1H<-mean(beta1H)
B2H<-mean(beta2H)
B0o<-mean(beta0ordinary)
B1o<-mean(beta1ordinary)
B2o<-mean(beta2ordinary)

BB0H<-mean(beta0H)-0
BB1H<-mean(beta1H)-1
BB2H<-mean(beta2H)-1
BB0o<-mean(beta0ordinary)-0
BB1o<-mean(beta1ordinary)-1
BB2o<-mean(beta2ordinary)-1

v0H<-var(beta0H)
v1H<-var(beta1H)
v2H<-var(beta2H)
v0o<-var(beta0ordinary)
v1o<-var(beta1ordinary)
v2o<-var(beta2ordinary)

efficiency<- function(x11,x12,x21,x22,x31,x32,n1,n2,n3,z11,z12,z21,z22,z31,z32,n1ordinary,n2ordinary,n3ordinary,B0H,B1H,B2H,B0o,B1o,B2o)
{
  
  p1H<-1/(1+exp(-(B0H+B1H*x11+B2H*x12)))
  p2H<-1/(1+exp(-(B0H+B1H*x21+B2H*x22)))
  p3H<-1/(1+exp(-(B0H+B1H*x31+B2H*x32)))
  #print( p1H)
  #print( p2H)
  #print( p3H)
 
  p1o<-1/(1+exp(-(B0o+B1o*z11+B2o*z12)))
  p2o<-1/(1+exp(-(B0o+B1o*z21+B2o*z22)))
  p3o<-1/(1+exp(-(B0o+B1o*z31+B2o*z32)))
  
 
  
  ll<-function(delta1)
  {((((((delta1^2)/(p1H^2*(1-p1H)^2))+(0.25*((1-2*p1H)^2)*((delta1^4)/(p1H^4*(1-p1H)^4)))+(((1-2*p1H))*((delta1^3)/(p1H^3*(1-p1H)^3))))/(((((delta1^2)/(p1H*(1-p1H)))+1)^n1)-1))^(-1))*(1))}
  f<-optimize(ll,interval = c(-0.5,-0.0001),maximum=TRUE)
  W1<-f[[1]]
  #print(W1)
  lll<-function(delta2)
  {((((((delta2^2)/(p2H^2*(1-p2H)^2))+(0.25*((1-2*p2H)^2)*((delta2^4)/(p2H^4*(1-p2H)^4)))+(((1-2*p2H))*((delta2^3)/(p2H^3*(1-p2H)^3))))/(((((delta2^2)/(p2H*(1-p2H)))+1)^n2)-1))^(-1))*(1))}
  ff<-optimize(lll,interval = c(-0.5,-0.0001),maximum=TRUE)
  W2<-ff[[1]]
  #print(W2)
  llll<-function(delta3)
  {((((((delta3^2)/(p3H^2*(1-p3H)^2))+(0.25*((1-2*p3H)^2)*((delta3^4)/(p3H^4*(1-p3H)^4)))+(((1-2*p3H))*((delta3^3)/(p3H^3*(1-p3H)^3))))/(((((delta3^2)/(p3H*(1-p3H)))+1)^n3)-1))^(-1))*(1))}
  fff<-optimize(llll,interval = c(-0.5,-0.0001),maximum=TRUE)
  W3<-fff[[1]]
  #print(W3)
   Mo<-matrix(0,3,3)
  Mo[1,1]<-(n1ordinary*p1o*(1-p1o)*1) +(n2ordinary*p2o*(1-p2o)*1)+(n3ordinary*p3o*(1-p3o)*1)
  Mo[1,2] <- (n1ordinary*p1o*(1-p1o)*z11) +(n2ordinary*p2o*(1-p2o)*z21)+(n3ordinary*p3o*(1-p3o)*z31)
  Mo[1,3] <- (n1ordinary*p1o*(1-p1o)*z12) +(n2ordinary*p2o*(1-p2o)*z22)+(n3ordinary*p3o*(1-p3o)*z32)
  Mo[2,2] <- (n1ordinary*p1o*(1-p1o)*z11*z11) +(n2ordinary*p2o*(1-p2o)*z21*z21)+(n3ordinary*p3o*(1-p3o)*z31*z31)
  Mo[2,3] <- (n1ordinary*p1o*(1-p1o)*z11*z12) +(n2ordinary*p2o*(1-p2o)*z21*z22)+(n3ordinary*p3o*(1-p3o)*z31*z32)
  Mo[3,3] <- (n1ordinary*p1o*(1-p1o)*z12*z12) +(n2ordinary*p2o*(1-p2o)*z22*z22)+(n3ordinary*p3o*(1-p3o)*z32*z32)
  Mo[2,1] <- Mo[1,2]
  Mo[3,1] <- Mo[1,3]
  Mo[3,2] <- Mo[2,3]
  MH<-matrix(0,3,3)
  MH[1,1]<-((((((W1^2)/(p1H^2*(1-p1H)^2))+(0.25*((1-2*p1H)^2)*((W1^4)/(p1H^4*(1-p1H)^4)))+(((1-2*p1H))*((W1^3)/(p1H^3*(1-p1H)^3))))/(((((W1^2)/(p1H*(1-p1H)))+1)^n1)-1))^(-1))*(1))+((((((W2^2)/(p2H^2*(1-p2H)^2))+(0.25*((1-2*p2H)^2)*((W2^4)/(p2H^4*(1-p2H)^4)))+(((1-2*p2H))*((W2^3)/(p2H^3*(1-p2H)^3))))/(((((W2^2)/(p2H*(1-p2H)))+1)^n2)-1))^(-1))*(1))+((((((W3^2)/(p3H^2*(1-p3H)^2))+(0.25*((1-2*p3H)^2)*((W3^4)/(p3H^4*(1-p3H)^4)))+(((1-2*p3H))*((W3^3)/(p3H^3*(1-p3H)^3))))/(((((W3^2)/(p3H*(1-p3H)))+1)^n3)-1))^(-1))*(1))
  MH[1,2]<-((((((W1^2)/(p1H^2*(1-p1H)^2))+(0.25*((1-2*p1H)^2)*((W1^4)/(p1H^4*(1-p1H)^4)))+(((1-2*p1H))*((W1^3)/(p1H^3*(1-p1H)^3))))/(((((W1^2)/(p1H*(1-p1H)))+1)^n1)-1))^(-1))*(x11))+((((((W2^2)/(p2H^2*(1-p2H)^2))+(0.25*((1-2*p2H)^2)*((W2^4)/(p2H^4*(1-p2H)^4)))+(((1-2*p2H))*((W2^3)/(p2H^3*(1-p2H)^3))))/(((((W2^2)/(p2H*(1-p2H)))+1)^n2)-1))^(-1))*(x21))+((((((W3^2)/(p3H^2*(1-p3H)^2))+(0.25*((1-2*p3H)^2)*((W3^4)/(p3H^4*(1-p3H)^4)))+(((1-2*p3H))*((W3^3)/(p3H^3*(1-p3H)^3))))/(((((W3^2)/(p3H*(1-p3H)))+1)^n3)-1))^(-1))*(x31))
  MH[1,3]<-((((((W1^2)/(p1H^2*(1-p1H)^2))+(0.25*((1-2*p1H)^2)*((W1^4)/(p1H^4*(1-p1H)^4)))+(((1-2*p1H))*((W1^3)/(p1H^3*(1-p1H)^3))))/(((((W1^2)/(p1H*(1-p1H)))+1)^n1)-1))^(-1))*(x12))+((((((W2^2)/(p2H^2*(1-p2H)^2))+(0.25*((1-2*p2H)^2)*((W2^4)/(p2H^4*(1-p2H)^4)))+(((1-2*p2H))*((W2^3)/(p2H^3*(1-p2H)^3))))/(((((W2^2)/(p2H*(1-p2H)))+1)^n2)-1))^(-1))*(x22))+((((((W3^2)/(p3H^2*(1-p3H)^2))+(0.25*((1-2*p3H)^2)*((W3^4)/(p3H^4*(1-p3H)^4)))+(((1-2*p3H))*((W3^3)/(p3H^3*(1-p3H)^3))))/(((((W3^2)/(p3H*(1-p3H)))+1)^n3)-1))^(-1))*(x32))
  MH[2,2]<-((((((W1^2)/(p1H^2*(1-p1H)^2))+(0.25*((1-2*p1H)^2)*((W1^4)/(p1H^4*(1-p1H)^4)))+(((1-2*p1H))*((W1^3)/(p1H^3*(1-p1H)^3))))/(((((W1^2)/(p1H*(1-p1H)))+1)^n1)-1))^(-1))*(x11*x11))+((((((W2^2)/(p2H^2*(1-p2H)^2))+(0.25*((1-2*p2H)^2)*((W2^4)/(p2H^4*(1-p2H)^4)))+(((1-2*p2H))*((W2^3)/(p2H^3*(1-p2H)^3))))/(((((W2^2)/(p2H*(1-p2H)))+1)^n2)-1))^(-1))*(x21*x21))+((((((W3^2)/(p3H^2*(1-p3H)^2))+(0.25*((1-2*p3H)^2)*((W3^4)/(p3H^4*(1-p3H)^4)))+(((1-2*p3H))*((W3^3)/(p3H^3*(1-p3H)^3))))/(((((W3^2)/(p3H*(1-p3H)))+1)^n3)-1))^(-1))*(x31*x31))
  MH[2,3]<-((((((W1^2)/(p1H^2*(1-p1H)^2))+(0.25*((1-2*p1H)^2)*((W1^4)/(p1H^4*(1-p1H)^4)))+(((1-2*p1H))*((W1^3)/(p1H^3*(1-p1H)^3))))/(((((W1^2)/(p1H*(1-p1H)))+1)^n1)-1))^(-1))*(x11*x12))+((((((W2^2)/(p2H^2*(1-p2H)^2))+(0.25*((1-2*p2H)^2)*((W2^4)/(p2H^4*(1-p2H)^4)))+(((1-2*p2H))*((W2^3)/(p2H^3*(1-p2H)^3))))/(((((W2^2)/(p2H*(1-p2H)))+1)^n2)-1))^(-1))*(x21*x22))+((((((W3^2)/(p3H^2*(1-p3H)^2))+(0.25*((1-2*p3H)^2)*((W3^4)/(p3H^4*(1-p3H)^4)))+(((1-2*p3H))*((W3^3)/(p3H^3*(1-p3H)^3))))/(((((W3^2)/(p3H*(1-p3H)))+1)^n3)-1))^(-1))*(x31*x32))
  MH[3,3]<-((((((W1^2)/(p1H^2*(1-p1H)^2))+(0.25*((1-2*p1H)^2)*((W1^4)/(p1H^4*(1-p1H)^4)))+(((1-2*p1H))*((W1^3)/(p1H^3*(1-p1H)^3))))/(((((W1^2)/(p1H*(1-p1H)))+1)^n1)-1))^(-1))*(x12*x12))+((((((W2^2)/(p2H^2*(1-p2H)^2))+(0.25*((1-2*p2H)^2)*((W2^4)/(p2H^4*(1-p2H)^4)))+(((1-2*p2H))*((W2^3)/(p2H^3*(1-p2H)^3))))/(((((W2^2)/(p2H*(1-p2H)))+1)^n2)-1))^(-1))*(x22*x22))+((((((W3^2)/(p3H^2*(1-p3H)^2))+(0.25*((1-2*p3H)^2)*((W3^4)/(p3H^4*(1-p3H)^4)))+(((1-2*p3H))*((W3^3)/(p3H^3*(1-p3H)^3))))/(((((W3^2)/(p3H*(1-p3H)))+1)^n3)-1))^(-1))*(x32*x32))
  MH[2,1] <- MH[1,2]
  MH[3,1] <- MH[1,3]
  MH[3,2] <- MH[2,3]
  
  MH
  Mo
  InfO<-(det(Mo))
  InfCH<-(det(MH))
  
  R=(InfO)/(InfCH)
  
  print(BB0H)
  print(BB1H)
  print(BB2H)
  print(BB0o)
  print(BB1o)
  print(BB2o)
  print("*****")
  print(v0H)
  print(v1H)
  print(v2H)
  print(v0o)
  print(v1o)
  print(v2o)
  print("*****")
  
  return(R)
}

HH<-efficiency(1.0468,0,0,1.0468,0,0,8,8,8,2.399,0,0,2.399,0,0,8,8,8,B0H,B1H,B2H,B0o,B1o,B2o)
print(HH)

#######################################
##############################################################
#ratio of determinant information matrix
xdata <- c(12,24,30,60,120,180,300)
R<- c(8.2382e-10,1.60355e-13,2.21217e-14,1.481485e-20,1.798043e-40,5.410753e-63,4.365932e-109)
plot(xdata, R, type="o", col="black", pch=15, lty=1, ylim=c(0,9e-10),xlab="N", ylab="" )
################
#ratio of determinant information matrix
xdata <- c(24,30,60,120,180,300)
R<- c(1.60355e-13,2.21217e-14,1.481485e-20,1.798043e-40,5.410753e-63,4.365932e-109)
plot(xdata, R, type="o", col="black", pch=15, lty=1, ylim=c(0,2e-13),xlab="N", ylab="" )

###########
#Bias beta0
xdata <- c(12,24,30,60,120,180,300)
BH0<- c(0.0167013,0.00152793,0.00306439,5.104571e-06,0.000458078,0.001544596,0.0002269778)
BSc0 <- c(0.00458317,0.00559767,0.00427927,0.0002241411,0.0003313133,0.0001289338,0.0001841029)



# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, BH0, type="o", col="blue", pch=15, lty=1, ylim=c(0,0.02),xlab="", ylab="" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, BSc0, col="black", pch=16)
lines(xdata, BSc0, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)
#############################################
################
#variance beta0
xdata <- c(12,24,30,60,120,180,300)
vH0<- c(51.2115, 3.68489,1.27528,0.2249762,0.1056813,0.06913564,0.04101104)
vSc0 <- c(64.5495,4.33578,1.4666,0.2232364,0.1046071,0.06858182,0.0407615)



# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, vH0, type="o", col="blue", pch=15, lty=1, ylim=c(0,65),xlab="", ylab=" " )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, vSc0, col="black", pch=16)
lines(xdata, vSc0, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "s-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)
#############################################
################
#MsE beta0 for n=24
xdata <- c(24,30,60,120,180,300)
MSEH0<- c( 3.684892,1.275289,0.2249762,0.1056815,0.069138025,0.04101109)
MSEs0 <- c(4.33581,1.4666,0.22323645,0.1046072,0.068581836,0.0407615)
plot(xdata, MSEH0, type="o", col="blue", pch=15, lty=1, ylim=c(0,4.5),xlab="", ylab=" " )
points(xdata, MSEs0, col="black", pch=16)
lines(xdata, MSEs0, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)

#############################################
################
#Bias beta1
xdata <- c(12,24,30,60,120,180,300)
BH1<- c(4.85312,1.56452,1.01269,0.1861296,0.04190866,0.02509925,0.01454825)
BSc1 <- c(5.19283,3.54623,2.93651,1.247266,0.2607228,0.07924455,0.02695694)
plot(xdata, BH1, type="o", col="blue", pch=15, lty=1, ylim=c(0,5.5),xlab="", ylab="" )
points(xdata, BSc1, col="black", pch=16)
lines(xdata, BSc1, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)
#############################################

################
#variance beta1
xdata <- c(12,24,30,60,120,180,300)
vH1<- c(251.942,29.6432,16.4457,1.98831,0.1738596,0.08925503,0.04996275)
vSc1 <- c(25.4182,15.5891,14.1854,7.753845,1.603573,0.3347757,0.04257787)
plot(xdata, vH1, type="o", col="blue", pch=15, lty=1, ylim=c(0,256),xlab="", ylab=" " )
points(xdata, vSc1, col="black", pch=16)
lines(xdata, vSc1, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)


#############################################
################
#MsE beta1 for n=24
xdata <- c(24,30,60,120,180,300)
MSEH1<- c(32.0909,17.4712,2.02295,0.175615,0.08988,0.050174)
MSEs1 <- c(28.1648,22.8084,9.3091,1.67154,0.34105,0.043304)
plot(xdata, MSEH1, type="o", col="blue", pch=15, lty=1, ylim=c(0,32.2),xlab="", ylab=" " )
points(xdata, MSEs1, col="black", pch=16)
lines(xdata, MSEs1, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)




#############################################
################
#Bias beta2
xdata <- c(12,24,30,60,120,180,300)
BH2<- c(4.81955,1.58424,1.013,0.1840529,0.04305607,0.02550454,0.01551741)
BSc2 <- c(5.19309,3.52682,2.94774,1.232505,0.2630648,0.08020433,0.02632539)
plot(xdata, BH2, type="o", col="blue", pch=15, lty=1, ylim=c(0,5.5),xlab="", ylab="" )
points(xdata, BSc2, col="black", pch=16)
lines(xdata, BSc2, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)
#############################################

################
#variance beta2
xdata <- c(12,24,30,60,120,180,300)
vH2<- c(251.586,29.9761,16.4579,1.943966,0.1732381,0.09233106,0.05010382)
vSc2 <- c(25.4917,15.592,14.1874,7.692278,1.606834,0.3439565,0.04284458)
plot(xdata, vH2, type="o", col="blue", pch=15, lty=1, ylim=c(0,256),xlab="", ylab="" )
points(xdata, vSc2, col="black", pch=16)
lines(xdata, vSc2, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)


#############################################
################
#MsE beta2 for n=24
xdata <- c(24,30,60,120,180,300)
MSEH2<- c(32.48591,17.4840,1.9778,0.17509,0.09298,0.5034)
MSEs2 <- c(28.0304,22.8765,9.21134,1.67754,0.35038,0.043537)
plot(xdata, MSEH2, type="o", col="blue", pch=15, lty=1, ylim=c(0,32.6),xlab="", ylab=" " )
points(xdata, MSEs2, col="black", pch=16)
lines(xdata, MSEs2, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)



##############################################variance beta0 nfrom 24
xdata <- c(24,30,60,120,180,300)
vH0<- c(3.68489,1.27528,0.2249762,0.1056813,0.06913564,0.04101104)
vSc0 <- c(4.33578,1.4666,0.2232364,0.1046071,0.06858182,0.0407615)



# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, vH0, type="o", col="blue", pch=15, lty=1, ylim=c(0,4.5),xlab="", ylab=" " )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, vSc0, col="black", pch=16)
lines(xdata, vSc0, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "s-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)

#############################################
#variance beta1 from n=24
xdata <- c(24,30,60,120,180,300)
vH1<- c(29.6432,16.4457,1.98831,0.1738596,0.08925503,0.04996275)
vSc1 <- c(15.5891,14.1854,7.753845,1.603573,0.3347757,0.04257787)



# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, vH1, type="o", col="blue", pch=15, lty=1, ylim=c(0,30),xlab="", ylab=" " )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, vSc1, col="black", pch=16)
lines(xdata, vSc1, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)


#############################################
################
#variance beta2 from n=24
xdata <- c(24,30,60,120,180,300)
vH2<- c(29.9761,16.4579,1.943966,0.1732381,0.09233106,0.05010382)
vSc2 <- c(15.592,14.1874,7.692278,1.606834,0.3439565,0.04284458)



# plot the first curve by calling plot() function
# First curve is plotted
plot(xdata, vH2, type="o", col="blue", pch=15, lty=1, ylim=c(0,30),xlab="", ylab="" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(xdata, vSc2, col="black", pch=16)
lines(xdata, vSc2, col="black",lty=2)
legend("bottomright", legend=c("H-C-R", "S-S"),
       col=c("blue","black"), lty=1:2,pch=c(15,16), cex=1)

#############################################

