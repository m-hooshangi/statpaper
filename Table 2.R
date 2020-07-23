library(stats)#ED50=0# for negative x
options(digits = 6)
d<-function(v,beta01,beta1,beta2,epsilon,N) {
  for (i in 1:20000){
   x1<- epsilon*0.5
    
  beta0<- beta2*epsilon*0.5 #alpha
 # beta0<- -beta2*epsilon*0.5   
    x2<-v-i*0.0001
    ll<-function(delta1)
    {(((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))^2*(1-1/(1+exp(-(beta0+beta1*x1))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x1))))^2*delta1^4/(4*1/(1+exp(-(beta0+beta1*x1)))^4*(1-1/(1+exp(-(beta0+beta1*x1))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x1))))*delta1^3/(1/(1+exp(-(beta0+beta1*x1)))^3*(1-1/(1+exp(-(beta0+beta1*x1))))^3))/((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))*(1-1/(1+exp(-(beta0+beta1*x1)))))+1)^(N/2)-1)))}
    f<-optimize(ll,interval = c(0.0001, 0.3),maximum=TRUE)
    delta1<-f[[1]]
    lll<-function(delta2)
    {(((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))^2*(1-1/(1+exp(-(beta0+beta1*x2))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x2))))^2*delta2^4/(4*1/(1+exp(-(beta0+beta1*x2)))^4*(1-1/(1+exp(-(beta0+beta1*x2))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x2))))*delta2^3/(1/(1+exp(-(beta0+beta1*x2)))^3*(1-1/(1+exp(-(beta0+beta1*x2))))^3))/((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))*(1-1/(1+exp(-(beta0+beta1*x2)))))+1)^(N/2)-1)))}
    ff<-optimize(lll,interval = c(0.0001, 0.3),maximum=TRUE)
    delta2<-ff[[1]]
    if (ff[[2]]>f[[2]]) 
      delta<-delta2 else delta<-delta1
    m<-matrix(0,2,2)
    m[1,1]<-(1*((((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))^2*(1-1/(1+exp(-(beta0+beta1*x1))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x1))))^2*delta1^4/(4*1/(1+exp(-(beta0+beta1*x1)))^4*(1-1/(1+exp(-(beta0+beta1*x1))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x1))))*delta1^3/(1/(1+exp(-(beta0+beta1*x1)))^3*(1-1/(1+exp(-(beta0+beta1*x1))))^3))/((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))*(1-1/(1+exp(-(beta0+beta1*x1)))))+1)^(N/2)-1))^(-1))))+(1*((((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))^2*(1-1/(1+exp(-(beta0+beta1*x2))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x2))))^2*delta2^4/(4*1/(1+exp(-(beta0+beta1*x2)))^4*(1-1/(1+exp(-(beta0+beta1*x2))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x2))))*(-delta2)^3/(1/(1+exp(-(beta0+beta1*x2)))^3*(1-1/(1+exp(-(beta0+beta1*x2))))^3))/((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))*(1-1/(1+exp(-(beta0+beta1*x2)))))+1)^(N/2)-1))^(-1))))
    
    m[1,2]<-((x1)*((((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))^2*(1-1/(1+exp(-(beta0+beta1*x1))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x1))))^2*delta1^4/(4*1/(1+exp(-(beta0+beta1*x1)))^4*(1-1/(1+exp(-(beta0+beta1*x1))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x1))))*delta1^3/(1/(1+exp(-(beta0+beta1*x1)))^3*(1-1/(1+exp(-(beta0+beta1*x1))))^3))/((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))*(1-1/(1+exp(-(beta0+beta1*x1)))))+1)^(N/2)-1))^(-1))))+((x2)*((((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))^2*(1-1/(1+exp(-(beta0+beta1*x2))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x2))))^2*delta2^4/(4*1/(1+exp(-(beta0+beta1*x2)))^4*(1-1/(1+exp(-(beta0+beta1*x2))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x2))))*(-delta2)^3/(1/(1+exp(-(beta0+beta1*x2)))^3*(1-1/(1+exp(-(beta0+beta1*x2))))^3))/((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))*(1-1/(1+exp(-(beta0+beta1*x2)))))+1)^(N/2)-1))^(-1))))
    
    m[2,1]<-m[1,2]
    
    m[2,2]<-((x1^2)*((((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))^2*(1-1/(1+exp(-(beta0+beta1*x1))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x1))))^2*delta1^4/(4*1/(1+exp(-(beta0+beta1*x1)))^4*(1-1/(1+exp(-(beta0+beta1*x1))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x1))))*delta1^3/(1/(1+exp(-(beta0+beta1*x1)))^3*(1-1/(1+exp(-(beta0+beta1*x1))))^3))/((delta1^2/(1/(1+exp(-(beta0+beta1*x1)))*(1-1/(1+exp(-(beta0+beta1*x1)))))+1)^(N/2)-1))^(-1))))+((x2^2)*((((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))^2*(1-1/(1+exp(-(beta0+beta1*x2))))^2)+(1-2*1/(1+exp(-(beta0+beta1*x2))))^2*delta2^4/(4*1/(1+exp(-(beta0+beta1*x2)))^4*(1-1/(1+exp(-(beta0+beta1*x2))))^4)+(1-2*1/(1+exp(-(beta0+beta1*x2))))*(-delta2)^3/(1/(1+exp(-(beta0+beta1*x2)))^3*(1-1/(1+exp(-(beta0+beta1*x2))))^3))/((delta2^2/(1/(1+exp(-(beta0+beta1*x2)))*(1-1/(1+exp(-(beta0+beta1*x2)))))+1)^(N/2)-1))^(-1))))
    
    r<- solve(m)
    
    chek<-function(t){
     2*(r[1,1]*(1*(((delta^2/(1/(1+exp(-(beta0+beta1*t)))^2*(1-1/(1+exp(-(beta0+beta1*t))))^2)+(1-2*1/(1+exp(-(beta0+beta1*t))))^2*delta^4/(4*1/(1+exp(-(beta0+beta1*t)))^4*(1-1/(1+exp(-(beta0+beta1*t))))^4)+(1-2*1/(1+exp(-(beta0+beta1*t))))*delta^3/(1/(1+exp(-(beta0+beta1*t)))^3*(1-1/(1+exp(-(beta0+beta1*t))))^3))/((delta^2/(1/(1+exp(-(beta0+beta1*t)))*(1-1/(1+exp(-(beta0+beta1*t)))))+1)^(N/2)-1))))+2*r[1,2]*(t*(((delta^2/(1/(1+exp(-(beta0+beta1*t)))^2*(1-1/(1+exp(-(beta0+beta1*t))))^2)+(1-2*1/(1+exp(-(beta0+beta1*t))))^2*delta^4/(4*1/(1+exp(-(beta0+beta1*t)))^4*(1-1/(1+exp(-(beta0+beta1*t))))^4)+(1-2*1/(1+exp(-(beta0+beta1*t))))*delta^3/(1/(1+exp(-(beta0+beta1*t)))^3*(1-1/(1+exp(-(beta0+beta1*t))))^3))/((delta^2/(1/(1+exp(-(beta0+beta1*t)))*(1-1/(1+exp(-(beta0+beta1*t)))))+1)^(N/2)-1))))+r[2,2]*(((t^2)*(((delta^2/(1/(1+exp(-(beta0+beta1*t)))^2*(1-1/(1+exp(-(beta0+beta1*t))))^2)+(1-2*1/(1+exp(-(beta0+beta1*t))))^2*delta^4/(4*1/(1+exp(-(beta0+beta1*t)))^4*(1-1/(1+exp(-(beta0+beta1*t))))^4)+(1-2*1/(1+exp(-(beta0+beta1*t))))*delta^3/(1/(1+exp(-(beta0+beta1*t)))^3*(1-1/(1+exp(-(beta0+beta1*t))))^3))/((delta^2/(1/(1+exp(-(beta0+beta1*t)))*(1-1/(1+exp(-(beta0+beta1*t)))))+1)^(N/2)-1)))^(-1)))-2 
      )
    }
    dd<-optimize(chek,interval = c(-3,-0.0001),maximum=TRUE)
    print(x2 )
  
   print((dd[[2]]^2))
    print("****************************")
    if ((dd[[2]])+500*dd[[2]]^2<0.000000001)
    {break
    }
  }
  #plot(chek,0,3)
  
  # print(i)
  print("x2=")
  print(x2)
  print("delta=")
  print(delta)
  print(dd[2])
 
}
d(-0.1,0,1,1,0.0002,8) 

