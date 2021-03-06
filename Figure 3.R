options(digits = 5)
d<-function(x,delta,beta,mu,N) {
  
    x1<--x
    delta1<--delta
    ###########negative design space
    m<-matrix(0,2,2)
    m[1,1]<-(beta^2*((((delta^2/(1/(1+exp(-x))^2*(1-1/(1+exp(-x)))^2)+(1-2*1/(1+exp(-x)))^2*delta^4/(4*1/(1+exp(-x))^4*(1-1/(1+exp(-x)))^4)+(1-2*1/(1+exp(-x)))*delta^3/(1/(1+exp(-x))^3*(1-1/(1+exp(-x)))^3))/((delta^2/(1/(1+exp(-x))*(1-1/(1+exp(-x))))+1)^(N/2)-1))^(-1))))+(beta^2*((((delta^2/(1/(1+exp(x))^2*(1-1/(1+exp(x)))^2)+(1-2*1/(1+exp(x)))^2*delta^4/(4*1/(1+exp(x))^4*(1-1/(1+exp(x)))^4)+(1-2*1/(1+exp(x)))*(-delta)^3/(1/(1+exp(x))^3*(1-1/(1+exp(x)))^3))/((delta^2/(1/(1+exp(x))*(1-1/(1+exp(x))))+1)^(N/2)-1))^(-1))))
    
    m[1,2]<-(-beta*((x-mu)*(((delta^2/(1/(1+exp(-x))^2*(1-1/(1+exp(-x)))^2)+(1-2*1/(1+exp(-x)))^2*delta^4/(4*1/(1+exp(-x))^4*(1-1/(1+exp(-x)))^4)+(1-2*1/(1+exp(-x)))*delta^3/(1/(1+exp(-x))^3*(1-1/(1+exp(-x)))^3))/((delta^2/(1/(1+exp(-x))*(1-1/(1+exp(-x))))+1)^(N/2)-1)))^(-1)))+(-beta*((-x-mu)*(((delta^2/(1/(1+exp(x))^2*(1-1/(1+exp(x)))^2)+(1-2*1/(1+exp(x)))^2*delta^4/(4*1/(1+exp(x))^4*(1-1/(1+exp(x)))^4)+(1-2*1/(1+exp(x)))*(-delta)^3/(1/(1+exp(x))^3*(1-1/(1+exp(x)))^3))/((delta^2/(1/(1+exp(x))*(1-1/(1+exp(x))))+1)^(N/2)-1)))^(-1)))
    
    m[2,1]<-m[1,2]
    
    m[2,2]<-(((x-mu)^2*(((delta^2/(1/(1+exp(-x))^2*(1-1/(1+exp(-x)))^2)+(1-2*1/(1+exp(-x)))^2*delta^4/(4*1/(1+exp(-x))^4*(1-1/(1+exp(-x)))^4)+(1-2*1/(1+exp(-x)))*delta^3/(1/(1+exp(-x))^3*(1-1/(1+exp(-x)))^3))/((delta^2/(1/(1+exp(-x))*(1-1/(1+exp(-x))))+1)^(N/2)-1)))^(-1)))+(((-x-mu)^2*(((delta^2/(1/(1+exp(x))^2*(1-1/(1+exp(x)))^2)+(1-2*1/(1+exp(x)))^2*delta^4/(4*1/(1+exp(x))^4*(1-1/(1+exp(x)))^4)+(1-2*1/(1+exp(x)))*(-delta^3)/(1/(1+exp(x))^3*(1-1/(1+exp(x)))^3))/((delta^2/(1/(1+exp(x))*(1-1/(1+exp(x))))+1)^(N/2)-1)))^(-1)))
    
    r<- solve(m)
    
    chek<-function(t){
      r[1,1]*(beta^2*((((delta^2/(1/(1+exp(-t))^2*(1-1/(1+exp(-t)))^2)+(1-2*1/(1+exp(-t)))^2*delta^4/(4*1/(1+exp(-t))^4*(1-1/(1+exp(-t)))^4)+(1-2*1/(1+exp(-t)))*delta^3/(1/(1+exp(-t))^3*(1-1/(1+exp(-t)))^3))/((delta^2/(1/(1+exp(-t))*(1-1/(1+exp(-t))))+1)^(N/2)-1))^(-1))))+2*r[1,2]*(-beta*((t-mu)*(((delta^2/(1/(1+exp(-t))^2*(1-1/(1+exp(-t)))^2)+(1-2*1/(1+exp(-t)))^2*delta^4/(4*1/(1+exp(-t))^4*(1-1/(1+exp(-t)))^4)+(1-2*1/(1+exp(-t)))*delta^3/(1/(1+exp(-t))^3*(1-1/(1+exp(-t)))^3))/((delta^2/(1/(1+exp(-t))*(1-1/(1+exp(-t))))+1)^(N/2)-1)))^(-1)))+r[2,2]*(((t-mu)^2*(((delta^2/(1/(1+exp(-t))^2*(1-1/(1+exp(-t)))^2)+(1-2*1/(1+exp(-t)))^2*delta^4/(4*1/(1+exp(-t))^4*(1-1/(1+exp(-t)))^4)+(1-2*1/(1+exp(-t)))*delta^3/(1/(1+exp(-t))^3*(1-1/(1+exp(-t)))^3))/((delta^2/(1/(1+exp(-t))*(1-1/(1+exp(-t))))+1)^(N/2)-1)))^(-1)))-1
    }
 ###########positive design space
    mm<-matrix(0,2,2)
    mm[1,1]<-(beta^2*((((delta1^2/(1/(1+exp(-x1))^2*(1-1/(1+exp(-x1)))^2)+(1-2*1/(1+exp(-x1)))^2*delta1^4/(4*1/(1+exp(-x1))^4*(1-1/(1+exp(-x1)))^4)+(1-2*1/(1+exp(-x1)))*delta1^3/(1/(1+exp(-x1))^3*(1-1/(1+exp(-x1)))^3))/((delta1^2/(1/(1+exp(-x1))*(1-1/(1+exp(-x1))))+1)^(N/2)-1))^(-1))))+(beta^2*((((delta1^2/(1/(1+exp(x1))^2*(1-1/(1+exp(x1)))^2)+(1-2*1/(1+exp(x1)))^2*delta1^4/(4*1/(1+exp(x1))^4*(1-1/(1+exp(x1)))^4)+(1-2*1/(1+exp(x1)))*(-delta1)^3/(1/(1+exp(x1))^3*(1-1/(1+exp(x1)))^3))/((delta1^2/(1/(1+exp(x1))*(1-1/(1+exp(x1))))+1)^(N/2)-1))^(-1))))
    
    mm[1,2]<-(-beta*((x1-mu)*(((delta1^2/(1/(1+exp(-x1))^2*(1-1/(1+exp(-x1)))^2)+(1-2*1/(1+exp(-x1)))^2*delta1^4/(4*1/(1+exp(-x1))^4*(1-1/(1+exp(-x1)))^4)+(1-2*1/(1+exp(-x1)))*delta1^3/(1/(1+exp(-x1))^3*(1-1/(1+exp(-x1)))^3))/((delta1^2/(1/(1+exp(-x1))*(1-1/(1+exp(-x1))))+1)^(N/2)-1)))^(-1)))+(-beta*((-x1-mu)*(((delta1^2/(1/(1+exp(x1))^2*(1-1/(1+exp(x1)))^2)+(1-2*1/(1+exp(x1)))^2*delta1^4/(4*1/(1+exp(x1))^4*(1-1/(1+exp(x1)))^4)+(1-2*1/(1+exp(x1)))*(-delta1)^3/(1/(1+exp(x1))^3*(1-1/(1+exp(x1)))^3))/((delta1^2/(1/(1+exp(x1))*(1-1/(1+exp(x1))))+1)^(N/2)-1)))^(-1)))
    
    mm[2,1]<-mm[1,2]
    
    mm[2,2]<-(((x1-mu)^2*(((delta1^2/(1/(1+exp(-x1))^2*(1-1/(1+exp(-x1)))^2)+(1-2*1/(1+exp(-x1)))^2*delta1^4/(4*1/(1+exp(-x1))^4*(1-1/(1+exp(-x1)))^4)+(1-2*1/(1+exp(-x1)))*delta1^3/(1/(1+exp(-x1))^3*(1-1/(1+exp(-x1)))^3))/((delta1^2/(1/(1+exp(-x1))*(1-1/(1+exp(-x1))))+1)^(N/2)-1)))^(-1)))+(((-x1-mu)^2*(((delta1^2/(1/(1+exp(x1))^2*(1-1/(1+exp(x1)))^2)+(1-2*1/(1+exp(x1)))^2*delta1^4/(4*1/(1+exp(x1))^4*(1-1/(1+exp(x1)))^4)+(1-2*1/(1+exp(x1)))*(-delta1^3)/(1/(1+exp(x1))^3*(1-1/(1+exp(x1)))^3))/((delta1^2/(1/(1+exp(x1))*(1-1/(1+exp(x1))))+1)^(N/2)-1)))^(-1)))
    
    rr<- solve(mm)
    
    chek1<-function(t){
      rr[1,1]*(beta^2*((((delta1^2/(1/(1+exp(t))^2*(1-1/(1+exp(t)))^2)+(1-2*1/(1+exp(t)))^2*delta1^4/(4*1/(1+exp(t))^4*(1-1/(1+exp(t)))^4)+(1-2*1/(1+exp(t)))*(-delta1)^3/(1/(1+exp(t))^3*(1-1/(1+exp(t)))^3))/((delta1^2/(1/(1+exp(t))*(1-1/(1+exp(t))))+1)^(N/2)-1))^(-1))))+2*rr[1,2]*(-beta*((-t-mu)*(((delta1^2/(1/(1+exp(t))^2*(1-1/(1+exp(t)))^2)+(1-2*1/(1+exp(t)))^2*delta1^4/(4*1/(1+exp(t))^4*(1-1/(1+exp(t)))^4)+(1-2*1/(1+exp(t)))*(-delta1)^3/(1/(1+exp(t))^3*(1-1/(1+exp(t)))^3))/((delta1^2/(1/(1+exp(t))*(1-1/(1+exp(t))))+1)^(N/2)-1)))^(-1)))+rr[2,2]*(((-t-mu)^2*(((delta1^2/(1/(1+exp(t))^2*(1-1/(1+exp(t)))^2)+(1-2*1/(1+exp(t)))^2*delta1^4/(4*1/(1+exp(t))^4*(1-1/(1+exp(t)))^4)+(1-2*1/(1+exp(t)))*(-delta1)^3/(1/(1+exp(t))^3*(1-1/(1+exp(t)))^3))/((delta1^2/(1/(1+exp(t))*(1-1/(1+exp(t))))+1)^(N/2)-1)))^(-1)))-1
    }
    

      plot(chek,-3,-0.0001,xlab="t", ylab=" ")
      plot(chek1,0.0001,3,xlab="t", ylab=" ")
     ########################################################################
    ###################### plot of non-optimality
      t<-seq(-3,3,by=0.2)
      
      plot (t, chek(t), type="l", col="Red", xlab="", ylab=" ",ylim=c(-0.3, 0.3))
      abline(h=0)
    ###################################### plot of an optimal interval n=120 
    b<-seq(-1.7,-1.4,by=0.1)
      plot (b, chek(b), type="l", col="Red", xlab="", ylab=" ",ylim=c(-0.3, 0.3))
      
}
  
d(-1.49635,0.010461,1,0,120)
