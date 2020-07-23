options(repos = c(CRAN = "http://cran.rstudio.com"))
install.packages("plot3D")
library(plot3D)

n1<-seq(4,50,length=20)
p1<-seq(0.001,0.49,length=20)
z<-matrix(100,20,20)
for (i in 1:20) { 
  for (j in 1:20) { 
 
   n<-n1[i]
  p<-p1[j]
ff<-function (n,p){
 a<-function (delta){((delta^2)/(p^2*(1-p)^2))/(((delta^2)/(p*(1-p))+1)^n-1)+((1-2*p)^2*(delta^4)/(4*p^4*(1-p)^4))/(((delta^2)/(p*(1-p))+1)^n-1)+((1-2*p)*(delta^3)/(p^3*(1-p)^3))/(((delta^2)/(p*(1-p))+1)^(n/2)-1)}
 
 f<-optimize(a,interval =c(0.001,200) ,maximum=TRUE)
 d<-f[[1]]
return(d)}
z[i,j]<-ff(n,p)

#print(z[i,j])
}
}
print(n1)
print(p1)
print(z)

persp3D(p1,n1, z,theta=35, phi=15, axes=TRUE,scale=TRUE, box=TRUE, nticks=5, 
        
        ticktype="detailed",xlab="p", ylab="n", zlab="", 
        
        main="")

