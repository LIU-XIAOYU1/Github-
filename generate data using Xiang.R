###### generate data using Prof. Xiang #####

#parameter setting ##
n<-200
a1<-1
a2<-0.5
b1<-0.5


generate<-function(n,intercept,a1,a2,b1,b2,p,v)
{
x1<-rbinom(n,1,0.5)
lin<-cbind(1,x1)%*%c(a1,a2)
prob<-exp(lin)/(1+exp(lin))
Y<-rbinom(n,1,prob)
L_p<-runif(n,0,1)
C<-matrix(rexp(n,0.1),ncol = 1)   # fixed censoring point, with mean 10
status<-matrix(rep(0,n),ncol=1)
L<-matrix(rep(0,n),ncol=1)
R<-matrix(rep(0,n),ncol=1)
u<-runif(n,0,1)
time<--log(1-u)*exp(as.matrix(x1)%*%b1)
T<-time*Y+(1-Y)*C
for(i in 1:n)
{
  if(Y[i]==0)
  {
   status[i]<-2
   L[i]<-T[i]
   R[i]<-NA   # cured and right censoring, Ti=T_li=exp(0.1), mean 10
  }
  if(Y[i]==1) #uncured part
  {
   if(T[i]>C[i])
   {
     status[i]<-2
     L[i]<-C[i]
     R[i]<-NA   # uncure and right censoring
   }
    if(T[i]<L_p[i])
    {
      status[i]<-0
      L[i]<-0
      R[i]<-L_p[i] # left censoring 
    }
    if(T[i]>L_p[i]&T[i]<C[i])
    {
      status[i]<-1
      j<-1
      len<-runif(1,0,1)
      repeat{
        tem_l<-L_p[i]+(j-1)*len
        tem_r<-L_p[i]+j*len
        j<-j+1
        if(T[i]<tem_r)
        {break}
      }
      L[i]<-tem_l 
      R[i]<-tem_r #interval censored data
    }
    }
  }
  data<-data.frame(T,L,R,status,x1)
  return(data)
}
  
  
  
  
  
  
  
  
}