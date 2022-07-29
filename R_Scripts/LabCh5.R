##X~Bin(n=3,p=.5)
daughter<-function(n){
P0<-choose(3,0)*.5^0*(1-0.5)^(3-0)
P1<-choose(3,1)*.5^1*(1-0.5)^(3-1)
P2<-choose(3,2)*.5^2*(1-0.5)^(3-2)
P3<-choose(3,3)*.5^3*(1-0.5)^(3-3)
C0<-P0
C1<-P0+P1
C2<-P0+P1+P2
C3<-P0+P1+P2+P3
Xs<-numeric(n)
for(i in 1:n){
u<-runif(1)
if(u<=C0) X=0
if(u>C0 & u<=C1) X=1
if(u>C1 & u<=C2) X=2
if(u>C2) X=3
Xs[i]<-X
}
return(Xs)
}

### Problem B #####
##Algorithm: ####
##1. Simulate large number of X's ###
##2. Compute log(X)
## 3. Take avegare of log(X)
###################################
X<-rexp(n=20000,1)
lX<-log(X)
mean(lX)
### Problem C ######
X<-runif(20000)
Y<-runif(20000)
Z<-X+Y
mean(X)
