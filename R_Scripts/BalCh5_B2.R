##X~Binomial(n=3,p=.5)
X<-rbinom(20000,size=3,prob=0.5)
daughter<-function(n){
##P(X=k)=_nC_k*p^k*(1-p)^(n-k) ## pmf of Binomial distribution
P0<-choose(3,0)*0.5^0*(1-0.5)^(3-0)
P1<-choose(3,1)*0.5^1*(1-0.5)^(3-1)
P2<-choose(3,2)*0.5^2*(1-0.5)^(3-2)
P3<-choose(3,3)*0.5^3*(1-0.5)^(3-3)
F0<-P0
F1<-F0+P1
F2<-F1+P2
F3<-F2+P3
Xs<-numeric(n)
for(i in 1:n){
U<-runif(1)
if(U<=F0) Xs[i]<-0
if(U>F0 & U<=F1) Xs[i]<-1
if(U>F1 & U<=F2) Xs[i]<-2
if(U>F2 & U<=F3) Xs[i]<-3
}
return(Xs)
}
#### Problem b ####
X<-rexp(n=20000,rate=1)
mean(log(X))
#### Problem c ####
##X~Unif(0,1)
##Y~Unif(0,1)
## Z = X + Y 
################
X<-runif(20000)
Y<-runif(20000)
Z<-X+Y
plot(density(Z))
W<-10*X+Y
plot(density(W))
mean(W)
