###Recommnded exercise b, Ch. 8.1-8.3
rm(list=ls())
s0<-5 ### Current state
Rep<-10000 ### Number of Monte Carlo iteration
inb20<-rep(1,Rep) ### whether the person in business for n=20, start with always in business
inb200<-rep(1,Rep) ### whether the person in business for n=20, start with always in business
eqt20<-rep(NA,Rep) ### equity series for n=20; strat with missing values
eqt200<-rep(NA,Rep) ### equity series for n=200
set.seed(199)
for(i in 1:Rep){
rwk<-rbinom(n=200,prob=0.6,size=1)
rwk<-ifelse(rwk==0,-1,rwk)
f20<-cumsum(rwk[1:20])
f200<-cumsum(rwk)
if(any(f20<=(-1*s0))){
inb20[i]<-0 
}else{
eqt20[i]<-s0+f20[20]  
}
if(any(f200<=(-1*s0))){
  inb200[i]<-0 
}else{
  eqt200[i]<-s0+f200[200]  
}
}
mean(inb20) ### Probability to be in business for n=20
mean(inb200) ### Probability to be in business for n=20
mean(na.omit(eqt200)) ## Mean equity given in business
sd(na.omit(eqt200))
plot((eqt20[1:2000]),type="l")

