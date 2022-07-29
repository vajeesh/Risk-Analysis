####### Alternative solution of Hospital beds allocation
pr<-1/2.9 ### For geometric distribution E(X)=1/p
lmd<-14.8 ## Poisson mean parameter giving average patient inflow
nbeds<-64 ## Number of vaiable beds
boc<-0 ### Initially no bed is occupied
Repl<-10000 ## Monte-Carlo iteration
BOC<-numeric(Repl) ### Daily bed occupancy rate
PPL<-numeric(Repl) ### Proportion of patients lost each day
for(i in 1:Repl){
pin<-rpois(1,lambda=lmd)  ### New patient inflow
PPL[i]<-max(0,((pin-(nbeds-boc))/pin))
boc<-min(nbeds,(boc+pin)) ### Beds occupied at day i
BOC[i]<-boc/nbeds ### Current bed occupancy rate
pout<-rbinom(1,size=boc,prob=pr) ### Ptient out by the end of day i
boc<-boc-pout
}
mean(BOC<=0.85)
mean(PPL)

