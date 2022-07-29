R<-10000 ## replication
cl<-0 ### Customer lost
lambda<-1 ### Arrival rate
mu<-1 ### Service rate; assume mu=lambda
ta<-cumsum(rexp(n=R,rate=lambda)) #time of arrival of a customer
#ts<-0 # service time for the last customer arrived
ts<-ta[1]+rexp(1,rate=mu) ### time when the last one's service is finished
cs<-1 ### First customer served
for(i in 2:R){
if(ta[i]>ts){
  cs<-cs+1
  ts<-ta[i]+rexp(n=1,rate=mu)
}else{
  cl<-cl+1
}

}
#### question c
cl<-0 ### Customer lost, 0 at the beginning
lambda<-1
mu<-1
ta<-cumsum(rexp(n=R,rate=lambda)) #time of arrival of a customer
ts<-ta[1]+rexp(1,rate=mu) ### time until the server is busy
cs<-1 ### First customer served
q<-0 ## Queue is empty
tq<-0### Time until the queue is busy
for(i in 2:R){
if(ta[i]>tq) q<-0
  if(q==0){
  if(ta[i]>ts){
   cs<-cs+1
   ts<-ta[i]+rexp(1,rate=mu)
  }else{
    q<-1
    tq<-ts
    ts<-ts+rexp(1,rate=mu)
    cs<-cs+1
  }
}else{
    cl<-cl+1
  }
}
### Question d
cl<-0 ### Customer lost, 0 at the beginning
lambda<-1
mu<-1
ta<-cumsum(rexp(n=R,rate=lambda)) #time of arrival of a customer
ts1<-ta[1]+rexp(1,rate=mu) ## Time until sever 1 is busy
ts2<-ta[2]+rexp(1,rate=mu) ## Time until sever 2 is busy
cs<-2
for(i in 3:R){
  sb<-min(ts1,ts2) ### Time until the system is busy
  if(ta[i]>sb){
    cs<-cs+1
    bb<-ta[i]+rexp(n=1,rate=mu)
    if(ts1<ts2){ts1<-bb
    }else{ts2<-bb}
  }else{
    cl<-cl+1
  }
}
