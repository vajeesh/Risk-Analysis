#Problem Ch4_10
#Spam email per day is Poisson with mean 200
rep<-1000 #Number of days examined
lambda<-200 #rate parameter
p<-rpois(rep,lambda)
hist(p,freq=F) #histogram over empirical pdf
plot(ecdf(p)) #empirical cdf
sum(p<190)/rep



#Problem 31a
rep<-10000; prop<-0; x<-1:10
for (i in 1:rep)
{
d<-0
if(min(sample(x,3,replace=F))==4) d<-1
prop<-prop+d
}
print(prop/rep)



#1 Birthday problem. The probability that two (or more) are born on the same day
#What about three (or more)
#What about exactly three
#Number of persons
n<-14
rep<-10000
prop<-0
for (i in 1:rep)
{
d<-0
a<-(1+floor(365*runif(n)))
b<-unique(a)
if(length(b)<n) d<-1
#print(d)
prop<-prop+d
}
print(prop/rep)

#2 Expectation of Bernoulli r.v.
#We select at random one from the class and id her och him as 0 or 1 dep gender
m<-5 # set no of males
f<-2 # set no of females
p<-m/(m+f)
rep<-1000000
prop<-0
for (i in 1:rep)
{ d<-0
if(runif(1)<p) d<-1
prop<-prop+d }
print(prop/rep)

#3 Stud exercise: Binomial r.v.
#We select at random with replacement four from the class and id them
m<-5 # set no of males
f<-2 # set no of females
p<-m/(m+f)
rep<-20000
prop<-0
for (i in 1:rep)
{ d<-0
if(rbinom(1,4,p)==0) d<-1
prop<-prop+d }
print(prop/rep)

#4 Highway accidents: Poisson r.v. Example 2.11
lambda<-3
rep<-50000
prop<-0
for (i in 1:rep)
{ d<-0
if(rpois(1,lambda)<3) d<-1
prop<-prop+d }
print(prop/rep)

#5 pdf and cdf of a uniform r.v.
n<-100000 #no obs
u<-runif(n)
hist(u,freq=F) #histogram over empirical pdf
plot(seq((1/n),1,(1/n)), sort(u), "l")
#ALT plot(ecdf(u)) #empirical cdf

#6 pdf and cdf of an exponetial r.v.
n<-10000 #no obs
lambda<-3 #rate parameter
e<-rexp(n,lambda)
hist(e,freq=F) #histogram over empirical pdf
plot(ecdf(e)) #empirical cdf

#7 pdf and cdf of a normal r.v.
n<-10000 #no obs
mu<-9 #location parameter
sigma<-0.5 #scale parameter
x<-rnorm(n,mu,sigma)
hist(x,freq=F) #histogram over empirical pdf
plot(ecdf(x)) #empirical cdf

#8 Expected value of the cubic of a uniform(0,1) r.v. (examp 2.26)
n<-10000 #no obs
u<-runif(n)
y<-1/u
mean(y)
hist(y,freq=F) #histogram over empirical pdf

#9 Expected value of the sum of a uniform(0,10) and N(1,1) r.v. 
n<-10000 #no obs
y<-runif(n)*10+rnorm(n,1,1)
mean(y)
sd(y)
hist(y,freq=F) #histogram over empirical pdf

#10 Illustration of Central Limit Theorem, mean calculated from n iid Exp(1)
lambda<-1
n<-30   #30
rep<-1000
e_bar<-seq(1,rep,1)*0
for (i in 1:rep)
{ 
e_bar[i]<-mean(rexp(n,lambda))
}
hist(e_bar,freq=F) #histogram over empirical pdf

#11 Stochastic process - time to work
set.seed(630829)
rep<-10 #consider also 100, 1000, 10000
d<-c(0,1,1,23.6,24) #distances from home
z<-NULL
for (i in 1:rep) {
t<-rep(0,5) #initializing the random vector
t[2]<-rnorm(1,8,1)  #Walking time to the bus stop
b<-rbinom(1,1,0.03) #3% of the times there is major disturbance in the service
t[3]<-(1-b)*5*runif(1)+b*rexp(1,0.1)  #Waiting time at the bus stop given that buses are scheduled and I aim for 5 min
t[4]<-(1-b)*rnorm(1,31,2)+b*(31+rexp(1,0.1)) #Driving time for the bus
#t[5]<-rnorm(1,5,2) #Walking time for bus halt to office
t[5]<-rnorm(1,(6-(sum(t[1:4]))/50),2) #Walking time from bus halt is dependent
z1<-c(t[1],sum(t[1:2]),sum(t[1:3]),sum(t[1:4]),sum(t[1:5])) #Summing the duration
z<-c(z,z1)
}
plot(d,z[1:5],type="l",xlab="travel distance",ylab="time in min",xlim=c(0,25),ylim=c(0,90))
for (j in 1:(rep-1)){
#a<-combn(40,5)   #delays the graphics if you want
lines(d,z[(5*j+1):(5*(j+1))],col=min(j,100))
}
m<-matrix(z,5,rep) #create a column for each trip
mean(m[5,])   #average travel time
sd(m[5,])     #standard dev of travel time
hist(m[5,])   #emp pdf
plot(ecdf(m[5,]))   #emp cdf

#12 Conditional distributions
n<-100
a<-0
b<-1
c<-2
d<-0
x<-rnorm(n,0,1)
z<-d*x+rnorm(n,0,1)
e<-sqrt(12)*runif(n)-6
y<-a+b*x+c*z+e 
plot(x,y)

#estimate of lambda in distance decay parameter from HUI
l<-seq(0.033,0.034,0.0001)

lik<-((1-exp(-2.5*l))**0.14)*
((exp(-2.5*l)-exp(-5*l))**0.22)*
((exp(-5*l)-exp(-25*l))**0.32)*
((exp(-25*l)-exp(-50*l))**0.17)*
((exp(-50*l)-exp(-125*l))**0.09)*
((exp(-125*l)-exp(-250*l))**0.04)*
((exp(-250*l)-0)**0.02)
plot(l,lik,type="l")

l<-0.0335
f<-0.14*(2.5*exp(-2.5*l)-0*exp(-0*l))/(-exp(-2.5*l)+exp(-0*l))+
0.22*(5*exp(-5*l)-2.5*exp(-2.5*l))/(-exp(-5*l)+exp(-2.5*l))+
0.32*(25*exp(-25*l)-5*exp(-5*l))/(-exp(-25*l)+exp(-5*l))+
0.17*(50*exp(-50*l)-25*exp(-25*l))/(-exp(-50*l)+exp(-25*l))+
0.09*(125*exp(-125*l)-50*exp(-50*l))/(-exp(-125*l)+exp(-50*l))+
0.04*(250*exp(-250*l)-125*exp(-125*l))/(-exp(-250*l)+exp(-125*l))+
0.02*(-250*exp(-250*l))/(exp(-250*l))
print(f)

q<-c(2.5,5,25,50,125,250)
pexp(q,l)

#Illustrating interval estimation
#Second we conduct as an illustration with R=1
set.seed(630829)
x<-rnorm(100,1,10)
t1<-mean(x)-1.96*sd(x)/sqrt(100)
t2<-mean(x)+1.96*sd(x)/sqrt(100)
mean(x)
print(c(t1,t2))

#Bootstrap
br<-999 #Bootstrap replicates
mean_boot<-NULL
for (i in 1:br) {
x_boot<-sample(x,100,replace=T)
mean_boot<-c(mean_boot,mean(x_boot)) }
t1_a<-mean(mean_boot)-1.96*sd(mean_boot)
t2_a<-mean(mean_boot)+1.96*sd(mean_boot)

print(c(mean(mean_boot),sd(mean_boot)))
print(c(t1_a,t2_a))
quantile(mean_boot, probs=c(0.025,0.975))


#Second we conduct as an illustration with R large
cover<-0
for (i in 1:50)
{
x<-rnorm(100,1,10)
t1<-mean(x)-1.96*sd(x)/sqrt(100)
t2<-mean(x)+1.96*sd(x)/sqrt(100)
print(c(t1,t2)) 
if(t1<1&t2>1) print("covers") else print("not cover")
#if(t1<1&t2>1) cover<-cover+1
}
#cover/1000


