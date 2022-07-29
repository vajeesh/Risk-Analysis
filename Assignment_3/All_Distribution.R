# Poisson distribution----
1-ppois(3, 1.5) # P(X > 3)
1-ppois(27, 20) # P(X > 27)

ppois(2, 1) # cdf

# Exponential distribution----
dexp(2, 0.5) # pdf
pexp(2, 0.5) # cdf

mean((1-pexp(15, 1/30)))

# Binomial distribution----
# P(X=27) whtn X~Bin(100, 0.25), pmf
dbinom(27, 100, 0.25)


# CDF P(X<= 27) wher X~bin(100, 0.25)
pbinom (27, 100, 0.25)

# Uniform Distribution----
# problem 48, let t and u are id and unif(0, 1)
# find E[tu]
t<-runif(10^6, 0, 1)
u<-runif(10^6, 0, 1)
logtu <- log(t) + log(u)
#E(tU)
mean(t*u)

# E[logxy]

mean(logtu)

# What are the 10th, 20th, and so forth quantiles of 
# the Bin(10, 1/3) distribution?

qbinom(0.1, 10, 1/3)
qbinom(0.2, 10, 1/3)

#Normal distribution----
# what si P(x>19) when X~N(17.46, 375.67)

1-pnorm(19, mean = 17.46, sd=sqrt(375.87))

# what is the P(X<27.4) where X~N(50, sd= 20)
# phi((27.4-50)/20) = Phi(-1.13) = 1-Phi(1.13) = 1-.871 = 0.129

pnorm(27.4, mean=50, sd=20)


# F-1(0.95) when X ~N(100, 15^2)
#Phi((X-100)/15)=.95, from the Z table .95 lands on 1.64
# now x= 1.64*15 +100 = 124.6

qnorm(0.95, mean= 100, sd=15)

# Random Variates

x<-rnorm(1000, mean=100, sd=15) # this generates 1000 iid normal r.n
hist(x, probability = T)        # plotes their histogram
xx<- seq(min(x), max(x), length=100) # pdf of the same distribution
lines(xx, dnorm(xx, mean=100, sd=15)) # gragh the pdf

# Multivariate normal----
# given that x~N(5, 0.01)  & y~N(10, 0.04)
# corr(x, y) = 0.8
#cov(x, y) = 0.016
# A= XY and C= 2(X+Y)
# E[C]?

# creating the distribution

#-------- from Kenneth -----------#


#Prob Ch3:103a-c
#install.packages("MASS")                            # Install MASS package
library("MASS")
xy=mvrnorm(10000,c(5,10),matrix(c(0.01, 0.016, 0.016, 0.04),ncol = 2))
x=xy[,1]
y=xy[,2]
c=2*x+2*y
# E[C]
mean(c)

a=x*y
#E[A]
mean(a)

#A plate is useless
#if C is less than 29 or more than 31 in.
#What is the probability that this happens? 
sum((c>29)*(c<31))/10000

# Geometric distiribution----
x_dgeom <- seq(0, 20, by = 1)  
y_dgeom <- dgeom(x_dgeom, prob = 0.5)    # density

y_pgeom <- pgeom(x_pgeom, prob = 0.5)  # cdf

y_qgeom <- qgeom(x_qgeom, prob = 0.5)   # quantials
