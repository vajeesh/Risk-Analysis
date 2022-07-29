library(quantmod) # get stock prices; useful stock analysis functions
library(stringr) # working with strings
library(corrplot) # for plotting correlation
library(PerformanceAnalytics) # evaluating the performance and  risk  characteristics  of  financial  assets  or  funds
#Loads the company stock using ticker

getSymbols("MSFT",from="2009-10-01",to="2021-09-30",src="yahoo") # Microsoft 
getSymbols("AAPL",from="2009-10-01",to="2021-09-30",src="yahoo") # Apple 
getSymbols("INTC",from="2009-10-01",to="2021-09-30",src="yahoo") #Intel
getSymbols("KO",from="2009-10-01",to="2021-09-30",src="yahoo")  # Coca Cola
getSymbols("WMT",from="2009-10-01",to="2021-09-30",src="yahoo") #Walmart

#Stock returns in log
MSFT_log_returns<-dailyReturn(MSFT,type='log')
INTC_log_returns<-dailyReturn(INTC,type='log')
APPL_log_returns<-dailyReturn(AAPL,type='log')
KO_log_returns<-dailyReturn(KO,type='log')
WMT_log_returns<-dailyReturn(WMT,type='log')
MSFT_log_returns
#Mean of log stock returns 

MSFT_mean_log<-mean(MSFT_log_returns)
INTC_mean_log<-mean(INTC_log_returns)
APPL_mean_log<-mean(APPL_log_returns)
KO_mean_log<-mean(KO_log_returns)
WMT_mean_log<-mean(WMT_log_returns)
MSFT_mean_log
#round it to 4 decimal places

mean_log<-c(INTC_mean_log,APPL_mean_log,KO_mean_log,MSFT_mean_log,WMT_mean_log)
mean_log<-round(mean_log,4)
mean_log
#standard deviation of log stock returns

MSFT_sd_log<-sd(MSFT_log_returns)
INTC_sd_log<-sd(INTC_log_returns)
APPL_sd_log<-sd(APPL_log_returns)
KO_sd_log<-sd(KO_log_returns)
WMT_sd_Log<-sd(WMT_log_returns)
MSFT_sd_log
#round it to 4 decimal places 

sd_log<-c(INTC_sd_log,APPL_sd_log,KO_sd_log,MSFT_sd_log,WMT_sd_Log)
sd_log<-round(sd_log,4)
sd_log
#create data frame

graphic1<-data.frame(rbind(c("INTC",INTC_mean_log,INTC_sd_log),c("AAPL",APPL_mean_log,APPL_sd_log),c("MSFT",MSFT_mean_log,MSFT_sd_log),c("KO",KO_mean_log,KO_sd_log),c("WMT",WMT_mean_log,WMT_sd_Log)),stringsAsFactors = FALSE)


graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("INTC","APPL","MSFT","KO","WMT")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")
View(graphic1)
#Data frame contains the 5 companies with each company's average log return and standard deviation.
plot(Sd_Log_Return~Mean_Log_Return,data=graphic1,type="p",pch=(
  substring(row.names(graphic1),1))[order(graphic1$Mean_Log_Return)])
#Use R to observe a stock's performance
#chart components: bollinger bands, % bollinger change, volume, moving average convergence divergence

chart_Series(MSFT)
addBBands(n=10,sd=2)

#check correlation of different companies
data2<-cbind(diff(log(Cl(INTC))),diff(log(Cl(WMT))),diff(log(Cl(MSFT))),diff(log(Cl(AAPL))),diff(log(Cl(KO))))
View(data2)
#library(corrplot)
corrplot(cor(na.omit(data2)))

#Monte Carlo: Rooted in past performance is not an indicator of future results. Price fluctuations can not be predicted with accuracy

mu<-MSFT_mean_log
sig<-MSFT_sd_log
#testsim<-rep(NA,1000)

#generate random daily exponent increase rate using MSFT's mean and sd log returns

#one year 252 trading days, simulate for 4 years 
# 4*252 trading days
T<-252*4
price<-rep(NA,T)

#most recent colsing price
price[1]<-as.numeric(MSFT[(dim(MSFT))[1],4])
price[1]
#start simulating prices

for(i in 2:T){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

plot(price~seq(1,N),xlab="Day",ylab="Price")
#monte carlo simulation: incredibly useful forecasting tool to predict outcomes of events with many random variables

N<-300
mc_matrix<-matrix(nrow=T,ncol=N)
mc_matrix[1,1]<-as.numeric(MSFT[(dim(MSFT))[1],4])

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(MSFT[(dim(MSFT))[1],4])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

################################

#Code Question 7 here

