library(installr)
updateR()
#install these packages
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(Johnson)
library(TSstudio)
library(rcompanion)
library(NlinTS)
#library(ggpubr)
##load the data set
setwd("C:/Users/sazid/Desktop/documents/Thesis/")
crudeoil <- read.csv(file.choose(), stringsAsFactors=FALSE)
crudeoil$Date<-as.Date(crudeoil$Date,format = "%m/%d/%y")
summary(crudeoil$Oil.Close)
plotNormalHistogram(crudeoil$Oil.Close, main = "Crude oil price", col = "dark green")
view(crudeoil)
##boxplot
c<-ggplot(crudeoil,aes(Oil.Close))+geom_boxplot(fill="dark green",color="black",outlier.shape=25)
c+labs(x="oil closing price", y="")


shapiro.test(crudeoil$Oil.Close)
#log10
crudeoil$Oil.Close<-log(crudeoil$Oil.Close)
view(crudeoil)
shapiro.test(crudeoil$Oil.Close)
#sqrt
crudeoil$Oil.Close<-sqrt(crudeoil$Oil.Close)
view(crudeoil)
shapiro.test(crudeoil$Oil.Close)

#log-return
crudeoil$Oil.Closet=c(NA,diff(log(crudeoil$Oil.Close)))
hist(crudeoil$Oil.Closet)
shapiro.test(crudeoil$Oil.Closet)

#johnson
a<-(RE.Johnson(crudeoil$Oil.Close))
a<-data.frame(a)
#shapiro.test(d)
#d<-diff(a$transformed)
crudeoilf<-data.frame(crudeoil)
rownames(crudeoilf)<-crudeoilf$Date
abc<-data.frame(crudeoilf,a)
view(abc)

crudeoil<-abc
crudeoilf<-data.frame(crudeoil)
names(crudeoilf)
names(crudeoilf)[10]<-"Oil.Closet"
names(crudeoilf)
names(crudeoilf)[2]<-"Oil.Closet"


str(crudeoilf)
class(crudeoilf$Date)
view(crudeoilf)
head(crudeoil)

crudeoilf<-crudeoilf[-1,]
##############################################
snp <- read.csv(file.choose(), stringsAsFactors=FALSE)
snp$Date<-as.Date(snp$Date,format = "%m/%d/%y")
summary(snp$Snp.Close)
plotNormalHistogram(snp$Snp.Closet, main = "S & P 500 price", col = "sky blue")
##boxplot
s<-ggplot(snp,aes(Snp.Close))+geom_boxplot(fill="sky blue",color="black",outlier.shape=25)
s+labs(x="s&p 500 closing price", y="")
shapiro.test(snp$Snp.Close)

#log 
snp$Snp.Close<-log(snp$Snp.Close)
view(snp)
shapiro.test(snp$Snp.Close)
#sqrt
snp$Snp.Close<-sqrt(snp$Snp.Close)
view(snp)
shapiro.test(snp$Snp.Close)

#log-return
snp$Snp.Closet=c(NA,diff(log(snp$Snp.Close)))
hist(crudeoil$Oil.Closet)
shapiro.test(snp$Snp.Closet)
view(snp$Snp.Closet)
#johnson
b<-(RE.Johnson(snp$Snp.Close))
b<-data.frame(b)
snpf<-data.frame(snp)
rownames(snpf)<-snpf$Date
view(snpf)
cba<-data.frame(snpf,b)
view(cba)

snp<-cba
snpf<-data.frame(snp)
names(snpf)
names(snpf)[11]<-"Snp.Closet"
names(snpf)
names(snpf)[5]<-"Snp.Closet"

str(snpf)
class(snpf$Date)
view(snpf)
view(data)







############### merging
data<-merge(crudeoilf, snpf, by="Date")
data<-data.frame(data)
view(data)

##########row remove
data<-data[-1,]
view(data)

crudeoilf<-crudeoilf[-1,]




data$Snp.Close<-scale(data$Snp.Close)
data$Oil.Closet<-scale(data$Oil.Closet)
view(data)

##box plot together
ggplot(data = data,mapping = aes(x=data$Oil.Closet,data$Snp.Closet y=))+geom_boxplot()







plotNormalHistogram(data$Oil.Closet, main = "Crude oil price", col = "dark green")
plotNormalHistogram(data$Snp.Closet, main = "S & P 500 price", col = "sky blue")
hist(data$Oil.Closet)
hist(data$Snp.Closet)
#oil<-((data$Oil.Close+1))
#snp<-(log10(data$Snp.Close+1))
#hist(sqrt)
#hist(data$Snp.Close)
#sqrt1<-(sqrt(data$Snp.Close))
#hist(sqrt1)
###plotting
par(mfrow=c(2,1))
plot.ts(data$Oil.Closet,lty=1,lwd=1,col='dark green', xaxt="n", ylab="",xlab="September October",main="Crude oil closing price",
        ylim=c(),panel.first = grid())
plot.ts(data$Snp.Closet,lty=1,lwd=1,col='sky blue', xaxt="n", ylab="",xlab="September October",main="S&P 500 closing price",
        ylim=c(),panel.first = grid())
#plot.ts(data$Energy.Close,lty=1,lwd=1,col='blue', xaxt="n", ylab="",xlab="",main="",
#ylim=c(),panel.first = grid())



#dataf<-data.frame(data)
#View(dataf)

#dataf$Date <- lubridate::dmy(dataf$Date)
#dplyr::arrange(dataf, Date)


#dataf<- dataf[seq(dim(dataf)[1],1),]
#dataf[order(as.Date(dataf$Date, format="%m/%d/%Y")),]


#dataf$ID<-1:dim(dataf)[1]


#start=1          
#finish=128 

#plot.ts(dataf$Energy.Close[start:finish],lty=1,lwd=1,col='red', xaxt="n", ylab="",xlab="",main="",
# ylim=c(),panel.first = grid())
#view(dataf)
#see correlation

ggplot(data=data)+geom_point(mapping =aes(x=Oil.Closet,y=Snp.Closet))
ggplot(data,aes(x=Date,y=Oil.Closet,color= Oil.Closet))+geom_line()
ggplot(data,aes(x=Date,y=Snp.Closet,color= Snp.Closet))+geom_line()

g1<-ggplot(data = data,aes(x=Date,y=Snp.Closet))+geom_line(color="sky blue")
g1
g1<-g1+theme_classic()+theme(axis.text = element_text(size=rel(1.2)))







combine<- data.frame(date = index(data), 
                     data, 
                     row.names = NULL) %>% 
  select(Date, Close = Snp.Closet) %>% 
  mutate(ticker = 'Snp.Closet') %>%
  bind_rows(., 
            data.frame(date = index(data), 
                       data, 
                       row.names = NULL) %>% 
              select(Date, Close = Oil.Closet) %>% 
              mutate(ticker = 'Oil.Closet'))


g<-ggplot(data = combine,aes(x=Date,y=Close,color=ticker))+geom_line()
g





#declare time series variables
oilts<- ts(data$Oil.Closet, start=c(2020,10),frequency = 365)
snpts<- ts(data$Snp.Closet, start=c(2020, 9),frequency = 365)
#energyts<- ts(data$Energy.Close,start = c(2011,2),frequency = 365)


### first difference oilts
Do<-diff(oilts)
###ADF test
adf.test(oilts)
#pptest
pp.test(Do)
#kpss test
kpss.test(Do)
### first difference snp500
Ds<-diff(snpts)
###ADF test
adf.test(Ds)
#pptest
pp.test(Ds)
#kpss test
kpss.test(snpts)


##normality test
shapiro.test(Do)
shapiro.test(Ds)
qqplot(data$Oil.Closet, data$Snp.Closet, xlab = "Crude oil", ylab = "S&p 500", main = "Q-Q Plot")
#plot the series
autoplot(cbind(oilts,snpts))
autoplot(a)
view(a)

autoplot(cbind(oilts,snpts)) + ggtitle("Closing Price, 2nd November 2020 to 31st December 2020") + labs(x = "Time", y = "Closing Price")
ts_plot(cbind(oilts,snpts), line.mode = "lines", title = "Closing Price, 2nd November 2020 to 31st December 2020")


combine<- data.frame(date = index(a), 
                     a, 
                     row.names = NULL) %>% 
  select(Date, Close = Ds) %>% 
  mutate(ticker = 'Snp.Close') %>%
  bind_rows(., 
            data.frame(date = index(a), 
                       a, 
                       row.names = NULL) %>% 
              select(Date, Close = Do) %>% 
              mutate(ticker = 'Oil.Close'))
view(combine)

g<-ggplot(data = combine,aes(x=Date,y=Close,color=ticker))+geom_line()



ols1<-lm(snpts~oilts)
summary(ols1)
par(mfrow=c(2,2))
plot(ols1)

library(MASS)
bc<-boxcox(ols1,lambda = seq(-3,3))
view(data)

ols2<-lm(oilts~snpts)
par(mfrow=c(2,2))
plot(ols2)
summary(ols2) 

#determine the persistence of the model
acf(oilts, main= "ACF for crude oil closing price")
pacf(oilts, main= "PACF for crude oil closing price")
acf(snpts, main= "ACF for S&P 500 closing price")
pacf(snpts, main= "PACF for S&P 500 closing price")

ggAcf(oilts)+ggtitle("ACF of the crude oil price")
ggPacf(oilts)+ggtitle("PACF of the crude oil price")
ggAcf(snpts)+ggtitle("ACF of the s&p 500 price")
ggPacf(snpts)+ggtitle("PACF of the s&p 500 price")

##lag selection
datal<-cbind(snpts,oilts)
lagselect<-VARselect(datal,lag.max = 15,type = "const")
lagselect$selection
##############building VAR
datam<-VAR(datal,p=10,type="const", season=NULL,exog=NULL)
summary(datam)

######### non-stationary

#diagnosis VAR
#serial test for autocorrelation
datasr<-serial.test(datam, lags.pt = 10, type = "PT.asymptotic")
datasr
#arch test for volatility/heteroscadasticity
dataarch<-arch.test(datam, lags.multi = 10, multivariate.only = TRUE)
dataarch
#normal distribution of the residuals
#normaity test
datanorm<-normality.test(datam, multivariate.only = TRUE)
datanorm
shapiro.test(datanorm$resid)

#testing for structural breaks in the residuals/ stability test
datast<-stability(datam, type = "OLS-CUSUM")
plot(datast)
##granger causality test linear
grangeroil<-causality(datam,cause = "snpts")
grangeroil
grangersnp<-causality(datam,cause = "oilts")
grangersnp

##granger causality test non-linear
nlgrangeroil<-nlin_causality.test(oilts,snpts,lag = 3,LayersUniv = 2, LayersBiv = 1)
nlgrangeroil$summary()

nlgrangersnp<-nlin_causality.test(snpts,oilts,lag = 3,LayersUniv = 2, LayersBiv = 1)
nlgrangersnp$summary()





##Impulse response function
snpirf<-irf(datam, impulse = "oilts", response = "snpts", n.ahead = 10, boot = TRUE)
snpirf
plot(snpirf,ylab="s&p 500", main= "shock from crude oil price")
oilirf<-irf(datam, impulse = "snpts", response = "oilts", n.ahead = 10, boot = TRUE)
oilirf
plot(oilirf,ylab="crude oil price", main= "shock from s&p 500")
#variance decomposition
vd<-fevd(datam, n.ahead = 10)
vd
plot(vd)
#var forecasting
forecast<-predict(datam,n.ahead = 10,ci=.95)
fanchart(forecast, names = "snpts")
fanchart(forecast, names = "oilts")
forecast
######################################
forecast(datam) %>%
  autoplot()+xlab ("Time")


##SVAR restriction
mat<-diag(2)
mat
mat[2,1]<-NA
mat

datal<-cbind(snpts,oilts)
lagselect<-VARselect(datal,lag.max = 15,type = "const")
lagselect$selection
##estimating the model
datam<-VAR(datal,p=10,type="const", season=NULL,exog=NULL)
svar<-SVAR(datam,Amat = mat)
summary(svar)
##impulse response function

svarirfss<-irf(svar, impulse = "Ds", response = "Ds")
plot(svarirfss)
svarirfso<-irf(svar,impulse = "snpts", response = "oilts")
plot(svarirfso)
svarirfos<-irf(svar,impulse = "oilts", response = "snpts")
plot(svarirfos)
svarirfoo<-irf(svar,impulse = "Do", response = "Do")
plot(svarirfoo)
