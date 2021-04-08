###
######### ARIMA

snpts<- ts(snpf$Snp.Close, start=c(2020),frequency = 365)
oilts<-ts(crudeoilf$Oil.Close,start=c(2020),frequency=365)
summary(oilts)
ggAcf(Ds)+ggtitle("ACF of the crude oil price")
ggPacf(Ds)+ggtitle("PACF of the crude oil price")
Ds<-diff(snpts)
ggAcf(Ds)+ggtitle("ACF of the crude oil price after first difference")
ggPacf(Ds)+ggtitle("PACF of the crude oil price after first difference")
shapiro.test(oilts)
acf(Ds, main= "ACF for crude oil closing price")
pacf(Ds, main= "PACF for crude oil closing price")
bestmodel<-auto.arima(Ds,seasonal = TRUE)
bestmodel
arima(Ds,order = c(9L,1L,12L))
autoplot(bestmodel)

shapiro.test(Do)
summary(crudeoil$Oil.Close)
summary(snp$Snp.Close)
setwd("C:/Users/sazid/Desktop/documents/Thesis/")
crudeoil <- read.csv(file.choose(), stringsAsFactors=FALSE)
crudeoil$Date<-as.Date(crudeoil$Date,format = "%m/%d/%y")
view(crudeoil)
summary(crudeoil$Oil.Close)



View(snp)
summary(snp$Snp.Close)
