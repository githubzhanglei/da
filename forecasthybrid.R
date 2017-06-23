##auto.arima
library('forecast')
library('ggplot2')
library('forecastHybrid')

data1<-read.csv("D:\\DataChallenge\\Rprojects\\portland-oregon-average-monthly-.csv")
P<-data1[2]
P<-log(P)
ridersnew<-ts(P[1:96,],frequency=12,start=c(1960,1))
riderstest<-ts(P[97:114,],frequency=12,start=c(1968,1))
autoarima<-auto.arima(ridersnew,trace=T, max.p = 8, max.q = 8, max.d = 2, max.P = 8, max.Q = 8, max.D = 2)
autoarimaforecast<-forecast(autoarima,h=18,level=c(99.5))
rmse1<-sqrt(mean ((exp(riderstest)-exp(autoarimaforecast$mean))^2))
##forecasthybrid
hf<-hybridModel(ridersnew)
hff<-forecast(hf,h=18, level = c(99.5))
accuracy(hf)
plot(hff)
rmse2<-sqrt(mean ((exp(riderstest)-exp(hff$mean))^2))


