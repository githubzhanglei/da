##auto.arima
library('forecast')
library('ggplot2')
library('forecastHybrid')

data1<-read.csv("D:\\qq\\portland-oregon-average-monthly-.csv")
P<-data1[2]
ridersnew<-ts(P[1:96,],frequency=12,start=c(1960,1))
riderstest<-ts(P[97:114,],frequency=12,start=c(1968,1))
autoarima<-auto.arima(ridersnew,trace=T)
autoarimaforecast<-forecast(autoarima,h=18,level=c(95,99))
rmse1<-sqrt(mean (riderstest-autoarimaforecast$mean)^2)
##forecasthybrid
hf<-hybridModel(ridersnew)
hff<-forecast(hf,h=18)
accuracy(hf)
plot(hff)
rmse2<-sqrt(mean (riderstest-hff$mean)^2)

