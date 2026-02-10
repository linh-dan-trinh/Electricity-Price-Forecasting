summary(hksum)

  hksum$DATES = seq.Date(as.Date("2022-06-01"),
                           as.Date("2022-08-24"),
                           by="days")
hist(hksum$HOKKAIDO.DAILY, freq = F, main = "Summer Hokkaido distribution", xlab = "Price (JPY/kWh)")

plot(hksum$DATES, hksum$HOKKAIDO.DAILY, type = "l", col = 2, main = "Summer Price",ylim = c(0, 70), xlab = "Date", ylab = "Price (JPY/kWh)" )
lines(hksum$DATES, kssum$KYUSHU.DAILY, col = 3)
legend("topleft", legend=c("Hokkaido", "Kyushu"),
       col=c("red", "green"), lty=1:1, cex=0.8)
                                                                                                                                                                                                                         

df1 <- data.frame(hksum$DATES[-1][-1][-1][-1][-1][-1][-1],diffhksum_ts)
ggplot() + geom_line(data = hksum, aes(x=DATES, y = HOKKAIDO.DAILY, colour="Original Price ")) + xlab("Time") + geom_line(data = df1, aes(x = hksum$DATES[-1][-1][-1][-1][-1][-1][-1] , y = diffhksum_ts, colour="seasonal difference")) + ylab("Price (JPY/kWh)")

adf.test(hksum$HOKKAIDO.DAILY)
kpss.test(Hksum$HOKKAIDO.DAILY)


acf(hksum$HOKKAIDO.DAILY, main = "ACF of Summer Hokkaido")
pacf(hksum$HOKKAIDO.DAILY, main = "PACF of Summer Hokkaido")


################################
hksum_ts <- ts(hksum$HOKKAIDO.DAILY, start = c(1,85), frequency = 7)
diffhksum_ts <- diff(hksum_ts, differences = 1, lag = 7)
plot.ts(hksum_ts, ylim=c(-40,80),col= "blue", main = "Summer HKD original vs. seasonal difference", ylab = "Price (JPY/kWh)") 
lines(diffhksum_ts, col = "brown") 
legend("topleft", legend=c("Original Price ", "Seasonal difference"),
       col=c(1,6), lty=1:1, cex=0.8)

acf(diffhksum_ts, main = "Summer HKD seasonal difference")
pacf(diffhksum_ts, main = "Summer HKD seasonal difference")
#######################################
diffhksum <- diff(diffhksum_ts, differences = 1)
plot(diffhksum, main = "Summer HKD first and seasonal difference", xlab = "Time", ylab= "")
acf(diffhksum, main = "ACF of Summer HKD transformed")
pacf(diffhksum, main = "PACF of Summer HKD transformed")
############################

decomp = mstl(hksum_ts, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp, main = "Summer HKD trend and seasonality decomposition")

#############################
fit33 <- arima(hksum_ts,order = c(1,1,8), seasonal = c(1,1,2))
summary(fit33)
tsdisplay(residuals(fit33), lag.max = 90, main= 'Summer HKD
          ARIMA (1,1,8)(0,1,2) [7] model residuals')
hist(fit33$residuals, freq = F, main = "Histogram of Summer HKD residuals
  ARIMA(1,1,8)(1,1,2)[7]", xlab= "Residuals", ylim=c(0, 0.3))
lines(density(fit33$residuals), col="red")
Box.test(fit33$residuals, lag=40, type="Ljung-Box", fitdf =14)

fit13 <- arima(hksum_ts,order = c(1,1,8), seasonal = c(3,1,8))
summary(fit13)
tsdisplay(residuals(fit13), lag.max = 90, main= 'Summer HKD
          ARIMA (1,1,8)(3,1,2) [7] model residuals')
hist(fit13$residuals, freq = F, main = "Histogram of Summer HKD residuals
  ARIMA(1,1,8)(3,1,2)[7]", xlab= "Residuals", ylim=c(0, 0.13))
lines(density(fit13$residuals), col="red")
Box.test(fit13$residuals, lag=40, type="Ljung-Box", fitdf =17)

fit53 <- arima(hksum_ts,order = c(1,2,8), seasonal = c(3,1,8))
summary(fit13)
tsdisplay(residuals(fit13), lag.max = 90, main= 'Summer HKD
          ARIMA (1,1,8)(3,1,2) [7] model residuals')
hist(fit13$residuals, freq = F, main = "Histogram of Summer HKD residuals
  ARIMA(1,1,8)(3,1,2)[7]", xlab= "Residuals", ylim=c(0, 0.13))
lines(density(fit13$residuals), col="red")

fit23 <- auto.arima(hksum_ts, seasonal = T)
summary(fit23)
tsdisplay(residuals(fit2), lag.max = 85)
Box.test(fit2$residuals, lag=10, type="Ljung-Box", fitdf =2)
fit13$aic
fit23$aic          
fit33$aic
fit1$b

fit43 <- auto.arima(diffhksum_ts, seasonal = F)
summary(fit43)
##########################
hold <- window(hksum_ts, start = 20 )
??window
fit_no_holdout = arima(hksum_ts[-c(50:68)], order = c(1,1,8), seasonal = c(1,1,2))
fcast_no_holdout <- forecast(fit_no_holdout, h = 7)
plot(fcast_no_holdout, ylim = c(-30, 70))
plot(hksum_ts, col = 2)
lines(diffhksum_ts, col = 3)

####################
fmod13 = forecast(fit13, h = 7)
fmod23 = forecast(fit23, h = 7)
fmod33 = forecast(fit33, h = 7)
plot(fmod13, main = "7 - day forecast for Summer HKD
     ARIMA(1,1,8)(3,1,2)[7]", xlab = "Time", ylab = "Price (JPY/kWh)")
plot(fmod23, main = "7 - day forecast for Summer HKD
     AUTO.ARIMA(1,0,0)(1,0,0)[7]", xlab = "Time", ylab = "Price (JPY/kWh)")
plot(fmod33)
fmod13
fmod23
fmod33

write.csv(x = fmod13, file = "summer hkd.csv")
