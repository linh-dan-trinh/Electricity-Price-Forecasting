summary(ksspr)

ksspr$DATES = seq.Date(as.Date("2023-03-01"),
                       as.Date("2023-05-24"),
                       by="days")

hist(ksspr$KYUSHU.DAILY, freq = F, main = "Spring Kyushu distribution", xlab = "Price (JPY/kWh)")

plot(ksspr$DATES, ksspr$KYUSHU.DAILY, type = "l", main = "Spring Kyushu Price", xlab = "Time", ylab = "Price (JPY/kWh)"  )

adf.test(ksspr$KYUSHU.DAILY)
kpss.test(ksspr$KYUSHU.DAILY)


acf(ksspr$KYUSHU.DAILY, main = "ACF of Spring Kyushu") 
pacf(ksspr$KYUSHU.DAILY, main = "PACF of Spring Kyushu")


################################
ksspr_ts <- ts(ksspr$KYUSHU.DAILY, start = c(1,85), frequency = 7)
diffksspr_ts <- diff(ksspr_ts, differences = 1, lag = 7)
plot.ts(ksspr_ts, ylim=c(-8,15),col = "purple", main = "Spring KSH original vs. seasonal difference", ylab = "Price (JPY/kWh)") 
lines(diffksspr_ts, col = "orange") 
legend("topright", legend=c("Original Price", "Seasonal difference"),
       col=c("purple","orange"), lty=1:1, cex=0.6)
df <- data.frame(ksspr$DATES[-1][-1][-1][-1][-1][-1][-1],diffksspr_ts)

ggplot() + geom_line(data = ksspr, aes(x=DATES, y = KYUSHU.DAILY, colour="Original Price")) + geom_line(data = df, aes(x =ksspr$DATES[-1][-1][-1][-1][-1][-1][-1] , y = diffksspr_ts, colour="seasonal difference"))


acf(diffksspr_ts, main = "spring KSH seasonal difference")
pacf(diffksspr_ts, main = "spring KSH seasonal difference")
#######################################
diffksspr <- diff(diffksspr_ts, differences = 1)
plot(diffksspr, main = "Spring KSH first and seasonal difference", xlab = "Time", ylab= "")
acf(diffksspr, main = "ACF of Spring KSH transformed")
pacf(diffksspr, main = "PACF of Spring KSH transformed")
adf.test(diffksspr)
############################

decomp = mstl(ksspr_ts, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp, main = "Spring KSH trend and seasonality decomposition")

#############################
fit12 <- arima(ksspr_ts,order = c(1,1,10), seasonal = c(0,1,3))
summary(fit12)
tsdisplay(residuals(fit12), lag.max = 90, main= 'Spring KSH
          ARIMA (1,1,10)(0,1,3) [7] model residuals')
hist(fit12$residuals, freq = F, main = "Histogram of Spring KSH residuals
  ARIMA(1,1,10)(0,1,3)[7]", xlab= "Residuals", ylim = c(0, 0.4))
lines(density(fit12$residuals), col="red")
Box.test(fit12$residuals, lag=40, type="Ljung-Box", fitdf =17)

fit3 <- arima(ksspr_ts,order = c(1,1,8), seasonal = c(3,1,2))
summary(fit3)
tsdisplay(residuals(fit3), lag.max = 90, main= 'ARIMA (1,1,8)(3,1,2) 7 model residuals')
Box.test(fit1$residuals, lag=40, type="Ljung-Box", fitdf =14)


fit22 <- auto.arima(ksspr_ts, seasonal = T)
summary(fit22)
tsdisplay(residuals(fit22), lag.max = 85)
Box.test(fit22$residuals, lag=10, type="Ljung-Box", fitdf =2)
fit12$aic
fit22$aic          
fit2$bic
fit1$b
##########################
hold <- window(ksspr_ts, start = 20 )
??window
fit_no_holdout = arima(ksspr_ts[-c(50:68)], order = c(1,1,8), seasonal = c(1,1,2))
fcast_no_holdout <- forecast(fit_no_holdout, h = 7)
plot(fcast_no_holdout, ylim = c(-30, 70))
plot(ksspr_ts, col = 2)
lines(diffksspr_ts, col = 3)

####################
fmod12 = forecast(fit12, h = 7)
fmod22 = forecast(fit22, h = 7)
plot(fmod12, main = "7 - day forecast for Spring KSH
     ARIMA(1,1,10)(0,1,3)[7]", xlab = "Time", ylab = "Price (JPY/kWh)")
plot(fmod22, main = "7 - day forecast for Spring KSH
     AUTO.ARIMA(0,1,2)", xlab = "Time", ylab = "Price (JPY/kWh)")
fmod12
fmod22
write.csv(x = fmod12, file = "spring ksh.csv")

