summary(kssum)

kssum$DATES = seq.Date(as.Date("2022-06-01"),
                       as.Date("2022-08-24"),
                       by="days")
hist(kssum$KYUSHU.DAILY, freq = F, main = "Summer Kyushu distribution", xlab = "Price (JPY/kWh)")

plot(kssum$DATES, kssum$KYUSHU.DAILY, type = "l", main = "Summer Kyushu Price", xlab = "Time", ylab = "Price (JPY/kWh)"  )
df2 <- data.frame(kssum$DATES[-1][-1][-1][-1][-1][-1][-1],diffkssum_ts)
ggplot() + geom_line(data = kssum, aes(x=DATES, y = KYUSHU.DAILY, colour="Original Price ")) + xlab("Time") + geom_line(data = df2, aes(x = kssum$DATES[-1][-1][-1][-1][-1][-1][-1] , y = diffkssum_ts, colour="seasonal difference")) + ylab("Price (JPY/kWh)")


adf.test(kssum$KYUSHU.DAILY)
kpss.test(kssum$KYUSHU.DAILY)


acf(kssum$KYUSHU.DAILY, main = "ACF of Summer Kyushu") 
pacf(kssum$KYUSHU.DAILY, main = "PACF of Summer Kyushu")


################################
kssum_ts <- ts(kssum$KYUSHU.DAILY, start = c(1,85), frequency = 7)
diffkssum_ts <- diff(kssum_ts, differences = 1, lag = 7)
plot.ts(kssum_ts, ylim=c(-15,30),col="purple", main = "Summer KSH original vs. seasonal difference", ylab = "Price (JPY/kWh)") 
lines(diffkssum_ts, col = "orange") 
legend("topright", legend=c("Original Price ", "Seasonal difference"),
       col=c("purple","orange"), lty=1:1, cex=0.7)

acf(diffkssum_ts, main = "Summer KSH seasonal difference")
pacf(diffkssum_ts, main = "Summer KSH seasonal difference")
#######################################
diffkssum <- diff(diffkssum_ts, differences = 1)
plot(diffkssum, main = "Summer KSH first and seasonal difference", xlab = "Time", ylab= "")
acf(diffkssum, main = "ACF of Summer KSH transformed")
pacf(diffkssum, main = "PACF of Summer KSH transformed")
############################

decomp = mstl(kssum_ts, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp, main = "Summer KSH trend and seasonality decomposition")

#############################
fit14 <- arima(kssum_ts,order = c(2,1,8), seasonal = c(0,1,3))
summary(fit14)
tsdisplay(residuals(fit14), lag.max = 90, main= 'Summer KSH
          ARIMA (2,1,8)(0,1,3) [7] model residuals')
hist(fit14$residuals,main = "Histogram of Summer KSH residuals
  ARIMA(2,1,8)(0,1,3)[7]", freq = F, xlab= "Residuals", ylim = c(0, 0.25))
lines(density(fit14$residuals), col="red")
Box.test(fit14$residuals, lag=40, type="Ljung-Box", fitdf =16)

fit34 <- arima(kssum_ts,order = c(2,1,7), seasonal = c(0,1,3,7))
summary(fit34)
tsdisplay(residuals(fit3), lag.max = 90, main= 'ARIMA (2,1,7)(0,1,3) 7 model residuals')
Box.test(fit1$residuals, lag=40, type="Ljung-Box", fitdf =14)


fit24 <- auto.arima(kssum_ts, seasonal = T)
summary(fit24)
tsdisplay(residuals(fit24), lag.max = 85)
Box.test(fit24$residuals, lag=10, type="Ljung-Box", fitdf =2)
fit14$aic
fit24$aic          
fit34$bic
fit1$b
##########################
df <- data.frame(kssum_ts)
train <- subset(df, end=length(df)-61)
test <- subset(df, start=length(df)-60)
kssum.train <- Arima(training, order=c(2,1,8),
                    seasonal=c(0,1,3,7), lambda=0)
cafe.train %>%
  forecast(h=60) %>%
  autoplot() + autolayer(test)
####################
fmod14 = forecast(fit14, h = 7)
fmod24 = forecast(fit24, h = 7)
plot(fmod14,main = "7 - day forecast for Summer KSH
     ARIMA(2,1,8)(0,1,3)[7]", xlab = "Time", ylab = "Price (JPY/kWh)")
plot(fmod24, main = "7 - day forecast for Summer KSH
     AUTO.ARIMA(2,0,0)(0,1,1)[7]", xlab = "Time", ylab = "Price (JPY/kWh)")
fmod14
fmod24
write.csv(x = fmod14, file = "summer ksh.csv")

fit11$aic
fit12$aic
fit13$aic
fit14$aic

fit21$aic
fit22$aic
fit23$aic
fit24$aic
