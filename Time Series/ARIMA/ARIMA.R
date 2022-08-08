library("forecast")
library("tseries")

setwd("~/Desktop/...)
hsi = read.csv(file = "HSI.csv", header = T)

#### index = hsi[,2] != "null"
##### hsi_remove_null = hsi[index,]
time_series_hsi = ts(hsi[,6])
plot.ts(time_series_hsi)

time_series_log_hsi = diff(log(time_series_hsi))
plot.ts(time_series_log_hsi)
Acf(time_series_log_hsi, plot = F)
Pacf(time_series_log_hsi, plot = F)
Box.test(time_series_log_hsi, lag = 12, type = ("Ljung-Box"))

setwd("~/Desktop/...")
m_dec19 = read.table("m-dec19.txt", header = T)
dec1 = m_dec19[,2]
Acf(dec1, plot = F)
Pacf(dec1, plot = F)
Box.test(dec1, lag = 12, type = ("Ljung-Box"))

dec2 = m_dec19[,3]
Acf(dec2, plot = F)
Box.test(dec2, lag = 12, type = ("Ljung-Box"))

cpi = read.table("m-cpileng.txt", header = F)
cpi_data = cpi[,4]
cpi_growth_rate = 100*diff(log(cpi_data))
Acf(cpi_growth_rate, plot = F)
Pacf(cpi_growth_rate, plot = F)
Box.test(cpi_growth_rate, lag = 12, type = ("Ljung-Box"))
cpi_growth_rate_diff = diff(cpi_growth_rate)
Acf(cpi_growth_rate_diff, plot = F)
cpi_ARIMA15 = arima(cpi_growth_rate, order=c(1,0,5), include.mean = FALSE)
cpi_ARIMA15
FALSEcpi_ARMA15 = arma(cpi_growth_rate, order = c(1,5))
cpi_ARMA15

gnp = read.table("q-gnprate.txt", header = F)
gnp_AR3 = arima(gnp, order=c(3,0,0))

gnp=scan(file="http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/dgnp82.txt")
gnp1=ts(gnp,frequency=4,start=c(1947,2))
par(mfrow=c(1,1))
plot(gnp1)
points(gnp1,pch="*")
m1=ar(gnp,method="mle")
m2=arima(gnp,order=c(3,0,0))
(1-.348-.1793+.1423)*0.0077
sqrt(m2$sigma2)
p1=c(1,-m2$coef[1:3])
Mod(roots)
roots = polyroot(p1)
k=2*pi/acos(1.590253/1.913308)

sqrt(gnp_AR3$sigma2)
gnp_AR3_p1=c(1,-gnp_AR3$coef[1:3])
gnp_AR3_p1_roots = polyroot(gnp_AR3_p1)
Mod(gnp_AR3_p1_roots)
gnp_AR3_k=2*pi/acos(1.542931613/1.800682525)

gnp_pred1to4 = predict(gnp_AR3, n.ahead = 4, se.fit = TRUE)
gnp_pred1to4

dec2_ma1 = arima(dec2, order=c(0,0,1))
dec2_ma1

dec2_ma2 = arima(dec2, order=c(0,0,2))
dec2_ma2
dec2_ma1

dec2_pred1to4 = predict(dec2_ma1, n.ahead = 4, se.fit = TRUE)
dec2_pred1to4

