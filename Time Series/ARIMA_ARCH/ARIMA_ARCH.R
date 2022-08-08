library("forecast")
library("fGarch")
library("aTSA")
library("rugarch")
library("tseries")
library("MTS")
library("FinTS")
library("nardl")

# Import data
data = read.table("/Users/.../d-sbuxsp0106.txt")

SBUX_return = data[,2]
SNP_return = data[,3]

# change to log return
log_SBUX_return = log(SBUX_return + 1) * 100
log_SNP_return = log(SNP_return + 1) * 100

# check the serial correlation
Acf(log_SBUX_return)
acf(log_SBUX_return, plot = FALSE)
pacf(log_SBUX_return)
for (i in c(6,12,18,24)) {
  print(Box.test(log_SBUX_return, type = "Ljung-Box", lag = i)$p.value)
}
arima(log_SBUX_return, order = c(0,0,0))

# Check for the ARCH effect
# LM
for (i in c(1:12)) {
  print(ArchTest(log_SBUX_return,lags=i))
}
# Q
for (i in c(1:12)) {
  print(paste0("df = ", i))
  print(archTest(log_SBUX_return,lag=i))
}

# fit GARCH (1,1) model
# ARIMA Model Checking
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,aic
    fit=Arima(log_SBUX_return,order=c(p,0,q))
    print(paste0("AIC is ", AIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_AIC = AIC(fit)}
    ## update p,q,aic
    if(AIC(fit) < largest_AIC){
      largest_p = p
      largest_q = q
      largest_AIC = AIC(fit)
    }
    ## cout the finalised AIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest AIC is ", largest_AIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,bic
    fit=Arima(log_SBUX_return,order=c(p,0,q))
    print(paste0("BIC is ", BIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_BIC = BIC(fit)}
    ## update p,q,bic
    if(BIC(fit) < largest_BIC){
      largest_p = p
      largest_q = q
      largest_BIC = BIC(fit)
    }
    ## cout the finalised BIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest BIC is ", largest_BIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
fit = arima(log_SBUX_return, order = c(4,0,5))
for (i in c(10:21)) {
  print(Box.test(fit$residuals, lag = i, fitdf = 9, type = ("Ljung-Box")))
  }
fit2 = arima(log_SBUX_return, order = c(0,0,1))
for (i in c(2:13)) {
  print(Box.test(fit2$residuals, lag = i, fitdf = 1, type = ("Ljung-Box")))
}

GARCH11 = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,1)))
SBUS_GARCH11 = ugarchfit(spec = GARCH11, data = log_SBUX_return)
SBUS_GARCH11





# Series correlation checking
Acf(log_SNP_return)
pacf(log_SNP_return)
acf(log_SNP_return, plot = FALSE)
for (i in c(6,12,18,24)) {
  print(Box.test(log_SNP_return, type = "Ljung-Box", lag = i)$p.value)
}

# Check for the ARCH effect
# LM
for (i in c(1:12)) {
  print(ArchTest(log_SNP_return,lags=i))
}
# Q
for (i in c(1:12)) {
  print(paste0("df = ", i))
  print(archTest(log_SNP_return,lag=i))
}
# fit GARCH (1,1) model
# ARIMA Model Checking
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,aic
    fit=Arima(log_SNP_return,order=c(p,0,q))
    print(paste0("AIC is ", AIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_AIC = AIC(fit)}
    ## update p,q,aic
    if(AIC(fit) < largest_AIC){
      largest_p = p
      largest_q = q
      largest_AIC = AIC(fit)
    }
    ## cout the finalised AIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest AIC is ", largest_AIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,bic
    fit=Arima(log_SNP_return,order=c(p,0,q))
    print(paste0("BIC is ", BIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_BIC = BIC(fit)}
    ## update p,q,bic
    if(BIC(fit) < largest_BIC){
      largest_p = p
      largest_q = q
      largest_BIC = BIC(fit)
    }
    ## cout the finalised BIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest BIC is ", largest_BIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
fit = arima(log_SNP_return, order = c(6,0,3))
for (i in c(10:21)) {
  print(Box.test(fit$residuals, lag = i, fitdf = 9, type = ("Ljung-Box")))
}
fit2 = arima(log_SNP_return, order = c(0,0,0))
for (i in c(1:12)) {
  print(Box.test(fit2$residuals, lag = i, fitdf = 0, type = ("Ljung-Box")))
}
fit3 = arima(log_SNP_return, order = c(0,0,1))
for (i in c(2:13)) {
  print(Box.test(fit3$residuals, lag = i, fitdf = 1, type = ("Ljung-Box")))
}

IGARCH11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0)))
IGARCH11_log_SNP = ugarchfit(spec = IGARCH11, data = log_SNP_return)
IGARCH11_log_SNP

# Forecast 
forecast_log_SNP = ugarchforecast(IGARCH11_log_SNP, n.ahead = 4)
forecast_log_SNP

IGARCH11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(0,1)))
IGARCH11_log_SNP = ugarchfit(spec = IGARCH11, data = log_SNP_return)
IGARCH11_log_SNP

# Forecast 
forecast_log_SNP = ugarchforecast(IGARCH11_log_SNP, n.ahead = 4)
forecast_log_SNP






# GARCH(1,1)-M fit
GARCH11_M = ugarchspec(variance.model=list(model="sGARCH"),
                  mean.model=list(armaOrder=c(0,1),archm=TRUE))

GARCH11_M_log_SNP = ugarchfit(spec = GARCH11_M, data = log_SBUX_return)
GARCH11_M_log_SNP

# EGARCH(1,1) fit
EGARCG11 = ugarchspec(variance.model=list(model="eGARCH"),
                      mean.model=list(armaOrder=c(0,1)))

EGARCH11_log_SNP = ugarchfit(spec = EGARCG11, data = log_SBUX_return)
EGARCH11_log_SNP



# 
data = read.table("/.../m-pg5606.txt")

PNG = data[,2]

# change to log return
log_PNG = log(PNG + 1) * 100

# series checking
Acf(log_PNG)
acf(log_PNG, plot = FALSE)
pacf(log_PNG)
for (i in c(6,12,18,24)) {
  print(Box.test(log_PNG, type = "Ljung-Box", lag = i)$p.value)
}
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,aic
    fit=Arima(log_PNG,order=c(p,0,q))
    print(paste0("AIC is ", AIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_AIC = AIC(fit)}
    ## update p,q,aic
    if(AIC(fit) < largest_AIC){
      largest_p = p
      largest_q = q
      largest_AIC = AIC(fit)
    }
    ## cout the finalised AIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest AIC is ", largest_AIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,bic
    fit=Arima(log_PNG,order=c(p,0,q))
    print(paste0("BIC is ", BIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_BIC = BIC(fit)}
    ## update p,q,bic
    if(BIC(fit) < largest_BIC){
      largest_p = p
      largest_q = q
      largest_BIC = BIC(fit)
    }
    ## cout the finalised BIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest BIC is ", largest_BIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
# Test case
fit = arima(log_PNG, order = c(1,0,3))
for (i in c(5:16)) {
  print(Box.test(fit$residuals, lag = i, fitdf = 4, type = ("Ljung-Box")))
}
fit2 = arima(log_PNG, order = c(0,0,0))
for (i in c(1:12)) {
  print(Box.test(fit2$residuals, lag = i, fitdf = 0, type = ("Ljung-Box")))
}
fit3 = arima(log_PNG, order = c(1,0,1))
for (i in c(2:13)) {
  print(Box.test(fit3$residuals, lag = i, fitdf = 1, type = ("Ljung-Box")))
}
fit4 = arima(log_PNG, order = c(0,0,1))
for (i in c(2:13)) {
  print(Box.test(fit4$residuals, lag = i, fitdf = 1, type = ("Ljung-Box")))
}
fit5 = arima(log_PNG, order = c(1,0,0))
for (i in c(2:13)) {
  print(Box.test(fit5$residuals, lag = i, fitdf = 1, type = ("Ljung-Box")))
}
# model fitting
GARCH11 = ugarchspec(variance.model=list(model="sGARCH"),
                  mean.model=list(armaOrder=c(1,1)))
GARCH11_log_PNG = ugarchfit(spec = GARCH11, data = log_PNG)
GARCH11_log_PNG

forc_log_PNG = ugarchforecast(GARCH11_log_PNG, n.ahead=5)
forc_log_PNG

# Different ARIMA
GARCH11 = ugarchspec(variance.model=list(model="sGARCH"),
                     mean.model=list(armaOrder=c(0,0)))
GARCH11_log_PNG = ugarchfit(spec = GARCH11, data = log_PNG)
GARCH11_log_PNG

forc_log_PNG = ugarchforecast(GARCH11_log_PNG, n.ahead=5)
forc_log_PNG



# Import data
data = read.table("/.../d-exuseu.txt")
# Log different 
exr = diff(log(data[,4]))*100

# check the series correlation
Acf(exr)
pacf(exr)
acf(exr, plot = FALSE)
for (i in c(6,12,18,24)) {
  print(Box.test(exr, type = "Ljung-Box", lag = i)$p.value)
}

# Check for the ARCH effect
# LM
for (i in c(1:12)) {
  print(ArchTest(exr,lags=i))
}
#
for (i in c(1:12)) {
  print(paste0("df = ", i))
  print(archTest(exr,lag=i))
}

# IGARCG (1,1)
# ARIMA Model Checking
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,aic
    fit=Arima(exr,order=c(p,0,q))
    print(paste0("AIC is ", AIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_AIC = AIC(fit)}
    ## update p,q,aic
    if(AIC(fit) < largest_AIC){
      largest_p = p
      largest_q = q
      largest_AIC = AIC(fit)
    }
    ## cout the finalised AIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest AIC is ", largest_AIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,bic
    fit=Arima(exr,order=c(p,0,q))
    print(paste0("BIC is ", BIC(fit), " for p = ", p, " and q = ", q))
    
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_BIC = BIC(fit)}
    ## update p,q,bic
    if(BIC(fit) < largest_BIC){
      largest_p = p
      largest_q = q
      largest_BIC = BIC(fit)
    }
    ## cout the finalised BIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest BIC is ", largest_BIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
fit = arima(exr, order = c(2,0,6))
for (i in c(9:20)) {
  print(Box.test(fit$residuals, lag = i, fitdf = 8, type = ("Ljung-Box")))
}
fit2 = arima(exr, order = c(0,0,0))
for (i in c(1:12)) {
  print(Box.test(fit2$residuals, lag = i, fitdf = 0, type = ("Ljung-Box")))
}
fit3 = arima(exr, order = c(0,0,1))
for (i in c(2:13)) {
  print(Box.test(fit3$residuals, lag = i, fitdf = 1, type = ("Ljung-Box")))
}
fit4 = arima(exr, order = c(1,0,0))
for (i in c(2:13)) {
  print(Box.test(fit3$residuals, lag = i, fitdf = 1, type = ("Ljung-Box")))
}
fit5 = arima(exr, order = c(1,0,1))
for (i in c(3:14)) {
  print(Box.test(fit3$residuals, lag = i, fitdf = 2, type = ("Ljung-Box")))
}

IGARCH11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0)))
IGARCH11_exr = ugarchfit(spec = IGARCH11, data = exr)
IGARCH11_exr

# Forecasting
forc = ugarchforecast(IGARCH11_exr, n.ahead=4)
forc
