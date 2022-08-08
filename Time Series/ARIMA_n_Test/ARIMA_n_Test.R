library("forecast")
library("tseries")

setwd("~/Desktop/...")

# Example
hsi = read.csv(file = "HSI.csv", header = T)
time_series_hsi = hsi[,6]
time_series_log_hsi = diff(log(time_series_hsi), lag = 1)
Box.test(time_series_log_hsi, lag = 12, type = ("Ljung-Box")) ### P.9
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,aic
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_AIC = 0}
    ## print the AIC with p and q
    fit=Arima(time_series_log_hsi,order=c(p,0,q))
    print(paste0("AIC is ", AIC(fit), " for p = ", p, " and q = ", q))
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

auto.arima(time_series_log_hsi, ic = 'bic', stationary = TRUE,
           start.p = 0, start.q = 0, start.P = 0, start.Q = 0,
           stepwise = FALSE) # WRONG
time_series_log_hsi_AR4 = arima(time_series_log_hsi, order=c(4,0,0)) ### P.9 ... 3 after checking the best model
Acf(time_series_log_hsi_AR4$residuals, plot = F)  
Box.test(time_series_log_hsi_AR4$residuals, lag = 18, fitdf = 4, type = ("Ljung-Box")) 
predict(time_series_log_hsi_AR4, n.ahead = 4, se.fit = TRUE)


## import data 
mortgage=read.table("m-mortg.txt")
mortgage_value = mortgage[,4]

## taking log
log_mortgage = log(mortgage_value)
## Plot data
ts.plot(log_mortgage)
## Checking stationary
adf.test(log_mortgage, alternative = "stationary")
## Take the log difference
log_diff_mortgage = diff(log_mortgage)
ts.plot(log_diff_mortgage)
## Check stationary
adf.test(log_diff_mortgage, alternative = "stationary")
## Check lowest aic, aicc, bic, value
for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,aic
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_AIC = 0}
    ## print the AIC with p and q
    fit=Arima(log_diff_mortgage,order=c(p,0,q))
    print(paste0("AIC is ", AIC(fit), " for p = ", p, " and q = ", q))
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
    ## initilize p,q,aic
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_BIC = 0}
    ## print the BIC with p and q
    fit=Arima(log_diff_mortgage,order=c(p,0,q))
    print(paste0("BIC is ", BIC(fit), " for p = ", p, " and q = ", q))
    ## update p,q,aic
    if(BIC(fit) < largest_BIC){
      largest_p = p
      largest_q = q
      largest_BIC = BIC(fit)
    }
    ## cout the finalised AIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest BIC is ", largest_BIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}

## Construct fitted model and check the residual
fit_log_diff_mortgage_1 = arima(log_diff_mortgage, order=c(3,0,0))
fit_log_diff_mortgage_2 = arima(log_diff_mortgage, order=c(0,0,1))
Box.test(fit_log_diff_mortgage_1$residuals, lag = 15, fitdf = 3, type = ("Ljung-Box"))
Box.test(fit_log_diff_mortgage_2$residuals, lag = 15, fitdf = 3, type = ("Ljung-Box"))
## Check the residual differences
for(p in 4:15){
  r1 = Box.test(fit_log_diff_mortgage_1$residuals, lag = p, fitdf = 3, type = ("Ljung-Box"))
  r2 = Box.test(fit_log_diff_mortgage_2$residuals, lag = p, fitdf = 3, type = ("Ljung-Box"))
  
  print(paste0("ARMA (3,0) Q (", p-3, ") p-value", r1$p.value))
  print(paste0("ARMA (0,1) Q (", p-3, ") p-value", r2$p.value))
}

## fited model
fit_log_mortgage = arima(log_mortgage, order=c(3,1,0))
## Prediction
predict(fit_log_mortgage, n.ahead = 4, se.fit = TRUE)


# arima(y, order = c(0, 0, 0), seasonal = c(0, 0, 0), xreg = NULL,
#      include.mean = TRUE, include.drift = FALSE, include.constant,
#      lambda = model$lambda, biasadj = FALSE, method = c("CSS-ML", "ML",
#                                                        "CSS"), model = NULL, x = y, ...)

#arima(x, order = c(0L, 0L, 0L),
#      seasonal = list(order = c(0L, 0L, 0L), period = NA),
#      xreg = NULL, include.mean = TRUE,
#      transform.pars = TRUE,
#      fixed = NULL, init = NULL,
#      method = c("CSS-ML", "ML", "CSS"), n.cond,
#      SSinit = c("Gardner1980", "Rossignol2011"),
#      optim.method = "BFGS",
#      optim.control = list(), kappa = 1e6)

## import data
dec_1 = read.table("m-dec1-8006.txt")
dec_1 = dec_1[,2]
## plot graph
ts.plot(dec_1)
## Fitted model
fit_dec_1 = arima(dec_1, order = c(0,0,1),
                  seasonal = list(order = c(1,0,1), period = 12),
                  method = c("CSS-ML", "ML", "CSS"))
## check the residual
for(p in 4:27){
  r_d_1 = Box.test(fit_dec_1$residuals, lag = p, fitdf = 3, type = ("Ljung-Box"))
  
  print(paste0("Q (", p-3, ") p-value", r_d_1$p.value))
}
## the finalized model
fit_dec_1

## import data
aaearn=read.table("q-aa-earn.txt")
aaearn = aaearn[,4]
## plot graph
ts.plot(aaearn)

## Calculate Quarterly mean
for(i in 1:14){
  if(i == 1)
  {
    first_quarter = aaearn[2]
  }
  first_quarter = first_quarter+aaearn[4*i+2]
}
mean1 = first_quarter/15

for(i in 1:14){
  if(i == 1)
  {
    second_quarter = aaearn[3]
  }
  second_quarter = second_quarter+aaearn[4*i+3]
}
mean2 = second_quarter/15

for(i in 1:14){
  if(i == 1)
  {
    third_quarter = aaearn[4]
  }
  third_quarter = third_quarter+aaearn[4*i+4]
}
mean3 = third_quarter/15

for(i in 1:15){
  if(i == 1)
  {
    fourth_quarter = aaearn[1]
  }
  fourth_quarter = fourth_quarter+aaearn[4*i+1]
}
mean4 = fourth_quarter/16

## combine the quarterly mean and plot the graph
all_mean = c(mean1, mean2, mean3, mean4)
plot(all_mean,type="b")

## Check stationary
adf.test(aaearn, alternative = "stationary")

### Consider the seasonal case
aaearn_s_diff4=diff(aaearn, 4)
adf.test(aaearn_s_diff4, alternative = "stationary")
aaearn_s_diff1and4 = diff(aaearn_s_diff4, 1)
adf.test(aaearn_s_diff1and4, alternative = "stationary")
## Plot acf, pacf
acf(aaearn_s_diff1and4)
pacf(aaearn_s_diff1and4)
## fitted model 
fitted_aaearn_s_diff1and4 = arima(aaearn_s_diff1and4, order = c(0,0,0), seasonal = list(order = c(0,0,0), period = 4))
## Check AIC and BIC
fitted_aaearn_s_diff1and4
AIC(fitted_aaearn_s_diff1and4)
BIC(fitted_aaearn_s_diff1and4)
## p-value
for(p in 1:12){
  p_a_1and4 = Box.test(fitted_aaearn_s_diff1and4$residuals, lag = p, type = ("Ljung-Box"))
  
  print(paste0("Q (", p, ") p-value ", p_a_1and4$p.value))
}


for(p in 0:6){
  for(q in 0:6){
    for(P in 0:6){
      for(Q in 0:6){
    ## initilize p,q,aic, Q, P
    if(p == 0 && q == 0 && P == 0 && Q == 0){
      largest_p = 0
      largest_q = 0
      largest_P = 0
      largest_Q = 0
      largest_AIC = 0}
    ## fit the model
    test_fitted_s_aaearn = arima(aaearn_s_diff1and4, order = c(p, 0, q),
                                 seasonal = list(order = c(P,0,Q), period = 4))
    ## update p,q,Q and P with the lowest AIC  model
    if(AIC(test_fitted_s_aaearn) < largest_AIC){
      largest_p = p
      largest_q = q
      largest_P = P
      largest_Q = Q
      largest_AIC = AIC(fit)
    }
    print(paste0(p,q,P,Q))
    ## cout the finalised AIC, q, p
    if(p ==6 && q == 6 && P ==6 && Q == 6)
    {print(paste0("Largest BIC is ", largest_AIC, " for p = ", largest_p, " and q = ", largest_q,
                  " for P = ", largest_P, " and Q = ", largest_Q))}
      }
    }
  }
}

fitted_s_aaearn = arima(aaearn, order = c(0, 1, 0),
                        seasonal = list(order = c(0,1,0), period = 4))
fitted_s_aaearn


### Consider the non-seasonal case
aaearn_diff1 = diff(aaearn, 1)
adf.test(aaearn_diff1, alternative = "stationary")

for(p in 0:6){
  for(q in 0:6){
    ## initilize p,q,aic
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_AIC = 0}
    ## print the AIC with p and q
    fit=Arima(aaearn_diff1,order=c(p,0,q))
    print(paste0("AIC is ", AIC(fit), " for p = ", p, " and q = ", q))
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
    ## initilize p,q,aic
    if(p == 0 && q == 0){
      largest_p = 0
      largest_q = 0
      largest_BIC = 0}
    ## print the BIC with p and q
    fit=Arima(aaearn_diff1,order=c(p,0,q))
    print(paste0("BIC is ", BIC(fit), " for p = ", p, " and q = ", q))
    ## update p,q,aic
    if(BIC(fit) < largest_BIC){
      largest_p = p
      largest_q = q
      largest_BIC = BIC(fit)
    }
    ## cout the finalised AIC, q, p
    if(p ==6 && q == 6)
    {print(paste0("Largest BIC is ", largest_BIC, " for p = ", largest_p, " and q = ", largest_q))}
  }
}
acf(aaearn_diff1)
pacf(aaearn_diff1)
## fit model residual checking 
ARMA400 = arima(aaearn_diff1, order = c(4,0,0))
ARMA400 
ARMA005 = arima(aaearn_diff1, order = c(0,0,5))
ARMA005
Box.test(ARMA400$residuals, lag = 16, fitdf = 4, type = ("Ljung-Box"))
Box.test(ARMA005$residuals, lag = 17, fitdf = 5, type = ("Ljung-Box"))
for(p in 5:16){
  r1 = Box.test(ARMA400$residuals, lag = p, fitdf = 4, type = ("Ljung-Box"))
  print(paste0("ARMA (4,0) Q (", p-4, ") p-value", r1$p.value))
}
for(p in 6:17){
  r1 = Box.test(ARMA005$residuals, lag = p, fitdf = 5, type = ("Ljung-Box"))
  print(paste0("ARMA (0,5) Q (", p-5, ") p-value", r1$p.value))
}

## non-seasonal fitted model
fitted_aaearn = arima(aaearn, order = c(4,1,0))
fitted_aaearn
## Non-seasonal fitted model prediction
predict(fitted_aaearn, n.ahead = 4, se.fit = TRUE)

## non-seasonal fitted model
fitted_s_aaearn = arima(aaearn, order = c(0, 1, 0), seasonal = list(order = c(0,1,0), period = 4))
fitted_s_aaearn
## Non-seasonal fitted model prediction
predict(fitted_s_aaearn, n.ahead = 4, se.fit = TRUE)

