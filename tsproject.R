#Decomposition model
#getwd()
tsproject = read.csv(file = "611project.csv", header = FALSE, sep = ",")
Gas.data = tsproject[,2]
Gas.ts <- ts(tsproject[,2], start = c(1992,1), end = c(2016,12), freq = 12)
Gas.tse <- ts(tsproject[,2], start = c(1992,1), end = c(2017,12), freq = 12)
plot(Gas.ts)
test.gas <- ts(tsproject[,2], start = c(1992, 1), end = c(2017, 12), frequency = 12)
test.gas = window(test.gas, start = c(2017, 1), end = c(2017, 12), frequency = 12)

#Additive model
par(mfrow=c(1,1))
#plot(decompose(Gas.ts))
acf(Gas.ts)
pacf(Gas.ts)
sd(Gas.ts)
Gas.dadd <- decompose(Gas.ts, type = "additive")
str(Gas.dadd)
plot(Gas.dadd$random)
acf(Gas.dadd$random)
#plot(Gas.dadd$trend)
#plot(Gas.dadd$seasonal)
#plot(Gas.dadd$random)
predda = Gas.dadd$trend + Gas.dadd$seasonal
layout(1:1)
plot(Gas.ts, lwd=2)
lines(predda, col = "red", lwd=2)
Gas.daddresid = window(Gas.dadd$random, start=c(1992, 7), end=c(2016, 6))
acf(Gas.daddresid)
rmse.dadd = sqrt(sum(Gas.daddresid^2)/length(Gas.daddresid))
rmse.dadd
tr.dadd = Gas.dadd$trend
plot(tr.dadd)
tim.dadd = time(tr.dadd)
tim.dadd
ti.dadd = unclass(tim.dadd)
ti.dadd
trreg1 = lm(tr.dadd ~ ti.dadd)
summary(trreg1)
plot(tr.dadd, lwd = 2, type='l')
lines(ti.dadd[7:294], fitted(trreg1), col='red', lwd=2)
predtime = seq(2017, 2017.917, by=1/12)
predtime
preddata1 = predict(trreg1, data.frame(ti.dadd = predtime), se.fit = TRUE)
preddata1
Gas.dadd$seasonal
season = c(-0.165838686, -0.127632089, -0.022583478,  0.057209925,  0.122956453,  0.131046730,  0.096753328,  0.092656105,  0.085411314, -0.004895978, -0.095651186, -0.169432436)
predvalues = preddata$fit
spredvalues = predvalues + season
prederror = preddata$se.fit
predupperse = spredvalues + 2*prederror
predlwrse = spredvalues - 2*prederror
resid.dadd = test.gas - spredvalues
resid.dadd
rmse.dadd = sqrt(sum(resid.dadd^2)/ length(resid.dadd))
rmse.dadd

#Multiplicative model
Gas.dmult <- decompose(Gas.ts, type = "mult")
plot(Gas.dmult)
plot(ts(Gas.dmult$random[7:294]))
#acf(Gas.dmult$random[7:294])
sd(Gas.ts[7:294])
sd(Gas.ts[7:294] - (Gas.dmult$trend[7:294]*Gas.dmult$seasonal[7:294]))
preddm = Gas.dmult$trend*Gas.dmult$seasonal
dmult.resid = Gas.ts - preddm
preddmresid = window(dmult.resid, start = c(1992, 7), end = c(2016, 6))
rmsedm = sqrt(sum(preddmresid^2)/ length(preddmresid))
rmsedm
tr.dmult = Gas.dmult$trend
plot(tr.dmult)
tim.dmult = time(tr.dmult)
ti.dmult = unclass(tim.dmult)
trreg2 = lm(tr.dmult ~ ti.dmult)
summary(trreg2)
plot(tr.dmult, lwd = 2, type='l')
lines(ti.dmult[7:294], fitted(trreg2), col='red', lwd=2)
predtime = seq(2017, 2017.917, by=1/12)
predtime
preddata = predict(trreg2, data.frame(ti.dmult = predtime), se.fit = TRUE)
preddata
#Gas.dmult$seasonal
season = c(0.9270145, 0.9403801, 0.9840679, 1.0231099, 1.0539975, 1.0587930, 1.0411766, 1.0403735, 1.0394977, 1.0026375, 0.9628965, 0.9260554)
predvalues = preddata$fit
spredvalues = predvalues * season
prederror = preddata$se.fit
predupperse = spredvalues + 2*prederror
predlwrse = spredvalues - 2*prederror
resid.dmult = test.gas - spredvalues
resid.dmult
rmse.dmult = sqrt(sum(resid.dmult^2)/ length(resid.dmult))
rmse.dmult

#STL Function Additive
stl.add <- stl(Gas.ts, s.window = 'periodic')
plot(stl.add)
stladd.pred = stl.add$time.series[,'seasonal'] + stl.add$time.series[,'trend']
plot(Gas.ts, lwd=2)
lines(stladd.pred, col='red', lwd=2)
stladd.rmse = sqrt(sum(stl.add$time.series[,'remainder']^2/length(stl.add$time.series[,'remainder'])))
stladd.rmse
#plot(stl.add$time.series[,'remainder'], ylab='Remainder')
tr.sadd = stl.add$time.series[,'trend']
#plot(tr.sadd)
tim.sadd = time(tr.sadd)
ti.sadd = unclass(tim.sadd)
trreg3 = lm(tr.sadd ~ ti.sadd)
summary(trreg3)
plot(tr.sadd, lwd = 2, type='l')
lines(ti.sadd, fitted(trreg3), col='red', lwd=2)
predtime = seq(2017, 2017.917, by=1/12)
predtime
preddata = predict(trreg3, data.frame(ti.sadd = predtime), se.fit = TRUE)
preddata
#stl.add$time.series[,'seasonal']
season = c(-0.16046903, -0.12521945, -0.02484988,  0.05270375,  0.11733735,  0.12723541,  0.09565351, 0.08998257,  0.08459164, -0.00213744, -0.09318650, -0.16164193)
predvalues = preddata$fit
spredvalues = predvalues + season
#prederror = preddata$se.fit
#predupperse = spredvalues + 2*prederror
#predlwrse = spredvalues - 2*prederror
resid.sadd = test.gas - spredvalues
resid.sadd
rmse.sadd = sqrt(sum(resid.sadd^2)/ length(resid.sadd))
rmse.sadd

#STL function Multiplicative
loggas = log(Gas.ts)
plot(loggas, ylab='log(Gas price)')
class(loggas)
stl.mult = stl(loggas, s.window = 'periodic')
plot(stl.mult)
stlmult.pred = stl.mult$time.series[,'seasonal']+stl.mult$time.series[, 'trend']
plot(loggas, lwd=2)
lines(stlmult.pred, col='red', lwd=2)
stlmult.resid = Gas.ts - exp(stlmult.pred)
stlmult.rmse = sqrt(sum(stlmult.resid^2)/length(stlmult.resid))
stlmult.rmse
tr.smult = stl.mult$time.series[,'trend']
#plot(tr.smult)
#tr.smult
tim.smult = time(tr.smult)
#tim.smult
ti.smult = unclass(tim.smult)
#ti.smult
smultreg = lm(tr.smult ~ ti.smult)
summary(smultreg)
plot(tr.smult, lwd = 2)
lines(ti.smult, fitted(smultreg), col = 'red', lwd=2)
predtime = seq(2017, 2017.917, by=1/12)
predtime
pred.smult=predict(smultreg, data.frame(ti.smult=predtime), se.fit = TRUE)
pred.smult
#stl.mult$time.series[,'seasonal']
season.smult = c(-0.073715095, -0.061369731,-0.017386538 ,0.021508037 , 0.051374291 , 0.057317330, 0.041344497 , 0.040767272 , 0.040207932 , 0.006917381 ,-0.034413096, -0.072552280)
#season.smult
predvalues = pred.smult$fit
#predvalues
spredvalues = predvalues + season.smult
#prederror=pred.smult$se.fit
#predupperse=spredvalues+2*prederror
#predlwrse=spredvalues-2*prederror
epredvalues=exp(spredvalues)
#epredupperse=exp(predupperse)
#epredlwrse=exp(predlwrse)
epredvalues
resid.smult = test.gas - epredvalues
resid.smult
rmse.stl = sqrt(sum(resid.smult^2)/ length(resid.smult))
rmse.stl


#REGRESSION MODEL
library(FinTS)
library(zoo)
require(lmtest)
require("sos")
library(sos)
findFn("coredata")
library(xts)
gtime = time(Gas.ts)
gtime
gseas = cycle(Gas.ts)
gseas
gdata = coredata(Gas.ts)
greg = lm(gdata ~ gtime + I(gtime^2)+ I(gtime^3)+ I(gtime^4) +factor(gseas))
summary(greg)

#All the variables are significant with p-value less than 0.05
par(mfrow=c(2,2))
plot(greg)
# The residuals vs fitted shows a bit of curvilinear trend
# Almost all the points are near the dashed line except for few outliers in QQ plot
# The red line has an upward trend
# The red smooth line is close to 0 with some curve in the end showing few influential or rather outlier points

egas = resid(greg)
ghat = fitted(greg)
par(mfrow=c(1,1))
plot(Gas.ts)
lines(gtime, ghat, col='red', type='l')
#Find out why this line is not working
hist(egas)
#Pretty much normal
acf(egas)
#Residuals are autocorrelated 
AutocorTest(egas, lag=1)
AutocorTest(egas, lag=log(length(egas)))
enddate = gtime[length(gtime)]
enddate
predtime=seq(2017, 2017.917, by=1/12)
predtime
predseas=seq(1:12)
predseas
preddata = predict(greg, data.frame(gtime=predtime, gseas=predseas), se.fit = TRUE)
preddata
predvalues=preddata$fit
prederror=preddata$se.fit
predupperse=predvalues+2*prederror
predlwrse=predvalues-2*prederror
plot(gtime, Gas.ts, type='l')
lines(gtime, ghat, col='red', type='l')
points(predtime, predvalues, col='red')
lines(predtime, predupperse, col='blue')
lines(predtime, predlwrse, col='blue')
resid.greg = test.gas - predvalues
resid.greg
rmse.greg = sqrt(sum(resid.greg^2)/ length(resid.greg))
rmse.greg
greg2 = lm(gdata~0+gtime+I(gtime^2)+factor(gseas))
summary(greg2)
par(mfrow=c(2,2))
plot(greg2)
preddata2 = predict(greg2, data.frame(gtime=predtime, gseas=predseas), se.fit = TRUE)
preddata2
predvalues2=preddata2$fit
resid.greg2 = test.gas - predvalues2
resid.greg2
rmse.greg2 = sqrt(sum(resid.greg2^2)/ length(resid.greg2))
rmse.greg2

#HOLT WINTERS 
Gasw.hw = HoltWinters(Gas.ts, seasonal = "mult")
Gasw.hw
Gasw.hw$coef
Gasw.hw$SSE
sqrt(Gasw.hw$SSE/(length(Gas.data)-12))
sd(Gas.data)
regastw = Gas.ts - Gasw.hw$fitted[,'xhat']
rmse = sqrt(sum(regastw^2)/length(regastw))
rmse
Gasw.predict = predict(Gasw.hw, n.ahead = 1*12)
Gasw.predict
ts.plot(Gas.ts, Gasw.predict, lty = 1:2)
resid.hw = test.gas - Gasw.predict
resid.hw
rmse.hw = sqrt(sum(resid.hw^2)/ length(resid.hw))
rmse.hw
acf(regastw)

#Entire data set
Gasw.hw = HoltWinters(Gas.tse, seasonal = "mult")
Gasw.hw
Gasw.hw$coef
Gasw.hw$SSE
sqrt(Gasw.hw$SSE/(length(Gas.data)))
sd(Gas.data)
regastw = Gas.tse - Gasw.hw$fitted[,'xhat']
rmse = sqrt(sum(regastw^2)/length(regastw))
rmse
acf(regastw)
# Gasw.predict = predict(Gasw.hw, n.ahead = 1*12)
# Gasw.predict
# ts.plot(Gas.ts, Gasw.predict, lty = 1:2)
# resid.hw = test.gas - Gasw.predict
# resid.hw
# rmse.hw = sqrt(sum(resid.hw^2)/ length(resid.hw))
# rmse.hw



#AR Model
Z.ar = ar(Gas.ts)
Z.ar$order
Z.ma = arima(Gas.ts, order = c(0,0,1))
Z.ma
Z.arma = arima(Gas.ts, order = c(12,0,1))
Z.arma
best.order = c(0,0,0)
best.aic = Inf
for (i in 0:2) for (j in 0:2) {
  fit.aic = AIC(arima(resid(Gas.ts), order = c(i, 0, j)))
  if (fit.aic < best.aic) {
    best.order = c(i,0,j)
    best.arma = arima(resid(Gas.ts), order = best.order)
    best.aic = fit.aic
  }
}
best.order

#Changepoint analysis
install.packages("changepoint")
library(changepoint)
mvalue = cpt.mean(Gas.tse, method="PELT") #mean changepoints using PELT
cpts(mvalue)
tsproject[158,]
plot(mvalue)
vvalue = cpt.var(Gas.tse, method="PELT")
cpts(vvalue)
tsproject[c(229,275), ]
plot(vvalue)
mvalue = cpt.mean(Gas.tse, method="BinSeg")
cpts(mvalue)
plot(mvalue)
vvalue = cpt.var(Gas.tse, method="BinSeg")
cpts(vvalue)
plot(vvalue)
par(mfrow=c(2,1))
plot(Gas.ts)
plot(vvalue)

