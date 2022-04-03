# Loading the Required Libraries
library(readr)
library(ggfortify)
library(tseries)
library(Metrics)
library(ggplot2)
library(forecast)
library(TTR)
library("fUnitRoots")
library(dplyr)
require(graphics)


yahoo = read.csv("C:/Users/Ashish/Downloads/yahoo.csv", header = T, stringsAsFactors = F)
class(yahoo)
colnames(yahoo)
head(yahoo)
class(yahoo$Date)
class(yahoo$Adj.Close)
which(yahoo$Date == "2019-05-03")
which(yahoo$Date == "2020-05-01")
yahoo[1:252,]

# Q.2. Build a timeSeries object with the data.
yahoo.ts = ts(data = yahoo$Adj.Close, frequency = 12, start = c(2019-05-03), end=c(2020-05-01))
class(yahoo.ts)
str(yahoo.ts)
yahoo.ts
start(yahoo.ts)
end(yahoo.ts)
#The time() function extracts the time index as a ts object
time(yahoo.ts) 
#The frequency per period and time interval between observations of a ts object
frequency(yahoo.ts)
deltat(yahoo.ts)
#This will plot a time series of the data.
par(mar=c(1,1,1,1))
plot(yahoo.ts, col="blue", lwd=2, xlab="Data",ylab="Adjusted close", main="Monthly closing price of SBUX")
#plot a subset of the data use the window() function inside of plot()
par(mar=c(1,1,1,1))
plot(window(yahoo.ts, start = c(2019-05-03), end=c(2020-05-01), ylab="Adjusted close",col="blue", lwd=2, main="Monthly closing price of SBUX"))
#This will fit a line (also called trend line) shown in below figure.
#par(mar=c(1,1,1,1))
abline(reg = lm(yahoo.ts~time(yahoo.ts)),col="orange")

#-----------------------------------------
# Q.3. It will plot the yearly mean values

plot(aggregate(yahoo.ts,FUN = mean))

#-----------------------------------------
#Ques 4. Boxplot across Quarters of month
boxplot(yahoo.ts~cycle(yahoo.ts),xlab="Date", ylab = "Adjusted close", main ="Monthly closing price of SBUX")
#Data Cleaning : Since in the above box plot there was an outlier so I use tsclean() to remove the outlier.
yahoo1=tsclean(yahoo.ts)
boxplot(yahoo1~cycle(yahoo1), xlab="Date", ylab = "Adjusted close", main ="Boxplot with no outliers")
#-----------------------------------------
#Ques 5. Decomposition of Cleaned data (yahoo1) using stl function
yahoo.ts_stl <- stl(yahoo1, s.window = "periodic")
#par(mar=c(1,1,1,1))
plot(yahoo.ts_stl, main = "Time series decompostion using stl")

#------------------------------------------
# Ques 6. Type of Seasonality : yearly since the period is 12.
yahoo.ts_stl_seasonal <- yahoo.ts_stl$time.series[,1]#seasonal
plot((yahoo.ts_stl_seasonal),xlab="Data",ylab = "Adjusted close",main="Seasonal plot")
yahoo.ts_stl_trend <- yahoo.ts_stl$time.series #tread
plot((yahoo.ts_stl_trend),xlab="Data",ylab = "Adjusted close",main="Tread plot")
yahoo.ts_stl_random <- yahoo.ts_stl$time.series #random
plot((yahoo.ts_stl_random),xlab="Data",ylab = "Adjusted close",main="Tread plot")

#-------------------------------------------
# Ques 7. Residue after removing seasonality and trend
trend_yahoo.ts = ma(yahoo.ts,order = 12, centre = T)
plot(as.ts(yahoo.ts),col = "orange",xlab="Date", ylab = "Adjusted close", main ="removing seasonality")
lines(trend_yahoo.ts)
plot(as.ts(trend_yahoo.ts),col = "black",xlab="Date", ylab = "Adjusted close", main ="removing trend")
# Removing trend from time series :
Dtr = yahoo.ts/trend_yahoo.ts
plot(as.ts(Dtr),main="Time series after de-trending",col = "orange",xlab="Date", ylab = "Adjusted close")
#Removing seasonality
m = t(matrix(data = trend_yahoo.ts, nrow = 12))
seayahoo.ts = colMeans(m,na.rm = T)
plot((as.ts(rep(seayahoo.ts,4))), main ="Time series after de-seasonality")
res = yahoo.ts/ (trend_yahoo.ts * seayahoo.ts)
plot(as.ts(res),main="Residue after removing trending and seasonallity")

#--------------------------------------------
#Ques 8. Building Holt Winters Model and 
#Ques 12.Improving model by changing alpha,beta & gamma values
#Trainig Dataset
yahoo.ts_train <- ts(yahoo1, frequency = 12, start = c(2019-05-03), end=c(2020-05-01))
yahoo.ts_train
#Model 1: Seasonal HoltWinters Model
HWyahoo.ts <- HoltWinters(yahoo.ts_train)
HWyahoo.ts
plot(HWyahoo.ts, main = "Original time series agains the Predicted time series(ADD)")
HWyahoo.ts <- HoltWinters(yahoo.ts_train, seasonal ="multiplicative")
HWyahoo.ts
plot(HWyahoo.ts, main = "Orignal time series against the Predictedmn Time sries (MUL)")
plot(fitted(HWyahoo.ts))
#Model 2 : Simple Exponential Smoothing Model
#This model has no trend and zero seasonal components
Hotyahoo.ts <- HoltWinters(yahoo.ts_train, beta = F, gamma = F)
Hotyahoo.ts
Hotyahoo.ts$fitted #Store the forecasts made by HW
plot(Hotyahoo.ts, main = "Original time series against the Predicted time series")
#Model 3 : Non-seasonal Holt Winters
#It is better Prediction model than hotyahoo.ts
Hotyahoo.ts1 <- HoltWinters(yahoo.ts_train, gamma =F)
Hotyahoo.ts1
Hotyahoo.ts1$fitted
plot(Hotyahoo.ts1, main = "Original time series against the Predicted time series ")

#--------------------------------------
#Ques 9. Predict the values for the next 25% of the time
#Ques 10. Plot the predicted values along with the actual values to compare them.
HW_pred = predict(HWyahoo.ts, n.ahead = 3*12)
round(HW_pred) # predicted values for next  years
ts.plot(HW_pred, yahoo1, col = "blue")
H_pred = predict(Hotyahoo.ts,n.ahead = 3*12)
ts.plot(yahoo1,H_pred,col = "purple", main = "Original vs Predicted Values")
# method 2 : Predict values using forecast()
J_Pred = forecast(Hotyahoo.ts,h=36)
#par(mar=c(1,1,1,1))
plot(J_Pred,xlab="Date", ylab = "Adjusted close")
plot(J_Pred$residuals,xlab="Date", ylab = "Adjusted close")
summary(Hotyahoo.ts$fitted)
# Model 3
H_pred1 = predict(Hotyahoo.ts1,n.ahead = 3*12)
data3 = round(H_pred1) # predicted values for next 3 years
data4 = round(tail(yahoo1,36),0) # original values
#par(mar=c(1,1,1,1))
ts.plot(yahoo1,H_pred1,xlab="Date", ylab = "Adjusted close",col = "navyblue", main = "Original vs Predicted Values") # dotted predicted lines


# Plot of Predicted value vs Actual Quarterly Earnings for 3 years
X = time(data3)
Y1 = data.frame(data3)
Y2 = data.frame(data4)
df = tbl_df(data.frame(X,Y1,Y2))
df
#par(mar=c(1,1,1,1))
ggplot(df,aes(X))+
  geom_line(aes(y=data3),colour='red')+
  geom_line(aes(y=data4),colour='blue')+
  labs(y = "Earnings (in dollars)", x = "Data")+ 
  ggtitle("Comparison of predicted value of earnings and actual earnings")+
  scale_y_continuous(limits=c(0,40)) 
  geom_smooth(method="lm")
# Visualization
#par(mar=c(1,1,1,1))
plot.ts(data3, col="red", type ="l",xlim=c(2011,2019),ylim=c(0,40),xlab = "Time", ylab = "Earning (in dollars)")
par(new=TRUE)
plot.ts(data4, col="blue",type="l",xlim=c(2011,2019),ylim=c(0,40),xlab = "Time",ylab = "Earning (in dollars)")

#-------------------------------------------
# 11. RMSE (Root Mean Squared Error)
# rmse(predicted,actual) Note: Here, data4 contains the actual values of 25% data 

# Model 1 :
HWyahoo.ts$SSE
H_rmse = rmse(HW_pred,data4)
H_rmse

# Model 2 :
Hotyahoo.ts$SSE
HW_rmse = rmse(H_pred,data4)
HW_rmse

# Model 3 : 
Hotyahoo.ts1$SSE
HW1_rmse = rmse(H_pred1,data4)
HW1_rmse


#-------------------------------------
# 13. Arima Model trained using 75% of the given data (i.e., yahoo.ts_train)
arimayahoo.ts <- auto.arima(yahoo.ts_train)
arimayahoo.ts
# Since (p,d,q) = (0,1,1) We know that d = 0 => stationary data, hence using auot.arima(), we get best arima model
# Since d = 1, it will automatically differentiate the data i.e., diff(yahoo1) once to get stationary data 
ggtsdiag(arimayahoo.ts)


#-----------------------------------------
# 14. Predicted Values for next 25% of time
# Using predict()
predicted = predict(arimayahoo.ts,n.ahead = 3*12)
predicted # predicted values for next 3 years
ts.plot(yahoo1,predicted$pred,col = "green") # dotted predicted lines

# Using forecast()
Ar = forecast(arimayahoo.ts,h = 20)
Ar # 2nd column shows the predicted value and rest 4 columns are predicted value under 80% and 95% confidence interval
plot(Ar)
JArPredict = forecast(arimayahoo.ts,h = 20)
JArPredict
#plot(JArPredict$residuals)
#qqnorm(JArPredict$residuals)

# Testing ARIMA model
data2 = round(tail(predicted$pred,36),0) # predicted values
data1 = round(tail(yahoo1,36),0) # original values
data1
data2


#-----------------------------------------------------
# 15. Visualization : Plot predicted values with actual values for better comparison
x = time(data1)
y1 = data.frame(data1)
y2 = data.frame(data2)
df = tbl_df(data.frame(x,y1,y2))
df
# Plot of Predicted value vs Actual Quarterly Earnings for 3 years i.e., 2011-14
ggplot(df,aes(x,hp))+
  geom_line(aes(y=data1),colour='red')+
  geom_line(aes(y=data2),colour='blue')+
  labs(y = "Earnings (in dollars)", x = "Time")+ 
  scale_y_continuous(limits=c(0,40)) 
    geom_smooth(method="lm")
   ylim(0,20)+xlim(2011,2014)+
  ggtitle("Comparison of predicted value of earnings and actual earnings")

plot.ts(data1, col="red", type = "l",xlim=c(2011,2020),ylim=c(0,30), ylab = "Earning (in dollars)")
par(new=TRUE)
plot(data2, col="blue",type="l",xlim=c(2011,2020),ylim=c(0,30), ylab = "Earning (in dollars)", main = "Predicted value vs Actual Quarterly Earnings for the year 1980")


#------------------------------------------------
# 16. Accuracy of ARIMA Model (using auto.arima())
summary(arimayahoo.ts)
accuracy(arimayahoo.ts) #: this is a function for arima model so to run this we need to unload "Metrics" library
rmse(data1,data2)
accuracy(data1,data2)


#------------------------------------------------
# Ques17. Tuning the model by manually giving (p,d,q) values
# p : AR (Auto-regressive model), d : I (Difference/ Integration part), q : MA (Moving average time) 

# First make the Data Stationary
plot(log(yahoo.ts)) # homongenizing variance
abline(reg = lm(log(yahoo.ts)~time(yahoo.ts)),col="orange")
m = diff(log(yahoo.ts))
plot(m) # homogenizing mean

# Preprocessing
pacf(yahoo.ts) # q = 1
pacf = acf(log(yahoo.ts))
acf(yahoo.ts,plot =TRUE )
# Each observation is positively associated with its recent past at least through 4 lags.
pacf(diff(log(yahoo.ts))) # p=3
acf(m) # q =1

# First Try : 
fit = arima(log(yahoo.ts_train),c(2,1,1),seasonal = list(order=c(0,1,0),period=12))
fit
pred = predict(fit,n.ahead = 3*12)
pred # next 3 ten years pred values(in log)
pred1 = round(2.718^pred$pred,0)
pred1
ts.plot(yahoo.ts,pred1,log="y",lty=c(1,3),xlab="Time")#dotted predicted lines

# Testing the model
data11 = round(tail(pred1,20),0)#predicted values
data22 = round(tail(yahoo.ts,20),0)#original values
data11
data22
par(mfrow = c(2,1))
# Visualization
par(mar=c(1,1,1,1))
plot.ts(data11, col="red", type = "l",ylim=c(0,20), ylab = "Earning (in dollars)", main = "Comparison of predicted value of earnings and actual earnings for 3 years 2011-14")
par(new=TRUE)
plot.ts(data22, col="blue",type="l",ylim=c(0,20), ylab = "Earning (in dollars)", main = "Comparison of predicted value of earnings and actual earnings for 3 years 2011-14")

plot.ts(data1, col="red", type = "l",xlim=c(2014,2015),ylim=c(0,20), ylab = "Earning (in dollars)", main = "Predicted values vs Actual Quarterly Earnings for the year 2014")
par(new=TRUE)
plot(data2, col="blue",type="l",xlim=c(2014,2015),ylim=c(0,20), ylab = "Earning (in dollars)", main = "Predicted values vs Actual Quarterly Earnings for the year 2014")
# Accuracy of model
# accuracy(fit)
rmse(pred1,data2)
accuracy(pred1,data2)

# Second Try :  
yahoo.ts_fit <- auto.arima(log(yahoo.ts_train))
plot(yahoo.ts_fit)
accuracy(yahoo.ts_fit)
plot(accuracy(yahoo.ts_fit))

# 19. Raw data Vs Cleaned data
par(mfrow = c(2,1))
HW <- HoltWinters(yahoo.ts,seasonal = "multiplicative") # This will give same fitting as auto.arima()
HW
plot(HW, main = "Original time series against the Fitted time series : Raw Data")
HWC <- HoltWinters(yahoo1,seasonal = "multiplicative") # This will give same fitting as auto.arima()
HWC
plot(HWC, main = "Original time series against the Fitted time series : Cleaned Data")
HW$SSE
HWC$SSE

