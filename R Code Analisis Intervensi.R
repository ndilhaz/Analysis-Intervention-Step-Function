#==Pemanggilan Paket ==#
library(tseries)
library(readxl)
library(lmtest)
library(nortest)
library(tsoutliers)
library(TSA)
library(forecast)
library(strucchange)

#== Pemanggilan Data ==#
BBRI <- read_excel("F:/Skripsi/Data/Data Saham BRI 2017-2020.xlsx", sheet = "Data Mingguan")
View (BBRI)
Harga_Saham_All <- ts(BBRI[,5])
Harga_Saham <- ts(Harga_Saham_All[1:177])
Waktu_All <- ts(BBRI[,4])
Waktu <- ts(Waktu_All[1:177])
summary(Harga_Saham)
sd(Harga_Saham)

#== Plot Data ==#
plot(Harga_Saham, xlab = "Waktu", ylab ="Harga Saham", type = "l", col = "blue", main = "Plot Harga Saham BBRI 2017-2020")

#== Plot Keseluruhan Data dengan Batas Intervensi==#
plot(Harga_Saham, xlab = "Waktu", ylab ="Harga Saham", type = "l", col = "blue", main = "Plot Harga Saham BBRI 2017-2020")
points(Harga_Saham, cex = 0.5, col = "red")
abline(v=164, col = 1, lty = 2)
text (164, Harga_Saham[164], "(t=164)", pos = 1)

# Data sebelum intervensi (Kelompok 1) dengan titik intervensi 164
Kel1 <- ts(Harga_Saham[1:163])
head(Kel1)
plot(Kel1, xlab = "Waktu", ylab = "Harga Saham", col = "blue", type = "l", main = "Plot Harga Saham BBRI 2019-2020")
points (Kel1, cex = 0.5, col = "red")
summary(Kel1)

# Melihat Trend Data sebelum intervensi
t <- c(1:163)
trend_BBRI <- lm(Kel1~t)
plot(Kel1, xlab = "Waktu", ylab = "Harga Saham", col = "blue",
     type = "p", main = "Plot Data Kelompok 1")
points (Kel1, cex = 0.5, col = "red")
abline(trend_BBRI, col = "black", lwd = 2)

# Plot ACF dan PACF utk identifikasi Model ARIMA
acf (Kel1)
pacf (Kel1)


# Stasioneritas dalam Varians
#cara 1
lambda <- BoxCox.lambda(Kel1, method = "loglik", lower = -1,
                        upper = 2)
lambda

#cara 1
t1 <-1:length(Kel1)
MASS :: boxcox(lm(Kel1 ~ t1 ), lambda = seq ( 0 ,2 ,1/10),
               ylab = "Log - Likelihood")

#Stasioneritas dalam Rerata
adf.test(Kel1)

#Didapatkan hasil p-value 0.469 sehingga perlu differencing
# differencing 1
Diff1<- diff(Kel1, differences = 1)
adf.test(Diff1)
Diff1
#p-value dari adf test sudah dibawah taraf signifikasni 0,01 sehingga sudah stasioner

# Plot ACF dan PACF setelah data Kelompok 1 telah stasioner
acf(Diff1, lag.max = 46)
pacf(Diff1, lag.max = 46)

# ---Dugaan pemodelan ARIMA dan uji signifikansi parameter--- #

# using auto.arima
auto_arima <- auto.arima(Diff1, d=1, seasonal=FALSE, lambda=lambda,
                         stepwise=TRUE, trace=TRUE, max.p=5, max.q=5)

# Dugaan arima sementara2
fit110 <- arima(Diff1, order = c(1,1,0), method = "ML")
fit110
coeftest(fit110) #ok
fit011 <-arima(Diff1, order = c(0,1,1), method = "ML")
fit011
coeftest(fit011) #ok
fit111 <-arima(Diff1, order = c(1,1,1), method = "ML")
fit111
coeftest(fit111) #ok
#Diagnostics Models
#ARIMA(110)
# 1. Normalitas
lillie.test (fit110$residuals)
# 2. Uji White Noise
Box.test (fit110$residuals, type = "Ljung-Box")

#ARIMA(011)
# 1. Normalitas
lillie.test (fit011$residuals)
# 2. Uji White Noise
Box.test (fit011$residuals, type = "Ljung-Box")

#ARIMA(111)
lillie.test (fit111$residuals) #tidak signifikan dgn p-value 0,007
# 2. Uji White Noise
Box.test (fit111$residuals, type = "Ljung-Box") 

# Pemilihan Model Terbaik
AIC(fit110)
AIC(fit011)
AIC(fit111)
BIC(fit110)
BIC(fit011)
BIC(fit111)


#Peramalan dengan model 110
ramal_011 <- forecast::forecast (Kel1, model = fit011, h = 14)
ramal_011
plot (ramal_011)

# Residual dari hasil peramal_011an dan data harga saham setelah terintervensi
error_idintv <- rep(0,177)
error_idintv[1:163] <- fit011$residuals[1:163]
error_idintv[164:177] <- Harga_Saham[164:177] - ramal_011$mean[1:14]
error_idintv
plot(error_idintv, type="h", xlab="Waktu (T)", ylab ="Residual", xaxt = "n")
abline (h=c(-3*sd(fit011$residuals), 3*sd(fit011$residuals)),
        col="blue", lty=2)
abline(v = 164, col = "red", lty = 3, lwd = 1.5)
text (164, 200, "T=164",cex = .8, pos = 2)
axis(1, at = c(0, 50, 100, 150, 200), labels = c("t=0", "t=50", "t=100", "t=150", "t=200"))

# --- Detecting Outliers with tsoutliers::tso
BBRI_outlier <- tsoutliers::tso(Harga_Saham, types = c("AO","LS","TC"),
                               maxit.iloop=10, tsmethod = "auto.arima")
plot(BBRI)


# --- Plot BBRI vs forecasting of Kell
plot(ramal_011, main =NA, ylab="Saham BBRI", xlab="Waktu")
points (Harga_Saham[1:177], cex=.5, col="dark red", pch=19)
lines (Harga_Saham[1:177], col="red")
abline(v=164, lty=3, col="black")
text (164, Harga_Saham[164], "(t=164)", pos = 1)
legend("topleft", legend = c("Harga Saham BBRI", "Peramalan Model 011"), cex=0.75, lty=1, col=c("dark red", "blue"), pch=c(19,NA) )

# Estimasi Parameter untuk model b,s,r pada pemilihan model ARIMA 011
# Model Arima 011 dan int. b = 0, s = 0, r = 2
step_011_b0_s0_r2<-TSA::arimax(Harga_Saham, order = c(0,1,1), xtransf = data.frame (T62 = 1*(seq(Harga_Saham)>=164)), transfer = list(c(2,0))) 
step_011_b0_s0_r2
coeftest(step_011_b0_s0_r2)
T164=1*(seq(Harga_Saham)>=164)
T164
xreg.011.b0_s0_r2<-stats::filter(T164,c(-299.78),"convolution",sides=1)
xreg.011.b0_s0_r2[is.na(xreg.011.b0_s0_r2)]<-0
xreg.011.b0_s0_r2<-stats::filter(xreg.011.b0_s0_r2,c(0.031337,-0.0075008),"recursive",sides=1)
xreg.011.b0_s0_r2
BBRI_ARIMA_110_b0_s0_r2 <- Arima(Harga_Saham, 
                                 lambda = lambda, 
                                 order = c(1,1,0), 
                                 include.constant = TRUE, 
                                 xreg = xreg.011.b0_s0_r2)
#Pengecekan Residual Model Intervensi dengan ARIMA (0,1,1)
# Box Test
#110_r_2
Box.test(BBRI_ARIMA_110_b0_s0_r2$residuals,
         lag=round(length(Harga_Saham)/5,0), type = "Ljung-Box", fitdf = 1)
# Normalitas
#110_r_2
ks.test(BBRI_ARIMA_110_b0_s0_r2$residuals, "pnorm", 
        mean(BBRI_ARIMA_110_b0_s0_r2$residuals),
        sd(BBRI_ARIMA_110_b0_s0_r2$residuals))
hist(BBRI_ARIMA_110_b0_s0_r2$residuals) 
checkresiduals(BBRI_ARIMA_110_b0_s0_r2)

# Model ARIMA 011 dan int. b = 0, s = 0, r = 1
step_011_b0_s0_r1<-TSA::arimax(Harga_Saham, order = c(0,1,1), xtransf = data.frame (T62 = 1*(seq(Harga_Saham)>=164)), transfer = list(c(1,0))) 
step_011_b0_s0_r1
coeftest(step_011_b0_s0_r1)
T164=1*(seq(Harga_Saham)>=164)
T164
xreg.011.b0_s0_r1<-stats::filter(T164,c(-410.978171),"convolution",sides=1)
xreg.011.b0_s0_r1[is.na(xreg.011.b0_s0_r1)]<-0
xreg.011.b0_s0_r1<-stats::filter(xreg.011.b0_s0_r1,0.631703,"recursive",sides=1)
xreg.011.b0_s0_r1
BBRI_ARIMA_110_b0_s0_r1 <- Arima(Harga_Saham, 
                                 lambda = lambda, 
                                 order = c(1,1,0), 
                                 include.constant = TRUE, 
                                 xreg = xreg.011.b0_s0_r1)
#Pengecekan Residual Model Intervensi dengan ARIMA (0,1,1)
# Box Test
#110_b_0
Box.test(BBRI_ARIMA_110_b0_s0_r1$residuals,
         lag=round(length(Harga_Saham)/5,0), type = "Ljung-Box", fitdf = 1)
# Normalitas
#110_b_0
ks.test(BBRI_ARIMA_110_b0_s0_r1$residuals, "pnorm", 
        mean(BBRI_ARIMA_110_b0_s0_r1$residuals),
        sd(BBRI_ARIMA_110_b0_s0_r1$residuals))
hist(BBRI_ARIMA_110_b0_s0_r1$residuals) 
checkresiduals(BBRI_ARIMA_110_b0_s0_r1)

# Model Terbaik
AIC(BBRI_ARIMA_110_b0_s0_r2)
AIC(BBRI_ARIMA_110_b0_s0_r1)
BIC(BBRI_ARIMA_110_b0_s0_r2)
BIC(BBRI_ARIMA_110_b0_s0_r1)

# Peramalan
#step110_b_0
xreg.rob_step011_b0_s0_r1 = forecast (auto.arima(xreg.011.b0_s0_r1), h=15) $mean
#Forecasting
ramalan110_b0_s0_r1 <- forecast (BBRI_ARIMA_110_b0_s0_r1, xreg = xreg.rob_step011_b0_s0_r1)
ramalan110_b0_s0_r1
plot(ramalan110_b0_s0_r1, main = "Hasil Peramalan Model Intervensi \n b = 0, s = 0, r = 1", ylab = "Harga Saham BBRI", xlab = "Waktu (Minggu)")

#MAPE
hasil_ramal <- c(ramalan110_b0_s0_r1$mean)
hasil_ramal
nilai_aktual <- c(Harga_Saham_All[178:192] )
nilai_aktual
MAPE011_b0_s0_r1 <- sum(abs(nilai_aktual-hasil_ramal)/nilai_aktual)/length(nilai_aktual)*100
MAPE011_b0_s0_r1

