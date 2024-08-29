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
BBRI <- read_excel("F:/Skripsi/Data/Mingguan Awal.xlsx", sheet = "Mingguan")
View (BBRI)
Harga_Saham_All <- ts(BBRI$AdjustedClose)
Harga_Saham <- ts(Harga_Saham_All[1:171])
Waktu_All <- ts(BBRI$Waktu)
Waktu <- ts(Waktu_All[1:171])
summary(Harga_Saham)
sd(Harga_Saham)
length(Harga_Saham)

#== Plot Data ==#
plot(Harga_Saham, xlab = "Waktu", ylab ="Harga Saham", type = "l", col = "blue", main = "Plot Harga Saham BBRI 2017-2020")

#== Plot Keseluruhan Data dengan Batas Intervensi==#
plot(Harga_Saham, xlab = "Waktu", ylab ="Harga Saham", type = "l", col = "blue", main = "Plot Harga Saham BBRI 2017-2020")
points(Harga_Saham, cex = 0.5, col = "red")
abline(v=155, col = 1, lty = 2)
text (155, Harga_Saham[155], "(t=155)", pos = 4)

# Data sebelum intervensi (Kelompok 1) dengan titik intervensi 164
Kel1 <- ts(Harga_Saham[1:154])
head(Kel1)
plot(Kel1, xlab = "Waktu", ylab = "Harga Saham", col = "blue", type = "l", main = "Plot Harga Saham BBRI 2019-2020")
points (Kel1, cex = 0.5, col = "red")
summary(Kel1)

# Melihat Trend Data sebelum intervensi
t <- c(1:154)
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
fit210 <-arima(Diff1, order = c(2,1,0), method = "ML")
fit210
coeftest(fit210) #ok
fit012 <-arima(Diff1, order = c(0,1,2), method = "ML")
fit012
coeftest(fit012) #ok

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

#ARIMA(210)
lillie.test (fit210$residuals) #tidak signifikan dgn p-value 0,007
# 2. Uji White Noise
Box.test (fit210$residuals, type = "Ljung-Box")

#ARIMA(012)
lillie.test (fit012$residuals) #tidak signifikan dgn p-value 0,007
# 2. Uji White Noise
Box.test (fit012$residuals, type = "Ljung-Box")

#fitted vs actual 
Kel1_aa <- forecast::Arima(Kel1, order = c(2,1,0))
plot(Kel1)
lines(fitted(Kel1_aa), col = "red")
legend("topleft", legend = c("original", "fitted"), col = c("black", "red"), lty = 1)

#Peramalan dengan model 110
ramal_210 <- forecast::forecast (Kel1, model = fit210, h = 17)
ramal_210
plot (ramal_210)

# Residual dari hasil peramal_011an dan data harga saham setelah terintervensi
error_idintv <- rep(0,171)
error_idintv[1:154] <- fit210$residuals[1:154]
error_idintv[155:171] <- Harga_Saham[155:171] - ramal_210$mean[1:17]
error_idintv
plot(error_idintv, type="h", xlab="Waktu (T)", ylab ="Residual", xaxt = "n")
abline (h=c(-3*sd(fit210$residuals), 3*sd(fit210$residuals)),
        col="blue", lty=2)
abline(v = 155, col = "red", lty = 3, lwd = 1.5)
text (155, 200, "T=155",cex = .8, pos = 2)
axis(1, at = c(0, 50, 100, 150, 200), labels = c("t=0", "t=50", "t=100", "t=150", "t=200"))

# --- Detecting Outliers with tsoutliers::tso
BBRI_outlier <- tsoutliers::tso(Harga_Saham, types = c("AO","LS","TC"),
                                maxit.iloop=10, tsmethod = "auto.arima")
plot(BBRI_outlier)


 # --- Plot BBRI vs forecasting of Kell vs fitted value vs trend
plot(ramal_210, main =NA, ylab="Saham BBRI", xlab="Waktu")
lines(Harga_Saham[1:171], col="black")
lines(fitted(Kel1_aa), col = "red")
abline(trend_BBRI[1:155], col = "green", lwd = .5)
abline(v=155, lty=3, col="black")
text(155, Harga_Saham[155], "(t=155)", pos = 4)
legend("topleft", legend = c("Actual", "Forcasting by ARIMA(2,1,0)", "Fitted","Trend"), cex=0.75, lty=1, col=c("black", "blue", "red", "green"))  
# Estimasi Parameter untuk model b,s,r pada pemilihan model ARIMA 011
# Model Arima 011 dan int. b = 0, s = 0, r = 2
step_210_b0_s3_r0<-TSA::arimax(Harga_Saham, order  = c(2,1,0), xtransf = data.frame (T150 = 1*(seq(Harga_Saham)>=155)), transfer = list(c(0,3)))
step_210_b0_s3_r0
coeftest(step_210_b0_s3_r0)
step_210_b0_s2_r0<-TSA::arimax(Harga_Saham, order  = c(2,1,0), xtransf = data.frame (T150 = 1*(seq(Harga_Saham)>=155)), transfer = list(c(0,2)))
step_210_b0_s2_r0
coeftest(step_210_b0_s2_r0)
step_210_b0_s1_r0<-TSA::arimax(Harga_Saham, order  = c(2,1,0), xtransf = data.frame (T150 = 1*(seq(Harga_Saham)>=155)), transfer = list(c(0,1)))
step_210_b0_s1_r0
coeftest(step_210_b0_s1_r0)
T155=1*(seq(Harga_Saham)>=155)
T155
xreg.210.b0_s2_r0<-stats::filter(T155,c(-450.062634,-341.496843,-366.782217),"convolution",sides=1)
xreg.210.b0_s2_r0[is.na(xreg.210.b0_s2_r0)]<-0
BBRI_ARIMA_210_b0_s2_r0 <- Arima(Harga_Saham, 
                                 lambda = lambda, 
                                 order = c(2,1,0), 
                                 include.constant = TRUE, 
                                 xreg = xreg.210.b0_s2_r0)
#Pengecekan Residual Model Intervensi dengan ARIMA (0,1,1)
# Box Test
#110_r_2
Box.test(BBRI_ARIMA_210_b0_s2_r0$residuals,
         lag=round(length(Harga_Saham)/5,0), type = "Ljung-Box", fitdf = 1)
# Normalitas
#110_r_2
ks.test(BBRI_ARIMA_210_b0_s2_r0$residuals, "pnorm", 
        mean(BBRI_ARIMA_210_b0_s2_r0$residuals),
        sd(BBRI_ARIMA_210_b0_s2_r0$residuals))

# Model Arima 011 dan int. b = 1, s = 1, r = 0
step_210_b1_s2_r0<-TSA::arimax(Harga_Saham, order  = c(2,1,0), xtransf = data.frame (T150 = 1*(seq(Harga_Saham)>=156)), transfer = list(c(0,1)))
step_210_b1_s2_r0
coeftest(step_210_b1_s2_r0)
T155=1*(seq(Harga_Saham)>=156)
T155 
xreg.210.b1_s1_r0<-stats::filter(T155,c(-466.332300,-434.655032),"convolution",sides=1)
xreg.210.b1_s1_r0[is.na(xreg.210.b1_s1_r0)]<-0
BBRI_ARIMA_210_b1_s1_r0 <- Arima(Harga_Saham, 
                                 lambda = lambda, 
                                 order = c(2,1,0), 
                                 include.constant = TRUE, 
                                 xreg = xreg.210.b1_s1_r0)
#Pengecekan Residual Model Intervensi dengan ARIMA (0,1,1)
# Box Test
#110_r_2
Box.test(BBRI_ARIMA_210_b1_s1_r0$residuals,
         lag=round(length(Harga_Saham)/5,0), type = "Ljung-Box", fitdf = 1)
# Normalitas
#110_r_2
ks.test(BBRI_ARIMA_210_b1_s1_r0$residuals, "pnorm", 
        mean(BBRI_ARIMA_210_b1_s1_r0$residuals),
        sd(BBRI_ARIMA_210_b1_s1_r0$residuals))

# Model Terbaik
AIC(BBRI_ARIMA_210_b0_s2_r0)
AIC(BBRI_ARIMA_210_b1_s1_r0)
BIC(BBRI_ARIMA_210_b0_s2_r0)
BIC(BBRI_ARIMA_210_b1_s1_r0)

# Peramalan
#step210_b_0
xreg.rob_step210_b0_s2_r0 = forecast (auto.arima(xreg.210.b0_s2_r0), h=27)$mean
#Forecasting
ramalan210_b0_s2_r0 <- forecast (BBRI_ARIMA_210_b0_s2_r0, xreg = xreg.rob_step210_b0_s2_r0)
ramalan210_b0_s2_r0
plot(ramalan210_b0_s2_r0, main = "Hasil Peramalan Model Intervensi \n b = 0, s = 2, r = 0", ylab = "Harga Saham BBRI", xlab = "Waktu (Minggu)")

#Comparing
plot(ramalan210_b0_s2_r0, main =NA, ylab="Saham BBRI", xlab="Waktu")
points (Harga_Saham_All[1:198], cex=.2, col="black", pch=19)
lines (Harga_Saham_All[1:198], col="red")
abline(v=172, lty=2, col="red")
abline(v=155, lty=2, col="black")
legend("topleft", legend = c("Harga Saham BBRI", "Peramalan Model Intervensi"), cex=0.75, lty=1, col=c("red", "blue"), pch=c(20,NA))

#MAPE
hasil_ramal <- c(ramalan210_b0_s2_r0$mean)
hasil_ramal
nilai_aktual <- c(Harga_Saham_All[172:198])
nilai_aktual
MAPE011_b0_s2_r0 <- sum(abs((nilai_aktual-hasil_ramal)/nilai_aktual))/length(nilai_aktual)*100
MAPE011_b0_s2_r0
