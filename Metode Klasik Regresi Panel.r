# GEOGRAPHICALLY WEIGHTED PANEL REGRESSIN  MODELLING
library(lmtest)
library(car)
library(plm)
library(tseries)
library(car)
library(readxl)
data = read_excel("C:\\Users\\Mahendra\\OneDrive - UGM 365\\S1 STATISTIKA\\Academic\\Data Mining\\FINAL ARC - datmin\\X_train_cv.xlsx")


head(data)
dt = data.frame(Y = data$`Y(PDRB)`,
                X1 = data$`Frekuensi curah hujan (sedang)`,
                X2 = data$`Frekuensi curah hujan (lebat)`,
                X3 = data$`Curah Hujan dalam mm`,
                X4 = data$`Jumlah Bencana`,
                X5 = data$`Korban Meninggal`,
                X6 = data$`Korban Hilang`,
                X7 = data$`Korban Lukaluka`,
                X8 = data$`Korban Menderita`,
                X9 = data$`Korban Mengungsi`,
                X10 = data$`Bangunan terdampak`,
                X11 = data$`indeks bencana`,
                X12 = data$`Jumlah Penduduk`,
                X13 = data$`Tingkat Pengagguran Terbuka`,
                X14=data$`Realisasi Investasi Penanaman Modal Luar Negeri Menurut Prov`,
                X15 = data$`Realisasi Investasi Penanaman Modal Dalam Negeri Menurut Prov`,
                Tahun = data$Tahun,
                Provinsi = data$Provinsi
                )

summary(dt)
# Factorization
dt$Provinsi <- as.factor(dt$Provinsi)
dt$Tahun <- as.factor(dt$Tahun)

#########################################EDA###########################
#Pooled regression
model_pooled <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "pooling", index = c("Provinsi", "Tahun"))
summary(model_pooled)
# Fixed effect individual
model_fe_individual <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "within",effect='individual', index = c("Provinsi", "Tahun"))
summary(model_fe_individual)
summary(fixef(model_fe_individual, effect = "individual"))
# Fixed effect time
model_fe_time <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "within",effect='time', index = c("Provinsi", "Tahun"))
summary(model_fe_time)
summary(fixef(model_fe_time, effect = "time"))
# Fixed effect twoway
model_fe_twoways <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data=dt, model = "within",effect='twoways', index = c("Provinsi", "Tahun"))
summary(model_fe_twoways)
data.frame(fixef(model_fe_twoways, effect = "twoways"))
fixed_model = plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "within",index = c("Provinsi","Tahun"))
summary(fixed_model)
# Model Evaluation
# Uji Pengaruh Individu
plmtest(model_fe_twoways, type = "bp", effect = "individual")
# Uji Pengaruh waktu
plmtest(model_fe_twoways, type = "bp", effect = "time")
# FEM VS CEM
pooltest(model_pooled, model_fe_twoways)

# REM model
model_re <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "random", index = c("Provinsi", "Tahun"))
summary(model_re)

# FEM VS REM
phtest(model_fe_twoways, model_re)

#model 1
model_fe_individual <- plm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "within",effect='individual', index = c("Provinsi", "Tahun"))
summary(model_fe_individual)
#model 2
model_fe_individual_2 <- plm(Y ~ X1+X2 + X3 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "within",effect='individual', index = c("Provinsi", "Tahun"))
summary(model_fe_individual_2)
#MODEL 3
model_fe_individual_3 <- plm(Y ~X2 + X3 + X5 + X6 + X7 + X8 + X9 + X10 +X11 +X12 +X13+X14+X15, data = dt, model = "within",effect='individual', index = c("Provinsi", "Tahun"))
summary(model_fe_individual_3)
#normality
shapiro.test(model_fe_twoways$residuals) #tidak normal
#DIAGNOSTIC Tidak normal
ks.test(model_fe_individual$residuals, "pnorm", mean(model_fe_individual$residuals), sd(model_fe_individual$residuals))
# Autocorrelation
pbgtest(model_fe_individual) #terdapat serial correlation
# Homogenity
bptest(model_fe_individual) #tidak homogen/heteroskedastik

###############GWPR
########Modelling GWPR
library(jsonlite)
wilayah <- fromJSON("https://raw.githubusercontent.com/yusufsyaifudin/wilayah-indonesia/master/data/list_of_area/provinces.json")
#wilayah
# apply to data
data.sp.GWPR= dt
data.sp.GWPR
library(GWmodel)
coordinates(data.sp.GWPR) <- 2:3
class(data.sp.GWPR)

dt <- data.frame(
  Y = data_Jabar_Stunting$`Y(Persentase Balita TB/U)`,
  Lat = data_Jabar_Stunting$Latitude,
  Lon = data_Jabar_Stunting$Longitude,
  X1 = data_Jabar_Stunting$`X1(TabletTambahDarahIbu)`,
  X2 = data_Jabar_Stunting$`X2(BayiImunisasiDasarLengkap)`,
  X3 = data_Jabar_Stunting$`X3(ASIeksklusif)`,
  X4 = data_Jabar_Stunting$`X4(BayiVitaminA)`,
  X5 = data_Jabar_Stunting$`X5(KKAksesSanitasiLayak)`,
  X6 = data_Jabar_Stunting$`X6(PelayananBayi)`,
  X7 = data_Jabar_Stunting$`X7(IndeksKedalamanKemiskinan)`,
  X8 = data_Jabar_Stunting$`X8(K1)`,
  X9 = data_Jabar_Stunting$`X9(K4)`,
  X10 = data_Jabar_Stunting$`X10(IMD)`,
  Tahun = data_Jabar_Stunting$Tahun,
  Kabupaten = data_Jabar_Stunting$KabupatenKota
)

# Menentukan Fungsi Pembobot Spasial Terbaik4
# menggunakan 3 fungsi kernel: gaussian, eksponensial, bisquare )adaptive dan fixed)
ols_step_all_possible(log_model)
log_model=lm(formula= log(Y) ~ log(X1) + log(X2)+log(X3)+log(X7) +log(X5) + log(X9), data=dt)

formula= log(Y) ~ log(X1) + log(X2)+log(X3)+log(X5)+log(X7) + log(X9) #60 oersen bisquare adaptive
formula= log(Y) ~ log(X1) + log(X2)+log(X3)+log(X5)+log(X7) + log(X9) 
# adaptive bisquare
bwd.GWPR.bisquare.ad <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "bisquare", adaptive = T)
hasil.GWPR.bisquare.ad <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.bisquare.ad, kernel = "bisquare", adaptive = T)

# adaptive gaussian
bwd.GWPR.gaussian.ad <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "gaussian", adaptive = T)
hasil.GWPR.gaussian.ad <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.gaussian.ad, kernel = "gaussian", adaptive = T)

# adaptive eksponensial
bwd.GWPR.eksponensial.ad <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "exponential", adaptive = T)
hasil.GWPR.eksponensial.ad <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.eksponensial.ad, kernel = "exponential", adaptive = T)

# fixed bisquare
bwd.GWPR.bisquare.fx <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "bisquare", adaptive = F)
hasil.GWPR.bisquare.fx <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.bisquare.fx, kernel = "bisquare", adaptive = F)

# fixed gaussian
bwd.GWPR.gaussian.fx <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "gaussian", adaptive = F)
hasil.GWPR.gaussian.fx <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.gaussian.fx, kernel = "gaussian", adaptive = F)

# fixed eksponensial
bwd.GWPR.eksponensial.fx <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "exponential", adaptive = F)
hasil.GWPR.eksponensial.fx <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.eksponensial.fx, kernel = "exponential", adaptive = F)

# fixed tricube
bwd.GWPR.tricube.fx <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "tricube", adaptive = F)
hasil.GWPR.tricube.fx <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.tricube.fx, kernel = "tricube", adaptive = F)
hasil.GWPR.tricube.fx$GW.diagnostic$gw.R2

# adaptive tricube
bwd.GWPR.tricube.ad <- bw.gwr(formula, data = data.sp.GWPR, approach = "CV", kernel = "tricube", adaptive = T)
hasil.GWPR.tricube.ad <- gwr.basic(formula, data = data.sp.GWPR, bw = bwd.GWPR.tricube.fx, kernel = "tricube", adaptive = T)
hasil.GWPR.tricube.ad$GW.diagnostic$gw.R2


AIC <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$AIC, hasil.GWPR.gaussian.ad$GW.diagnostic$AIC, hasil.GWPR.eksponensial.ad$GW.diagnostic$AIC, hasil.GWPR.bisquare.fx$GW.diagnostic$AIC, hasil.GWPR.gaussian.fx$GW.diagnostic$AIC, hasil.GWPR.eksponensial.fx$GW.diagnostic$AIC)
R2 <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$gw.R2, hasil.GWPR.gaussian.ad$GW.diagnostic$gw.R2, hasil.GWPR.eksponensial.ad$GW.diagnostic$gw.R2, hasil.GWPR.bisquare.fx$GW.diagnostic$gw.R2, hasil.GWPR.gaussian.fx$GW.diagnostic$gw.R2, hasil.GWPR.eksponensial.fx$GW.diagnostic$gw.R2)
AICc <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$AICc, hasil.GWPR.gaussian.ad$GW.diagnostic$AICc, hasil.GWPR.eksponensial.ad$GW.diagnostic$AICc, hasil.GWPR.bisquare.fx$GW.diagnostic$AICc, hasil.GWPR.gaussian.fx$GW.diagnostic$AICc, hasil.GWPR.eksponensial.fx$GW.diagnostic$AICc)
R2Adj <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$gw.R2adj, hasil.GWPR.gaussian.ad$GW.diagnostic$gw.R2adj, hasil.GWPR.eksponensial.ad$GW.diagnostic$gw.R2adj, hasil.GWPR.bisquare.fx$GW.diagnostic$gw.R2adj, hasil.GWPR.gaussian.fx$GW.diagnostic$gw.R2adj, hasil.GWPR.eksponensial.fx$GW.diagnostic$gw.R2adj)
RSS <- c(hasil.GWPR.bisquare.ad$GW.diagnostic$RSS.gw, hasil.GWPR.gaussian.ad$GW.diagnostic$RSS.gw, hasil.GWPR.eksponensial.ad$GW.diagnostic$RSS.gw, hasil.GWPR.bisquare.fx$GW.diagnostic$RSS.gw, hasil.GWPR.gaussian.fx$GW.diagnostic$RSS.gw, hasil.GWPR.eksponensial.fx$GW.diagnostic$RSS.gw)

tabel <- cbind(AIC, R2, AICc, R2Adj, RSS)
rownames(tabel) <- c("Bisquare Adaptive", "Gaussian Adaptive", "Eksponensial Adaptive", "Bisquare Fixed", "Gaussian Fixed", "Eksponensial Fixed")
data.frame(tabel)
