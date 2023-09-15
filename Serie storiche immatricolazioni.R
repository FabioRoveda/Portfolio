##########################################
#CONFRONTO SERIE IMMATRICOLAZIONI MENSILI#
##########################################

#Import
{
library(ggplot2)
library(tseries)
library(fpp)
library(fpp2)
library(seasonal)
library(forecast)
library(readxl)
Serie <- read_excel("Serie.xlsx", col_types = c("date", "numeric"))
dati<- ts(Serie[,2], start = c(2000, 1), frequency = 12)
plot(dati, main="Dati Immatricolazioni", xlab="Anno", ylab="Immatricolazioni")
class(dati)
}

#normalizzo i dati in base ai giorni per mese
{
giorni<-rep(c(31,28,31,30,31,30,31,31,30,31,30,31),24)
giorni<-giorni[c(1:278)]
gmesi<-365/12
datig<-(dati/giorni)*gmesi
plot(datig, main="Dati Immatricolazioni normalizzati", xlab="Anno", ylab="Immatricolazioni")
}

par(mfrow = c(1, 1)) 
seqplot.ts(dati, datig,main="Dati reali e dati normalizzati", 
           xlab="Anno", ylab="Immatricolazioni")

#trattamento outlier additivo 2020
#sostituisco i mesi covid con una media tra quello precedente e successivo
s<-seas(datig)
class(s)
outlier(s) # outlier

datinew<-datig
for (i in c(243:247)) {
  datinew[i]<-(datinew[i-12]+datinew[i+12])/2
}
plot(datig, main="Dati normalizzati covid", xlab="Anno", ylab="Immatricolazioni",ylim=c(0,300000))
plot(datig, main="Dati normalizzati senza periodo covid", xlab="Anno", ylab="Immatricolazioni",ylim=c(0,300000), col="red")
lines(datinew)


#divisione in training sample e test sample
n=278
m=216
k=m+1
dati17<- ts(datinew[1:m,], start = c(2000, 1), frequency = 12)
class(dati17)
plot(dati17,main="Training Sample", xlab="Anno", ylab="Immatricolazioni")
dati22<- ts(datinew[k:278,], start = c(2018, 1), frequency = 12)
class(dati22)
plot(dati22,main="Test Sample", xlab="Anno", ylab="Immatricolazioni")


#################
#METODO CLASSICO#
#################

#analisi grafiche preliminari
plot(dati17,main="Training Sample", xlab="Anno", ylab="Immatricolazioni")
#media mobile evidenzio il trend grezzamente
lines(aggregate(dati17, FUN = mean),ylab = "Immatricolazioni", xlab= "mesi", 
      main = "Trend media mobile", col="red")
plot(aggregate(dati17, FUN = mean),ylab = "Immatricolazioni", xlab= "mesi", 
     main = "Trend media mobile",col="red")

#Media mobile 12 mesi per togliere stagionalità
autoplot(dati17)+xlab("Anno")+ylab("Immatricolazioni")
autoplot(dati17,series = "Dati")+
  autolayer(ma(dati17,12),series = "MA")+
  scale_colour_manual(values = c("Dati"="grey50","MA"="red"))+
  xlab("Anni")+ylab("Immatricolazioni")

#indagine grafica silla stagionalità
seasonplot(dati17,ylab = "Immatricolazioni", xlab= "Mesi",
           main = "Seasonal plot Immatricolazioni",
           year.labels=TRUE, year.labels.left = TRUE, col = 1:23, pch=19 )
#boxplot stagionalità
boxplot(dati17~cycle(dati17),ylab = "Immatricolazioni", xlab= "Anno" 
        , pch=19, main = "Boxplot Immatricolazioni")
#monthplot per evidenziare i mesi
monthplot(dati17,ylab = "Immatricolazioni", xlab= "Mesi", 
          main = "Variazioni stagionali Immatricolazioni")


#analisi grafiche preliminari
plot(dati17,main="Training Sample", xlab="Anno", ylab="Immatricolazioni")
#media
abline(reg = lm((dati17)~time(dati17)), col="blue")
#evidenzio il trend grezzamente
lines(aggregate(dati17, FUN = mean),ylab = "Immatricolazioni", xlab= "mesi", 
     main = "Trend media mobile", col="red")
plot(aggregate(dati17, FUN = mean),ylab = "Immatricolazioni", xlab= "mesi", 
     main = "Trend media mobile",col="red")

ndiffs(dati17, alpha = 0.01, test = "adf", max.d = 4) # augmented DF test
ndiffs(dati17, alpha = 0.01, test = "kpss", max.d = 4) # KPSS test
ndiffs(dati17, alpha = 0.01, test = "pp", max.d = 4) # Phillips-Perron test
# i test suggeriscono non si debbano fare differenze di tipo non stagionale

plot(diff(dati17))
#test sulla stazionariet?
(TestDF<-adf.test(dati17))
#la serie è stazionaria, è assente il trend legato ai soli coefficienti non stagionali
Acf(dati17)
pacf(dati17)

#residui
res<- residuals(snaive(dati17))
plot(res)
Acf(res, main="Autocorrelazione residui")
pacf(res, main="Autocorrelazione parziale residui")
hist(res, nclass = "FD")
(TestBP<-Box.test(res,lag=12, fitdf = 0)) #Box-Pierce
(TestLB<-Box.test(res,lag=12, fitdf = 0, type = "Lj")) #Ljung-Box


#Media mobile 12 mesi per togliere stagionalità
autoplot(dati17)+xlab("Anno")+ylab("Imm.")

autoplot(dati17,series = "Dati")+
  autolayer(ma(dati17,12),series = "MA")+
  scale_colour_manual(values = c("Dati"="grey50","MA"="red"))+
  xlab("Anni")+ylab("Imm.")

#################
#METODO CLASSICO#
#################

#metodo ETS exponential smoothing
ETS<- ets(dati17) #utilizzo i dati non differenziati perch? il comando include analisi dei trend
print(summary(ETS))
autoplot(ETS)
checkresiduals(ETS)
#rispettivamente errore trend e stagionalità
#"N"=none, "A"=additive, "M"=multiplicative 
# residui sd = sigma=0.0917
# modello errore moltiplicativo, no trend e stagionalità moltiplicativa
# AIC = 5322.802  AICc = 5325.202
# residui sd = sigma=0.0917
# ME= -636.3174  MSE= 261606505  MAE= 11369.41  MAPE= 6.737525

#scomposizione della serie secondo coefficienti ottenuti ETS
dati17 %>% decompose(type = "multiplicative") %>%
  autoplot()+xlab("Anno")+
  ggtitle("Scomposizione delle Immatricolazioni secondo moltiplicativit?")
# in questo caso i residui non sono totalmente casuali, esiste un'evoluzione armonica
# quindi il modello classico potrebbe non essere sufficiente a cogliere tutta 
# l'informazione con i coufficienti


#calcolo manuale su training sample di ME MSE MAE MAPE
Misure <- function(serie,errore) {
  m<-length(serie)
  ME=sum(errore)/m
  MSE=sum(errore^2)/m
  MAE=sum(abs(errore))/m
  MAPE=sum((abs(errore)/serie)*100)/m
  tabella<-data.frame("ME"=ME,"MSE"=MSE,"MAE"=MAE,"MAPE"=MAPE, row.names = "valore")
  tabella
  print(tabella)
} #implemento una funzione per il calcolo manuale

fitETS17<-ETS$fitted
plot(dati17,main="Training Sample e Fitted values", xlab="Anno", ylab="Immatr.")
lines(fitETS17, col="red")

erroriETS17<-dati17-fitETS17 # y - ycappuccio
mis_Classico17<-Misure(dati17,erroriETS17) #i risultati coincidono con quelli del summary


#calcolo manuale su test sample di ME MSE MAE MAPE
prevETS22<- forecast(ETS, h=n-m)
plot(forecast(object = ETS, h=n-m),
     main = "Previsione immatricolazioni con ETS",
     xlab= "Anno", ylab= "Imm.",  ylim=c(0,300000))

fitETS22<-prevETS22$mean
plot(dati22,main="Test Sample e Fitted values Classico", xlab="Anno", ylab="Immatr.")
lines(fitETS22, col="red")

erroriETS22<-dati22-fitETS22 # y - Forecast
mis_Classico22<-Misure(dati22,erroriETS22) 



########################
#ARIMA senza differenza#
########################
ARIMAd0<- auto.arima(dati17, max.d= 0, max.D= 1,
                     stepwise = FALSE, approximation = FALSE, trace = TRUE) # no differenza
print(summary(ARIMAd0))
autoplot(ARIMAd0)
checkresiduals(ARIMAd0)

fitARIMAd017<-ARIMAd0$fitted
plot(dati17,main="Training Sample e Fitted values", xlab="Anno", ylab="Immatr.")
lines(fitARIMAd017, col="red")

erroriARIMAd017<-dati17-fitARIMAd017 # y - ycappuccio
mis_ARIMAd017<-Misure(dati17,erroriARIMAd017) #i risultati coincidono con quelli del summary


#calcolo manuale su test sample di ME MSE MAE MAPE
prevARIMAd022<- forecast(ARIMAd0, h=n-m)
plot(forecast(object = ARIMAd0, h=n-m),
     main = "Previsione immatricolazioni con ARIMA D=0",
     xlab= "Anno", ylab= "Imm.",  ylim=c(0,300000))

fitARIMAd022<-prevARIMAd022$mean
plot(dati22,main="Test Sample e Fitted values D=0", xlab="Anno", ylab="Immatr.")
lines(fitARIMAd022, col="red")

erroriARIMAd022<-dati22-fitARIMAd022 # y - Forecast
mis_ARIMAd022<-Misure(dati22,erroriARIMAd022)


##########################
#ARIMA con una differenza#
##########################
ARIMAd1<- auto.arima(dati17, d=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) # differenza prima
print(summary(ARIMAd1))
autoplot(ARIMAd1)
checkresiduals(ARIMAd1)

fitARIMAd117<-ARIMAd1$fitted
plot(dati17,main="Training Sample e Fitted values", xlab="Anno", ylab="Immatr.")
lines(fitARIMAd117, col="red")

erroriARIMAd117<-dati17-fitARIMAd117 # y - ycappuccio
mis_ARIMAd117<-Misure(dati17,erroriARIMAd117) #i risultati coincidono con quelli del summary


#calcolo manuale su test sample di ME MSE MAE MAPE
prevARIMAd122<- forecast(ARIMAd1, h=n-m)
plot(forecast(object = ARIMAd1, h=n-m),
     main = "Previsione immatricolazioni con ARIMA D=1",
     xlab= "Anno", ylab= "Imm.",  ylim=c(0,300000))

fitARIMAd122<-prevARIMAd122$mean
plot(dati22,main="Test Sample e Fitted values D=1", xlab="Anno", ylab="Immatr.")
lines(fitARIMAd122, col="red")

erroriARIMAd122<-dati22-fitARIMAd122 # y - Forecast
mis_ARIMAd122<-Misure(dati22,erroriARIMAd122)


##########################
#ARIMA con due differenze#
##########################
ARIMAd2<- auto.arima(dati17, d=2, stepwise = FALSE, approximation = FALSE, trace = TRUE) # differenza seconda
print(summary(ARIMAd2))
autoplot(ARIMAd2)
checkresiduals(ARIMAd2)

fitARIMAd217<-ARIMAd2$fitted
plot(dati17,main="Training Sample e Fitted values", xlab="Anno", ylab="Immatr.")
lines(fitARIMAd217, col="red")

erroriARIMAd217<-dati17-fitARIMAd217 # y - ycappuccio
mis_ARIMAd217<-Misure(dati17,erroriARIMAd217) #i risultati coincidono con quelli del summary


#calcolo manuale su test sample di ME MSE MAE MAPE
prevARIMAd222<- forecast(ARIMAd2, h=n-m)
plot(forecast(object = ARIMAd2, h=n-m),
     main = "Previsione immatricolazioni con ARIMA D=2",
     xlab= "Anno", ylab= "Imm.",  ylim=c(0,300000))

fitARIMAd222<-prevARIMAd222$mean
plot(dati22,main="Test Sample e Fitted values", xlab="Anno", ylab="Immatr.")
lines(fitARIMAd222, col="red")

erroriARIMAd222<-dati22-fitARIMAd222 # y - Forecast
mis_ARIMAd222<-Misure(dati22,erroriARIMAd222)
#per quanto abbia Fit accettabili sul Training Sample è inutile spingersi
#oltre d=1 perchè la capacità previsiva diventa praticamente nulla a causa di intervalli
#di confidenza 80% e 95% estremamente ampi e non significativi ai fini previsivi


###########################
#ARIMA con parmetri decisi#
###########################
(ARIMA<- Arima(dati17, order = c(0,0,1), seasonal = c(0,1,1))) #lo compongio a mano
print(summary(ARIMA))
autoplot(ARIMA)
checkresiduals(ARIMA)

fitARIMA17<-ARIMA$fitted
plot(dati17,main="Training Sample e Fitted values", xlab="Anno", ylab="Immatr.")
lines(fitARIMA17, col="red")

erroriARIMA17<-dati17-fitARIMA17 # y - ycappuccio
mis_ARIMA17<-Misure(dati17,erroriARIMA17) #i risultati coincidono con quelli del summary


#calcolo manuale su test sample di ME MSE MAE MAPE
prevARIMA22<- forecast(ARIMA, h=n-m)
plot(forecast(object = ARIMA, h=n-m),
     main = "Previsione immatricolazioni con ARIMA",
     xlab= "Anno", ylab= "Imm.",  ylim=c(0,300000))

fitARIMA22<-prevARIMA22$mean
plot(dati22,main="Test Sample e Fitted values", xlab="Anno", ylab="Immatr.")
lines(fitARIMA22, col="red")

erroriARIMA22<-dati22-fitARIMA22 # y - Forecast
mis_ARIMA22<-Misure(dati22,erroriARIMA22)


#dopo un'analisi dei differenti modelli il migliore utilizzabile dovrebbe
#essere il modello ARIMA(3,0,0)(0,1,2)[12], il modello ottenuto dal
#metodo classico è comunque un modello abbastanza buono ai fini previsivi

