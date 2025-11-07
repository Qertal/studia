#### Analiza szeregow czasowych (M.S.) - laboratorium nr 5 (29 X 2025)
####  Tematy: 
####  1) Wygladzanie za pomoca metody Holta-Wintersa
####  2) Konstrukcja prognoz punktowych i przedzialowych; kryteria jakosci prognoz
####  3) Metoda dekompozycji STL (loess) i prognozowanie
####  Dane zrodlowe:  https://fred.stlouisfed.org/series/NGDPRNSAXDCPLQ 


####  1. Wczytanie i wstepna wizualizacja danych zrodlowych

library(tseries)
library(forecast)

dane <- read.csv("PLGDPQ.csv",header=TRUE,dec=".")

head(dane)
colnames(dane) <- c("Data","PKB")

# dane rejestrowane co kwartal
Xt <- dane$PKB

# konwersja do obiektu typu ts()
Xts <- ts(Xt,start=c(2003,1),frequency=4) 

plot(Xts,type="l",lwd=2,col="blue",xlab="Time",ylab="X(t)",main="Dane empiryczne: realny PKB Polski [mln USD]",las=1)
grid()

# retrospekcja:  dwukrotne roznicowanie (przyrosty, rozn. sezonowe)
Xts_2 <- diff(diff(Xts,lag=4))
plot(Xts_2,type="l",lwd=2,col="blue",xlab="Time",ylab="DD_12{X(t)}",main="Szereg po dwoch transformacjach",las=1)
grid()

Acf(Xts_2,lag.max=20,lwd=2,col="brown",main="Sample ACF dla DD_12{X(t)}")


####  2. Addytywna metoda Holta-Wintersa i prognozy ex-ante

# 3-letni horyzont prognozy
H <- 12

## a) wywolanie funkcji HoltWinters() w wersji podstawowej, model addytywny
HWadd <- HoltWinters(Xts,seasonal="additive")

# przykladowe reczne ustawienie parametrow wygladzenia [Hyndman: beta < alpha]
#HWadd <- HoltWinters(Xts,seasonal="additive",alpha=0.3,beta=0.2,gamma=0.25)

# parametry wygladzenia i skladniki trend/sezon
c(HWadd$alpha,HWadd$beta,HWadd$gamma)

HWadd$coefficients

## b) prognozy punktowe i przedzialowe na H okresow
HWaddprogn <- predict(HWadd,H,prediction.interval = TRUE,level=0.95)

# podglad prognoz ex-ante
HWaddprogn

# wizualizacja prognoz
plot(HWadd,HWaddprogn,lwd=2,col="blue",xlab="t",ylab="X(t)",main="Addytywny model Holta-Wintersa i prognozy PKB w Polsce",las=1)
grid()


#### 2'. HW multiplikatywny
## HWmult <- HoltWinters(Xts,seasonal="multiplicative") ....



#### 3. Addytywna metoda Holta-Wintersa: prognozowanie ex-post


## a) podzial trajektorii {X(t)} na zbior treningowy i testowy
N <- length(Xts)

# zbior treningowy
Xlearn <- window(Xts,start=c(2003,1),end=c(2019,4))

N1 <- length(Xlearn)
H <- N-N1
N1/N

# zbior testowy
Xtest <- window(Xts,start=c(2020,1))

## b) wygladzenie i prognozowanie metoda Holta-Wintersa + ustawienia startowe dla parametrow wygladzenia


HWadd2 <- HoltWinters(Xlearn,seasonal="additive",optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1))

# prognozy punktowe i przedzialowe ex-post
HWadd2progn <- predict(HWadd2,H,prediction.interval = TRUE,level=0.95)

# wizualizacja prognoz punktowych i przedzialowych ex-post
plot(HWadd2,HWadd2progn,lwd=2,col="blue",xlab="t",ylab="X(t)",main="Addytywny model Holta-Wintersa i prognozy PKB w Polsce",las=1)
grid()

# podglad zbioru testowego i prognoz punktowych ex-post
cbind(Xtest,HWadd2progn[,1])

# wizualizacja: zbior testowy vs. prognozy ex-post
y_ <- min(c(HWadd2progn[,1],Xtest))
Y_ <- max(c(HWadd2progn[,1],Xtest))

plot(HWadd2progn[,1],lwd=2,ylim=c(y_,Y_),xlab="t",ylab="X(t), ^X(t)",main="Prognozy ex-post i zbior testowy: model Holta-Wintersa",las=1)
par(new=TRUE)
plot(Xtest,col="blue",lwd=2,ylim=c(y_,Y_),xlab="t",ylab="X(t), ^X(t)",main="Prognozy ex-post i zbior testowy: model Holta-Wintersa",las=1)
grid()
legend("topleft",c("Prognozy ex-post","Zbior testowy"),col=c("black","blue"),lwd=c(2,2))


## c) kryteria jakosci prognoz ex-post na zbiorze testowym: obliczenia reczne

RMSE <- sqrt(mean((Xtest-HWadd2progn[,1])^2))
MAE  <- mean(abs(Xtest-HWadd2progn[,1])) 
MAPE <- mean(abs((Xtest-HWadd2progn[,1])/Xtest))*100   #MAPE w %

c(RMSE,MAE,MAPE)


## d) konstrukcja prognoz ex-post i obliczanie ich jakosci: funkcje forecast() i accuracy()

# prognozy na czasokres zbioru testowego
progn_fct <- forecast(Xlearn,h=H)

# kryteria jakosci prognoz (na zbiorze testowym!)
accuracy(progn_fct)

# skalowanie osi i wizualizacja {X(t)} oraz prognoz ex-post
y_ <- min(c(progn_fct$mean,Xtest))
Y_ <- max(c(progn_fct$mean,Xtest))

plot(progn_fct$mean,lwd=2,ylim=c(y_,Y_),xlab="t",ylab="X(t), ^X(t)",main="Prognozy ex-post i zbior testowy: funkcja forecast() ",las=1)
par(new=TRUE)
plot(Xtest,col="blue",lwd=2,ylim=c(y_,Y_),xlab="t",ylab="X(t), ^X(t)",main="Prognozy ex-post i zbior testowy: funkcja forecast() ",las=1)
grid()
legend("topleft",c("Prognozy ex-post","Zbior testowy"),col=c("black","blue"),lwd=c(2,2))


# dodatkowe funkcjonalnosci procedury forecast()
progn_fct2 <- forecast(Xlearn,h=H,fan=TRUE)

plot(progn_fct2,xlab="t",ylab="X(t)",main="Prognozy punktowe i przedzialowe ex-post dla PKB Polski",las=1)
grid()

plot(progn_fct2,xlab="t",ylab="X(t)")
grid()
# pomoc dla funkcji forecast
?forecast.ts


#### 4. Dekompozycja STL (seasonal, trend, Loess) i prognozowanie

?stl 

# metoda STL
STLobject <- stl(Xts,s.window="periodic",robust=TRUE)
plot(STLobject)

# prognozowanie "z automatu"
forecast(STLobject,h=12)

plot(forecast(STLobject,h=12))
grid()

# alternatywne wywolanie
# STLobject2 <- stl(Xts,s.window=7)


### 5. ETS - pelna rodzina wygladzen wykladniczych [Hyndman otexts.com] - "zajawka"

?ets
# linkownia:  https://otexts.com/fpp2/ets.html

# wygladzenie ETS - wywolanie podstawowe
etsfit <- ets(Xts)
plot(etsfit)

# prognozowanie
etsforecast <- forecast.ets(etsfit,h=12)
plot(etsforecast)


#################